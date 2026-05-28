;;; org-ql-custom-stuck-projects.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-17 19:56]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'org-project)
(require 'org-ql)
(require 'org-ql-search)
(require 'my-org-priority-filter)

;;; Helper predicates

(defun my/top-level (&rest _args)
  "Return t if current heading is top-level (no TODO ancestors with sufficient priority)."
  (not (save-excursion
         (catch 'break
           (while (org-up-heading-safe)
             (when (and (>= (my/org-get-priority-with-inheritance)
                            my/org-priority-filter-level)
                        (org-get-todo-state))
               (throw 'break t)))
           nil))))

(defconst my/is-project-short-circuit
  '((not (tags "short"))
    (todo "TODO" "TASK" "ONE" "META" "META1" "EMPTY" "SEQ")
    (my/top-level)))

;;; Restriction handling

(defmacro my/with-agenda-restriction (from-var narrow-var old-beg-var old-end-var &rest body)
  "Execute BODY with agenda restriction set up.
Binds FROM-VAR to the restriction source, NARROW-VAR to whether narrowing is needed,
and OLD-BEG-VAR/OLD-END-VAR to original buffer bounds."
  (declare (indent 4))
  `(let (,narrow-var ,old-beg-var ,old-end-var)
     (let ((,from-var (pcase org-agenda-overriding-restriction
                        ('nil (org-agenda-files nil 'ifmode))
                        ('file (get 'org-agenda-files 'org-restrict))
                        ('subtree (prog1 org-agenda-restrict
                                    (with-current-buffer org-agenda-restrict
                                      (setf ,old-beg-var (point-min)
                                            ,old-end-var (point-max)
                                            ,narrow-var t)
                                      (narrow-to-region org-agenda-restrict-begin
                                                        org-agenda-restrict-end)))))))
       (unless ,from-var
         (setf ,from-var (org-agenda-files nil 'ifmode)))
       (unwind-protect
           (progn ,@body)
         (when ,narrow-var
           (with-current-buffer ,from-var
             (narrow-to-region ,old-beg-var ,old-end-var)))))))

;;; Agenda insertion helpers

(defun my/insert-org-ql-agenda-block (items header)
  "Insert formatted agenda block with ITEMS under HEADER."
  (org-agenda-prepare)
  (insert (org-add-props header
              nil 'face 'org-agenda-structure 'org-agenda-type 'search) "\n")
  (let ((org-agenda-sorting-strategy-selected '(category-keep)))
    (->> items
         (-map #'org-ql-view--format-element)
         org-agenda-finalize-entries
         insert))
  (insert "\n"))

;;; Stuck projects

(defun my/get-project-stuck-displayables (element)
  "Get list of stuck project displayables for ELEMENT and its children.
Returns a list starting with ELEMENT followed by relevant children."
  (let ((marker (org-element-property :org-marker element)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (if (member (org-get-todo-state) '("EMPTY"))
          (list element)
        (cons element (my/collect-stuck-children))))))

(defun my/collect-stuck-children ()
  "Collect stuck children of current heading, respecting priority filter."
  (let (display nothing)
    (olc/todo-children
      (when (>= (my/org-get-priority-with-inheritance)
                my/org-priority-filter-level)
        (pcase (opr/get-type)
          ('project
           (pcase (opr/type-of-project)
             ('stuck
              (let ((children (-> (point)
                                  (org-element-headline-parser)
                                  (org-ql--add-markers)
                                  (my/get-project-stuck-displayables)
                                  (reverse))))
                (setf display (append children display))))
             ('hold
              (push (org-ql--add-markers (org-element-headline-parser (point)))
                    nothing))))
          ('task
           (cond
            ((eq 'stuck (opr/type-of-task))
             (push (org-ql--add-markers (org-element-headline-parser (point)))
                   display))
            ((member (org-get-todo-state) '("FUTURE" "BACKLOG"))
             (push (org-ql--add-markers (org-element-headline-parser (point)))
                   nothing)))))))
    (reverse (or display nothing))))

(defun my/org-ql-stuck-projects (tag)
  "Display stuck projects agenda block filtered by TAG."
  (my/with-agenda-restriction from narrow-p old-beg old-end
    (let* ((org-todo-keywords-1 '("EMPTY" "ONE" "META" "META1" "TODO" "TASK" "SEQ"))
           (query `(and ,@(when (and tag (not (zerop (length tag))))
                            `((tags ,tag)))
                        ,@my/is-project-short-circuit
                        (or (eq 'stuck (opr/type-of-task))
                            (eq 'stuck (opr/type-of-project)))
                        (or (my/org-part-of-current-priority-p)
                            (ol/any-descendents?
                              (my/org-part-of-current-priority-p)))))
           (items (mapcan #'my/get-project-stuck-displayables
                          (org-ql-select from query
                            :action 'element-with-markers
                            :narrow narrow-p
                            :sort 'todo))))
      (my/insert-org-ql-agenda-block items org-ql-block-header))))

;;; Active projects

(defun my/get-project-active-displayables (element &optional plus-task)
  "Get list of active project displayables for ELEMENT and its children.
If PLUS-TASK is non-nil, also include active tasks."
  (let ((marker (org-element-property :org-marker element)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (if (member (org-get-todo-state) '("EMPTY"))
          (list element)
        (cons element (my/collect-active-children plus-task))))))

(defun my/collect-active-children (plus-task)
  "Collect active children of current heading.
If PLUS-TASK is non-nil, also include active tasks."
  (let (display)
    (olc/todo-children
      (when (or (and (eq 'task (opr/get-type))
                     (or (and plus-task (eq 'active (opr/type-of-task)))
                         (string= "WAIT" (org-get-todo-state))))
                (and (eq 'project (opr/get-type))
                     (eq 'active (opr/type-of-project 'active))))
        (let ((children (--> (point)
                             (org-element-headline-parser it)
                             (org-ql--add-markers it)
                             (my/get-project-active-displayables it plus-task)
                             (reverse it))))
          (setf display (append children display)))))
    (reverse display)))

(defun my/get-project-active-displayables-plus-tasks (element)
  "Get active project displayables for ELEMENT including tasks."
  (my/get-project-active-displayables element t))

(defun my/org-ql-active-projects-plus-tasks (tag)
  "Display active projects agenda block (with tasks) filtered by TAG."
  (my/org-ql-active-projects-impl tag t))

(defun my/org-ql-active-projects (tag)
  "Display active projects agenda block filtered by TAG."
  (my/org-ql-active-projects-impl tag nil))

(defun my/org-ql-active-projects-impl (tag plus-task)
  "Implementation of active projects display.
TAG filters projects, PLUS-TASK determines if tasks are included."
  (my/with-agenda-restriction from narrow-p old-beg old-end
    (let* ((org-todo-keywords-1 '("EMPTY" "META" "META1" "ONE" "TODO"))
           (query `(and ,@(when (and tag (not (zerop (length tag))))
                            `((tags ,tag)))
                        ,@(unless plus-task '((not (done))))
                        ,@my/is-project-short-circuit
                        (eq 'active (opr/type-of-project 'active))
                        (or (my/org-part-of-current-priority-p)
                            (ol/any-descendents?
                              (my/org-part-of-current-priority-p)))))
           (displayable-fn (if plus-task
                               #'my/get-project-active-displayables-plus-tasks
                             #'my/get-project-active-displayables))
           (items (mapcan displayable-fn
                          (org-ql-select from query
                            :action 'element-with-markers
                            :narrow narrow-p
                            :sort 'todo))))
      (my/insert-org-ql-agenda-block items org-ql-block-header))))

;;; Interactive testing

(defun my/testing-get-displayables ()
  "Interactively test stuck project displayables at point."
  (interactive)
  (my/get-project-stuck-displayables
   (org-ql--add-markers
    (org-element-headline-parser (point)))))

(provide 'org-ql-custom-stuck-projects)
;;; org-ql-custom-stuck-projects.el ends here
