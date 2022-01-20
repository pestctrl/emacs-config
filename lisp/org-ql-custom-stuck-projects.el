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

(defun my/top-level ()
  (save-excursion
    (catch 'break
      (while (org-up-heading-safe)
        (when (org-get-todo-state)
          (throw 'break nil)))
      t)))

(defun my/testing-get-displayables ()
  (interactive)
  (my/get-project-stuck-displayables
   (org-ql--add-markers
    (org-element-headline-parser
     (point)))))

(defconst my/is-project-short-circuit
  '((not (tags "short"))
    (todo "TODO" "TASK" "ONE" "META" "META1" "EMPTY" "SEQ")
    (my/top-level)))

(defun my/get-project-stuck-displayables (element)
  (let ((marker (org-element-property :org-marker element)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (let* ((state (org-get-todo-state)))
        (if (member state '("EMPTY"))
            (list element)
          (cons element
                (let ((display '()))
                  (olc/todo-children
                    (let ((type (opr/get-type)))
                      (pcase type
                        ('project
                         (when (eq 'stuck (opr/type-of-project))
                           (let ((res (-> (point)
                                          (org-element-headline-parser)
                                          (org-ql--add-markers)
                                          (my/get-project-stuck-displayables)
                                          (reverse))))
                             (setf display (append res display)))))
                        ('task
                         (when (member (opr/type-of-task) '(stuck wait))
                           (push (org-ql--add-markers (org-element-headline-parser (point)))
                                 display))))))
                  (reverse display))))))))

(defun my/org-ql-stuck-projects (tag)
  (let (narrow-p old-beg old-end)
    (when-let* ((from (pcase org-agenda-overriding-restriction
                        ('nil (org-agenda-files nil 'ifmode))
                        ('file (get 'org-agenda-files 'org-restrict))
                        ('subtree (prog1 org-agenda-restrict
                                    (with-current-buffer org-agenda-restrict
                                      ;; Narrow the buffer; remember to widen it later.
                                      (setf old-beg (point-min) old-end (point-max)
                                            narrow-p t)
                                      (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end)))))))
      (let* ((org-todo-keywords-1 '("EMPTY" "ONE" "META" "META1" "TODO" "TASK" "SEQ"))
             (items (mapcan #'my/get-project-stuck-displayables
                            (org-ql-select from
                              `(and ,@(when (and tag
                                                 (not (zerop (length tag))))
                                        `((tags ,tag)))
                                    (odl/part-of-current-level-p)
                                    ,@my/is-project-short-circuit
                                    (or (eq 'stuck (opr/type-of-task))
                                        (eq 'stuck (opr/type-of-project))))
                              :action 'element-with-markers
                              :narrow narrow-p
                              :sort 'todo))))
        (when narrow-p
          ;; Restore buffer's previous restrictions.
          (with-current-buffer from
            (narrow-to-region old-beg old-end)))
        (org-agenda-prepare)
        ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
        ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
        ;;  (org-agenda--insert-overriding-header (or org-ql-block-header (org-ql-agenda--header-line-format from query)))
        (insert (org-add-props org-ql-block-header 
                    nil 'face 'org-agenda-structure 'org-agenda-type 'search) "\n")
        ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
        ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
        ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
        (let ((org-agenda-sorting-strategy-selected '(category-keep) ))
          (->> items
               (-map #'org-ql-view--format-element)
               org-agenda-finalize-entries
               insert))
        (insert "\n")))))

;; (defun my/org-ql-stuck-projects (tag)
;;   (let* ((from (org-agenda-files nil 'ifmode))
;;          (org-todo-keywords-1 '("EMPTY" "ONE" "META" "META1" "TODO"))
;;          (items (org-ql-select org-agenda-files
;;                   `(and ,@(when (and tag
;;                                      (not (zerop (length tag))))
;;                             '((tags ,tag)))
;;                         (not (property "DELAYED"))
;;                         (or (descendants (eq 'stuck (opr/type-of-task)))
;;                             (eq 'stuck (opr/type-of-task))))
;;                   :action 'element-with-markers
;;                   :sort 'todo)))
;;     (org-agenda-prepare)
;;     ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
;;     ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
;;     ;;  (org-agenda--insert-overriding-header (or org-ql-block-header (org-ql-agenda--header-line-format from query)))
;;     (insert (org-add-props org-ql-block-header 
;;                 nil 'face 'org-agenda-structure 'org-agenda-type 'search) "\n")
;;     ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
;;     ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
;;     ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
;;     (let ((org-agenda-sorting-strategy-selected '(category-keep) ))
;;       (->> items
;;            (-map #'org-ql-view--format-element)
;;            org-agenda-finalize-entries
;;            insert))
;;     (insert "\n")))

(defun my/get-project-active-displayables (element)
  (let ((marker (org-element-property :org-marker element)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (if (member (org-get-todo-state) '("EMPTY"))
          (list element)
        (cons element
              (let ((display '()))
                (olc/todo-children
                  (let ((type (opr/get-type)))
                    (when (or (and (eq 'task (opr/get-type))
                                   (string= "WAIT" (org-get-todo-state)))
                           (and (eq 'project (opr/get-type))
                                   (eq 'active (opr/type-of-project 'active))))
                      (let ((res (-> (point)
                                     (org-element-headline-parser)
                                     (org-ql--add-markers)
                                     (my/get-project-active-displayables)
                                     (reverse))))
                        (setf display (append res display))))))
                (reverse display)))))))

(require 'org-dev-level)

(defun my/org-ql-active-projects (tag)
  (let (narrow-p old-beg old-end)
    (let* ((from (or (pcase org-agenda-overriding-restriction
                        ('nil (org-agenda-files nil 'ifmode))
                        ('file (get 'org-agenda-files 'org-restrict))
                        ('subtree (prog1 org-agenda-restrict
                                    (with-current-buffer org-agenda-restrict
                                      ;; Narrow the buffer; remember to widen it later.
                                      (setf old-beg (point-min) old-end (point-max)
                                            narrow-p t)
                                      (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end)))))
                     (org-agenda-files nil 'ifmode)))
           (org-todo-keywords-1 '("EMPTY" "META" "META1" "ONE" "TODO" ))
           (items (mapcan #'my/get-project-active-displayables
                          (org-ql-select from
                            `(and ,@(when (and tag
                                               (not (zerop (length tag))))
                                      `((tags ,tag)))
                                  (odl/part-of-current-level-p)
                                    ,@my/is-project-short-circuit
                                  (or (eq 'active (opr/type-of-task))
                                      (eq 'active (opr/type-of-project 'active))))
                            :action 'element-with-markers
                            :narrow narrow-p
                            :sort 'todo))))
      
      (when narrow-p
        ;; Restore buffer's previous restrictions.
        (with-current-buffer from
          (narrow-to-region old-beg old-end)))
      (org-agenda-prepare)
      ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
      ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
      ;;  (org-agenda--insert-overriding-header (or org-ql-block-header (org-ql-agenda--header-line-format from query)))
      (insert (org-add-props org-ql-block-header 
                  nil 'face 'org-agenda-structure 'org-agenda-type 'search) "\n")
      ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
      ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
      ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
      (let ((org-agenda-sorting-strategy-selected '(category-keep) ))
        (->> items
             (-map #'org-ql-view--format-element)
             org-agenda-finalize-entries
             insert))
      (insert "\n"))))

(defun my/org-ql-get-restriciton ())

;; (test-stuck-projects)

(provide 'org-ql-custom-stuck-projects)
;;; org-ql-custom-stuck-projects.el ends here
