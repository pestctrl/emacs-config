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

(defun my/get-project-stuck-displayables (element)
  (let ((marker (org-element-property :org-marker element)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (if (member (org-get-todo-state) '("EMPTY"))
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
                          (unless (zerop (1- (length res)))
                            (setf display (append res display))))))
                     ('task
                      (when (eq 'stuck (opr/type-of-task))
                        (push (org-ql--add-markers (org-element-headline-parser (point)))
                              display))))))
                (reverse display)))))))

(defun my/org-ql-stuck-projects (&rest args)
  (let ((from (org-agenda-files nil 'ifmode))
        (items (mapcan #'my/get-project-stuck-displayables
                              (org-ql-select org-agenda-files
                                '(and (tags "dev")
                                      (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                                      (my/top-level)
                                      (or (eq 'stuck (opr/type-of-task))
                                          (eq 'stuck (opr/type-of-project))))
                                :action 'element-with-markers
                                :sort 'todo))))
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
    (insert "\n")))

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
                   (when (and (eq 'project (opr/get-type))
                              (eq 'active (opr/type-of-project 'active)))
                     (let ((res (-> (point)
                                    (org-element-headline-parser)
                                    (org-ql--add-markers)
                                    (my/get-project-active-displayables)
                                    (reverse))))
                       (setf display (append res display))))))
                (reverse display)))))))

(defun my/org-ql-active-projects (&rest args)
  (let ((from (org-agenda-files nil 'ifmode))
        (items (mapcan #'my/get-project-active-displayables
                       (org-ql-select org-agenda-files
                         '(and (tags "dev")
                               (not (tags "short"))
                               (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                               (my/top-level)
                               (or (eq 'active (opr/type-of-task))
                                   (eq 'active (opr/type-of-project 'active))))
                         :action 'element-with-markers
                         :sort 'todo
                         ))))
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
    (insert "\n")))

;; (test-stuck-projects)

(provide 'org-ql-custom-stuck-projects)
;;; org-ql-custom-stuck-projects.el ends here
