;;; org-block-edna.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2021-04-28 18:10]

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
(use-package org-edna)
(org-edna-mode t)

(defun org-blocked-mark-blocking-tasks (change-plist)
  (when (and (string= "BLOCKED"
                      (plist-get change-plist :to))
             (not (string= "BLOCKED"
                           (plist-get change-plist :from)))
             (y-or-n-p "Mark blocking task? "))
    (let* ((id-origin (org-id-get-create))
           (id (org-id-get-with-outline-path-completion '((nil :maxlevel . 9))))
           (edna-string (format "ids(\"%s\") todo!(%s)"
                                id-origin
                                (plist-get change-plist :from))))
      (save-excursion
        (org-id-goto id)
        (let ((old-edna (or (org-entry-get (point) "TRIGGER") "")))
          (unless (string-match-p id-origin old-edna)
            (org-entry-put (point) "TRIGGER"
                           (concat old-edna " " edna-string))))))))

(add-hook 'org-trigger-hook
          #'org-blocked-mark-blocking-tasks)

;; (defun define-blocking-keyword (keyword)
;;   (add-to-list 'org-todo-state-tags-triggers
;;                `(,keyword ("blocked" . t)))
;;   (add-to-list 'org-todo-state-tags-triggers
;;                '(todo ("blocked"))))

(defun org-check-edit-inact (_ignore)
  (when (and (org-inside-heading-p)
             (string= "BLOCKED" (org-get-todo-state)))
    (user-error "Editing inactive task! Could be breaking some org-edna dependencies!")))

(defun org-inside-heading-p ()
  (when (save-excursion
          (beginning-of-line)
          (looking-at (format org-heading-keyword-regexp-format
                              (regexp-opt org-todo-keywords-1))))
    (>= (current-column)
       (+ (org-outline-level) 1 (length (org-get-todo-state)) 1))))

(advice-add #'org-check-before-invisible-edit
            :before
            #'org-check-edit-inact)

(provide 'org-block-edna)
;;; org-block-edna.el ends here
