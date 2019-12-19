;;; org-project-util.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-18 17:03]

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

(defvar opr/strict-projects '())

(defvar opr/strict-tasks '())

(defvar opr/ambiguous '())

(defvar opr/used-keys '())

(defconst keyword-type-index '(task ambiguous project util legacy))

(cl-defun define-todo-keyword (str type &key color key recordstr)
  (when key
    (when (member key opr/used-keys)
      (error (format "Can't define keyword \"%s\", key %c already used" str key)))
    (push key opr/used-keys))
  (let* ((keystr (if (and (not key) (not recordstr))
                     ""
                   (format "(%s%s)" (or (and key (char-to-string key)) "") (or recordstr ""))))
         (todo-keyword-str (concat str keystr))
         (index (cl-position type keyword-type-index)))
    (setf (nth index org-todo-keywords)
          (append (nth index org-todo-keywords)
                  (list todo-keyword-str))))
  (when color
    (add-to-list 'org-todo-keyword-faces
                 `(,str :foreground ,color :weight bold)))
  (pcase type
    ('ambiguous (add-to-list 'opr/ambiguous
                             str))
    ('project (add-to-list 'opr/strict-projects
                           str))
    ('task (add-to-list 'opr/strict-tasks
                        str))))

(defun finish-active-type (type)
  (let ((i (cl-position type keyword-type-index)))
    (setf (nth i org-todo-keywords)
          (append (nth i org-todo-keywords)
                  (list "|")))))

(defun opr/init ()
  (interactive)
  (setq org-todo-keywords '((sequence)
                            (sequence)
                            (sequence)
                            (sequence)
                            (sequence))
        org-todo-keyword-faces nil
        opr/used-keys nil
        opr/strict-projects '()
        opr/strict-tasks '()))

(provide 'opr-util)
;;; org-project-util.el ends here
