;;; org-loop-legacy.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-09 09:29]

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

(defmacro orgc-loop/children-cat (condition &rest body)
  (declare (indent defun))
  (let ((level-symbol (make-symbol "level")))
    `(let ((,condition nil)
           (,level-symbol (org-current-level)))
       (outline-next-heading)
       (when (< ,level-symbol (org-current-level))
         (while (progn
                  (while (string= (org-get-todo-state) "CAT")
                    (outline-next-heading))
                  ,@body
                  (and (not ,condition)
                       (or (org-get-next-sibling)
                           (and (not (eobp))
                                (< ,level-symbol (org-current-level)))))))
         ,condition))))

(defmacro orgc-loop/todo-children-cat (condition &rest body)
  (declare (indent defun))
  (let ((todo-state (make-symbol "todo-state"))
        (tags (make-symbol "tags")))
    `(orgc-loop/children-cat ,condition
       (let ((,todo-state (org-get-todo-state))
             (,tags (org-get-tags (point))))
         (when ,todo-state
           (if (member "ARCHIVE" ,tags)
               (org-end-of-subtree t)
             ,@body))))))

;; (defmacro traverse-org-headlines (headline &rest body)
;;   (declare (indent defun))
;;   (let ((buffer-symbol (make-symbol "buffer")))
;;     `(let (,buffer-symbol)
;;        (org-check-agenda-file ,(cadr headline))
;;        (setq ,buffer-symbol (if (file-exists-p ,(cadr headline))
;;                                 (org-get-agenda-file-buffer ,(cadr headline))
;;                               (error "No such file %s" ,(cadr headline))))
;;        (with-current-buffer ,buffer-symbol
;;          (while (and (not (eobp))
;;                      (outline-next-heading))
;;            ,@body)))))

;; (defmacro traverse-org-files (files &rest body)
;;   (declare (indent defun))
;;   (let ((file-symbol (make-symbol "file")))
;;     `(dolist (,file-symbol ,(cadr files))
;;        (traverse-org-headlines (,(car files) ,file-symbol)
;;          ,@body))))

(provide 'org-loop-legacy)
;;; org-loop-legacy.el ends here
