;;; emacs-custom-load-or-ask.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-14 16:33]

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
(require 'mmt)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defvar ec/my-variables-list '())

(defvar ec/prompt-functions (make-hash-table))

(defmacro defun-prompt (name type args &rest body)
  (declare (indent 3))
  (when (not (gethash name ec/prompt-functions)) ;;(not (fboundp name))
    (puthash type name ec/prompt-functions))
  `(defun ,name ,(append args '(&optional override))
     (setq ec/my-variables-list
           (delq (list ',type ,@args)
                 ec/my-variables-list))
     (add-to-list 'ec/my-variables-list
                  (list ',type ,@args))
     (when (or override (not (boundp ,(car args))))
       ,@body)))

(defun-prompt ec/load-or-ask-pred predicate (sym prompt)
  (customize-save-variable sym (y-or-n-p prompt)))

(defun-prompt ec/load-or-ask-key key (key key-key prompt)
  (let ((keygen (read-key prompt)))
      (customize-save-variable key-key keygen)
      (customize-save-variable key (char-to-string keygen))))

(defun ec/rerun-prompt (prompt-arglist)
  (apply (symbol-function (gethash (car prompt-arglist) ec/prompt-functions))
         (append (cdr prompt-arglist)
                 (list t))))

(defun ec/rerun-all-prompts ()
  (interactive)
  (dolist (prompt ec/my-variables-list)
    (ec/rerun-prompt prompt)))

(defun ec/rerun-one-prompt ()
  (interactive)
  (let ((resp (completing-read "Which prompt? "
                               (mapcar #'(lambda (a)
                                           (last a))
                                       ec/my-variables-list))))
    (loop for i in ec/my-variables-list
          until (member resp i)
          finally
          do
          (ec/rerun-prompt i))))

(provide 'emacs-custom-load-or-ask)
;;; emacs-custom-load-or-ask.el ends here
