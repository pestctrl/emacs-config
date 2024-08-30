;;; tmux-cmd.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-08-30 16:11]

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
(require 'tmux-send)

(defmacro defun-tmux-cmd (name args extra-args &rest body)
  (declare (indent 2))
  (mmt-with-gensyms (returned-list
                     tmux-window-name
                     string-commands
                     command)
    `(defun ,name ,args
       ,(when (member :interactive extra-args)
          (let ((int (plist-get extra-args :interactive)))
            `(interactive ,@int)))
       (let* ((,returned-list (list ,@body))
              (,tmux-window-name (car ,returned-list))
              (,string-commands (cdr ,returned-list))
              (,command (-->
                         ,string-commands
                         (flatten-list it)
                         (string-join it " && "))))
         (if (not (called-interactively-p))
             ,command
           (,(let ((tmux-type (plist-get extra-args :tmux-type)))
               (cond ((eq tmux-type 'sticky)
                      'ts/send-sticky-command)
                     ((eq tmux-type 'transient)
                      'ts/send-transient-command)
                     (t 'ts/send-sticky-command)))
            ,tmux-window-name ,command))))))

(defun replace-name-function-with-setq (name-symbol setq-symbol list)
  (cond
   ((not (consp list)) list)
   ((eq (car list) name-symbol)
    `(setq ,setq-symbol . ,(cdr list)))
   (t
    (let ((front (car list)))
      (cons (if (not (consp front))
                front
              (replace-name-function-with-setq
               name-symbol setq-symbol front))
            (mapcar #'(lambda (x)
                          (replace-name-function-with-setq
                           name-symbol setq-symbol x))
                    (cdr list)))))))

(defmacro defun-tmux-cmd-2 (name args extra-args &rest body)
  (declare (indent 2))
  (mmt-with-gensyms (tmux-window-name
                     command)
    `(defun ,name ,args
       ,(when (member :interactive extra-args)
          (let ((int (plist-get extra-args :interactive)))
            `(interactive ,@int)))
       (let* (,tmux-window-name
              (,command
               (progn
                 ,@(replace-name-function-with-setq
                    'name tmux-window-name body))))
         (if (not (called-interactively-p))
             ,command
           (,(let ((tmux-type (plist-get extra-args :tmux-type)))
               (cond ((eq tmux-type 'sticky)
                      'ts/send-sticky-command)
                     ((eq tmux-type 'transient)
                      'ts/send-transient-command)
                     (t 'ts/send-sticky-command)))
            ,tmux-window-name ,command))))))

(defmacro shell-and (&rest body)
  `(-->
    (list ,@body)
    ;; TODO: Should be strict about this, don't let this implicitly happen
    (flatten-list it)
    (string-join it " && ")))

(defmacro shell-or (&rest body)
  `(-->
    (list ,@body)
    ;; TODO: Should be strict about this, don't let this implicitly happen
    (flatten-list it)
    (string-join it " || ")))

(defmacro shell-then (&rest body)
  `(-->
    (list ,@body)
    ;; TODO: Should be strict about this, don't let this implicitly happen
    (flatten-list it)
    (string-join it " ; ")))

(defmacro shell-let* (let-clauses &rest body)
  (declare (indent 1))
  (let* ((front-clause (car let-clauses))
         (symbol-name (symbol-name (car front-clause)))
         (shell-name (string-replace "-" "_" (upcase symbol-name)))
         (shell-ref-name (format "${%s}" shell-name))
         (let-value (cadr front-clause)))
    (if (not front-clause)
        `(list ,@body)
      (when (or
             (not (consp let-value))
             (not (member (car let-value)
                          '(run set))))
        (user-error "Each let form should start with run or set"))
      `(let ((,(car front-clause) ,shell-ref-name))
         (shell-and
          ,(if (and (consp let-value)
                    (eq 'set (car let-value)))
               `(format "%s=\"%s\""
                        ,shell-name
                        ,(cadr let-value))
             `(format "%s=$(%s)"
                      ,shell-name
                      ,(cadr let-value)))
          (shell-let* ,(cdr let-clauses)
            ,@body))))))

(provide 'tmux-cmd)
;;; tmux-cmd.el ends here
