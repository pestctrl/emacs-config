;;; my-comp-minor-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:17]

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
(require 'anaphora)
(require 'llvm-buffer-chain)

(define-minor-mode compilation-minor-mode
  "Toggle Compilation minor mode.

When Compilation minor mode is enabled, all the error-parsing
commands of Compilation major mode are available.  See
`compilation-mode'."
  :lighter " Compilation" :keymap compilation-mode-map
  (if (not compilation-minor-mode)
      (compilation--unsetup)
    (compilation-setup t)
    (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
    (font-lock-flush)))

(defvar-local compilation-finish-local-transient nil)
(defvar-local compilation-finish-local-sticky nil)

(add-hook 'compilation-finish-functions
          'compilation-run-local-hooks)

(defun compilation-run-local-hooks (buf msg)
  (with-current-buffer buf
    (run-hook-with-args 'compilation-finish-local-transient buf msg)
    (setq compilation-finish-local-transient nil)
    (run-hook-with-args 'compilation-finish-local-sticky buf msg)))

(setq shell-file-name "bash")
(let ((aliases-file "~/.config/bash/bash-env.sh"))
  (when (file-exists-p aliases-file)
    (setenv "BASH_ENV" aliases-file)))

(defun my/enable-comp-keys-if-separate-mode (orig &rest args)
  (let ((ht compilation-finish-local-transient)
        (hs compilation-finish-local-sticky)
        (lbc-abi lbc/act-buffer-info))
    (-->
     (car args)
     (replace-regexp-in-string
      (rx (+ space) "\\" (* space) "\n" (* space)) " "
      it)
     (concat it "\n")
     (when (not (zerop (shell-command (format "grep -Fxq '%s' ~/.bash_history"
                                              it))))
       (write-region it nil "~/.bash_history" 'append)))
    (aprog1 (save-window-excursion (apply orig args))
      (with-current-buffer it
        (when-let (mode (cadr args))
          (unless (member mode '(grep-mode))
            (set-process-filter (get-buffer-process it) nil))
          (compilation-minor-mode t)
          (setq compilation-finish-local-sticky hs)
          (setq compilation-finish-local-transient ht)
          (setq lbc/act-buffer-info lbc-abi)))
      (sit-for 0.05)
      ;; Why doesn't this pop up in a side-window?
      (with-selected-window (display-buffer it)
        (when (eq major-mode 'compilation-mode)
          (goto-char (point-max)))))))

(advice-add #'compilation-start
            :around
            #'my/enable-comp-keys-if-separate-mode)

(defun my/recompile-save-windows (fun &rest args)
  (display-buffer-same-window
   (save-window-excursion
     (apply fun args)
     (current-buffer))
   nil))

(advice-add #'recompile
            :around
            #'my/recompile-save-windows)

(define-key compilation-mode-map (kbd "m") #'compilation-set-mode)

(defun compilation-set-mode ()
  (interactive)
  (when (not (or (eq major-mode 'compilation-mode)
                 compilation-minor-mode))
    (user-error "Yo, switch to a compilation-buffer"))

  (let ((mode
         (intern (completing-read
                  "Major mode? "
                  (let (l)
                    (mapatoms
                     (lambda (x)
                       (and (or (eq x 'fundamental-mode)
                                (eq x 'compilation-mode)
                                (get x 'derived-mode-parent))
                            (push x l))))
                    l)))))
    (setf (cadr compilation-arguments)
          mode
          (caddr compilation-arguments)
          `(lambda (_)
             ,(buffer-name (current-buffer))))
    (let ((args compilation-arguments))
      (call-interactively mode)
      (when (not (eq mode 'compilation-mode))
        (compilation-minor-mode t))
      (setq compilation-arguments args))))

(defvar compile-command-joiner-fun
  #'default-compile-command-joiner)

(defun default-compile-command-joiner (l)
  (string-join l " && \\\n"))

(defmacro def-compile-command (sym args extra-args &rest body)
  (declare (indent defun))
  (mmt-with-gensyms (result)
    `(defun ,sym ,args
       ,(when-let (int (plist-get extra-args :interactive))
          `(interactive ,@int))
       (let ((,result
              (funcall compile-command-joiner-fun
                       (progn
                         ,@body))))
         (if (not (called-interactively-p))
             ,result
           (compilation-start
            ,result
            ,@(plist-get extra-args :compilation-args)))))))

(provide 'my-comp-minor-mode)
;;; my-comp-minor-mode.el ends here
