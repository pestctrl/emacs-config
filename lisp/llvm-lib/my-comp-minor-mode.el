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

(defun my/enable-comp-keys-if-separate-mode (orig &rest args)
  (let ((ht compilation-finish-local-transient)
        (hs compilation-finish-local-sticky))
    (-->
     (car args)
     (replace-regexp-in-string
      (rx (+ space) "\\" (* space) "\n" (* space)) " "
      it)
     (concat it "\n")
     (write-region it nil "~/.bash_history" 'append))
    (aprog1 (apply orig args)
      (awhen (get-buffer-window it)
        (delete-window it))
      (with-current-buffer it
        (when (cadr args)
          (compilation-minor-mode t)
          (setq compilation-finish-local-sticky hs)
          (setq compilation-finish-local-transient ht)))
      ;; For some reason, when calling display-buffer, window doesn't get sent
      ;; to the side. This is because in my/is-compilation-buffer,
      ;; compilation-minor-mode is nil. No idea why, just manually call
      ;; display-buffer-in-side-window.

      ;; For some reason, calling display-window-in-side-window directly
      ;; doesn't work EITHER. The window doesn't pop up UNLESS there already
      ;; exists a side-window.

      ;; This only happens if we have (save-window-excursion (apply orig args))
      ;; instead of delete-window.
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

(provide 'my-comp-minor-mode)
;;; my-comp-minor-mode.el ends here
