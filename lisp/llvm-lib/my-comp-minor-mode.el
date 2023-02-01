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
    (aprog1 (apply orig args)
      (with-current-buffer it
        (when (cadr args)
          (compilation-minor-mode)
          (setq compilation-finish-local-sticky hs)
          (setq compilation-finish-local-transient ht))))))

(advice-add #'compilation-start
            :around
            #'my/enable-comp-keys-if-separate-mode)

(defun my/compilation-start-should-goto-end-of-buffer (_command &optional mode &rest _)
  (unless mode
    (goto-char (point-max))))

(advice-add #'compilation-start
            :after
            #'my/compilation-start-should-goto-end-of-buffer)

(defun my/recompile-save-windows (fun &rest args)
  (save-window-excursion
    (apply fun args)))

(advice-add #'recompile
            :around
            #'my/recompile-save-windows)

(provide 'my-comp-minor-mode)
;;; my-comp-minor-mode.el ends here
