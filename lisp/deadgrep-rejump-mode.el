;;; deadgrep-rejump-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-01-25 09:39]

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
(require 'deadgrep)

(defun my/deadgrep-new-tab (orig &rest args)
  (if (not (y-or-n-p "Deadgrep rejump mode? "))
      (apply orig args)
    (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
      (unless (string-match-p "-deadgrep$" tab-name)
        (setq my/drj-tab (concat tab-name "-deadgrep"))
        (switch-or-create-tab (concat tab-name "-deadgrep"))))
    (when (window-parameter (selected-window) 'window-side)
      (window-toggle-side-windows))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (apply orig args)
    (deadgrep-rejump-mode 1)
    (setq my/drj-buffer (current-buffer))))

(defun deadgrep-fold-everything ()
  (interactive)
  (when deadgrep-rejump-mode
    (with-current-buffer my/drj-buffer
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (while (not (eobp))
              (progn
                (deadgrep--move-match t 'deadgrep-filename-face)
                (deadgrep-toggle-file-results)))
          (error nil))))))

(add-hook 'deadgrep-finished-hook
          #'deadgrep-fold-everything)

(advice-add #'deadgrep
            :around
            #'my/deadgrep-new-tab)

(defvar my/drj-buffer nil)
(defvar my/drj-tab nil)
;; (make-variable-buffer-local 'my/deadgrep-rejump-buffer)

(defun my/drj-jump (next-fun)
  (when-let ((win (get-buffer-window my/drj-buffer)))
    (let* ((curr-win (selected-window)))
      (select-window win)
      (funcall next-fun)
      (deadgrep--visit-result
       #'(lambda (f-name)
           ;; MUST HAPPEN BEFORE select-window, implicitly using
           ;; default-directory
           (let ((buffer (find-file-noselect f-name)))
             (select-window curr-win)
             (switch-to-buffer buffer))))
      (recenter))))

(defun my/drj-previous ()
  (interactive)
  (my/drj-jump #'deadgrep-backward-match))

(defun my/drj-next ()
  (interactive)
  (my/drj-jump #'deadgrep-forward-match))

(defvar deadgrep-rejump-mode-map nil)
(unless deadgrep-rejump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'my/drj-next)
    (define-key map (kbd "M-p") #'my/drj-previous)
    (define-key map (kbd "M-q") #'(lambda () (interactive) (deadgrep-rejump-mode -1)))
    (setq deadgrep-rejump-mode-map map)))

(define-minor-mode deadgrep-rejump-mode ""
  :global t
  :keymap deadgrep-rejump-mode-map
  (if deadgrep-rejump-mode
      (when my/drj-buffer
        (user-error "drj already in progress, can't start another"))
    (switch-or-create-tab my/drj-tab)
    (close-tab-switch)
    (setq my/drj-buffer nil
          my/drj-tab nil)))

(provide 'deadgrep-rejump-mode)
;;; deadgrep-rejump-mode.el ends here
