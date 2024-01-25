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
  (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
    (unless (string-match-p "-deadgrep$" tab-name)
      (switch-or-create-tab (concat tab-name "-deadgrep"))))
  (when (window-parameter (selected-window) 'window-side)
    (window-toggle-side-windows))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (apply orig args)
  (deadgrep-rejump-mode 1)
  (setq my/drj-buffer deadgrep-buffer))

(advice-add #'deadgrep
            :around
            #'my/deadgrep-new-tab)

(defvar my/drj-buffer nil)
;; (make-variable-buffer-local 'my/deadgrep-rejump-buffer)

(defun my/drj-previous ()
  (interactive)
  (when-let ((win (get-buffer-window my/drj-buffer)))
    (select-window win)
    (deadgrep-backward-match)
    (deadgrep-visit-result)))

(defun my/drj-next ()
  (interactive)
  (when-let ((win (get-buffer-window my/drj-buffer)))
    (select-window win)
    (deadgrep-forward-match)
    (deadgrep-visit-result)))

(defvar deadgrep-rejump-mode-map nil)
(unless deadgrep-rejump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'my/drj-next)
    (define-key map (kbd "M-p") #'my/drj-previous)
    (define-key map (kbd "q") #'(lambda () (interactive) (deadgrep-rejump-mode -1)))
    (setq deadgrep-rejump-mode-map map)))

(define-minor-mode deadgrep-rejump-mode ""
  :global t
  :keymap deadgrep-rejump-mode-map
  (unless deadgrep-rejump-mode
    (close-tab-switch)))

(defun my/drj-setup (orig &rest args)
  (delete-other-windows)
  (recenter))

(advice-add #'deadgrep--visit-result
            :after
            #'my/drj-setup)

(provide 'deadgrep-rejump-mode)
;;; deadgrep-rejump-mode.el ends here
