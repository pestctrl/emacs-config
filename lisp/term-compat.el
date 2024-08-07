;;; term-compat.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-05-30 16:59]

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

(global-set-key (kbd "M-[ emacs-C-SPC") #'set-mark-command)
(global-set-key (kbd "M-[ emacs-M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-[ emacs-C-/") #'undo)
(global-set-key (kbd "M-[ emacs-C-<backspace>") #'backward-kill-word)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c M-[ emacs-C-,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "M-[ emacs-C-<return>") #'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "M-[ emacs-M-S-<return>") #'org-insert-todo-heading)
  (define-key org-mode-map (kbd "M-[ emacs-C-S-<return>") #'org-insert-todo-heading-respect-content))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-[ emacs-C-<backspace>")
              #'(lambda () (interactive) (vterm-send-key "w" nil nil t))))

(provide 'term-compat)
;;; term-compat.el ends here
