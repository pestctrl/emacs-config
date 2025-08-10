;;; my-org-edit-special.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-08-10 13:28]

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

(defun my/org-edit-src-exit ()
  (interactive)
  (save-window-excursion
    (call-interactively #'org-edit-src-exit)))

(defvar-keymap org-capture-self-chat-keymap
  :parent org-src-mode-map
  "C-c C-c" #'my/org-edit-src-exit
  "C-c C-'" #'my/org-edit-src-exit
  "C-c C-k" #'(lambda ()
                (interactive)
                (call-interactively #'org-edit-src-abort)
                (call-interactively #'org-cut-special)
                (call-interactively #'delete-window)))

(defun my/org-edit-special ()
  (interactive)
  (call-interactively #'org-edit-special)
  (setq-local minor-mode-overriding-map-alist
              `((org-src-mode . ,org-capture-self-chat-keymap))))

(provide 'my-org-edit-special)
;;; my-org-edit-special.el ends here
