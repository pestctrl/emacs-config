;;; org-other-additions.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-26 18:48]

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
(defun my/org-agenda-narrow ()
  (interactive)
  (org-agenda-switch-to)
  (org-narrow-to-subtree)
  (outline-show-branches))

(define-key org-agenda-mode-map (kbd "S-<return>") 'my/org-agenda-narrow)

(defun org-check-before-killing-line (arg)
  (interactive "P")
  (if (string= "yes" (completing-read "Are you sure you want to use that keybinding? " '("yes" "no")))
      (kill-whole-line arg)
    (org-cut-subtree)))

(define-key org-mode-map (kbd "C-S-<backspace>") #'org-check-before-killing-line)

(provide 'org-other-additions)
;;; org-other-additions.el ends here
