;;; org-archive-tree-count.el ---  -*- lexical-binding: t -*-

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

(defun my/org-count-subtree-characters ()
  (interactive)
  (save-window-excursion
    (org-agenda-goto t)
    (org-mark-subtree)
    (message (format "This subtree has %d characters. " (- (region-end) (region-beginning))))))

(define-key org-agenda-mode-map (kbd "C") #'my/org-count-subtree-characters)

(provide 'org-archive-tree-count)
;;; org-archive-tree-count.el ends here
