;;; vterm-aux.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-05-14 14:08]

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
(require 'vterm)

(defun vterm-backup-path ()
  (interactive)
  (while
      (progn
        (re-search-backward (rx "/" (+ (or (not space) "\\ "))))
        (not (y-or-n-p "Did you want to kill this path? "))))
  (match-string 0))

(defun vterm-kill-save (thing)
  (interactive
   (list
    (read-key "Kill what? [c]urrent working directory | [p]ath")))
  (awhen
      (pcase thing
        ('?c default-directory)
        ('?p (vterm-backup-path)))
    (kill-new it)))

(define-key vterm-mode-map (kbd "M-w") #'vterm-kill-save)

(provide 'vterm-aux)
;;; vterm-aux.el ends here
