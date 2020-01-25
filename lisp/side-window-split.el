;;; side-window-split.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 13:36]

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

(defun side-window-op (side &optional size buffer)
  (let ((prev-win (selected-window))
        (win (display-buffer-in-side-window
              (or buffer (current-buffer))
              `((side . ,side)))))
    (select-window win)
    (set-window-dedicated-p win t)
    (set-window-parameter win 'no-delete-other-windows t)
    (when size
      (window-resize win (- size (window-pixel-width)) t nil t))
    (select-window prev-win)))

(defun side-left-window ()
  (interactive)
  (side-window-op 'left))

(defun side-right-window ()
  (interactive)
  (side-window-op 'right))

(defun side-bottom-window ()
  (interactive)
  (side-window-op 'bottom))

(setq window-sides-vertical t)

(provide 'side-window-split)
;;; side-window-split.el ends here
