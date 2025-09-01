;;; erc-doom-modeline-hack.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-09-01 10:50]

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

(defun my/string-pixel-width (string)
  "Return the width of STRING in pixels.

If you call this function to measure pixel width of a string
with embedded newlines, it returns the width of the widest
substring that does not include newlines."
  (declare (important-return-value t))
  (if (zerop (length string))
      0
    ;; Keeping a work buffer around is more efficient than creating a
    ;; new temporary buffer.
    (let ((inhibit-read-only t))
      (with-current-buffer (get-buffer-create " *string-pixel-width*")
        ;; If `display-line-numbers' is enabled in internal buffers
        ;; (e.g. globally), it breaks width calculation (bug#59311)
        (setq-local display-line-numbers nil)
        (delete-region (point-min) (point-max))
        ;; Disable line-prefix and wrap-prefix, for the same reason.
        (setq line-prefix nil
	          wrap-prefix nil)
        (insert (propertize string 'line-prefix nil 'wrap-prefix nil))
        (car (buffer-text-pixel-size nil nil t))))))

(advice-add #'string-pixel-width
            :override
            #'my/string-pixel-width)

(provide 'erc-doom-modeline-hack)
;;; erc-doom-modeline-hack.el ends here
