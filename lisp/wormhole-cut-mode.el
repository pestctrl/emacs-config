;;; wormhole-cut-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-16 14:27]

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

(defvar wormhole-cut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-w") #'wormhole-cut)
    (define-key map (kbd "C-c SPC") #'wormhole-reset-point)
    map))

(defvar wormhole-egress-marker nil)
(defvar wormhole-egress-overlay nil)

(defun wormhole-make-overlay ()
  (let ((overlay (make-overlay (point) (1+ (point)))))
    (overlay-put overlay 'face '(:background "green"))
    overlay))

(defun wormhole-reset-point ()
  (interactive)
  (move-marker wormhole-egress-marker (point))
  (move-overlay wormhole-egress-overlay (point) (1+ (point))))

(defun wormhole-cut ()
  (interactive)
  (kill-region (mark) (point) 'region)
  (save-excursion
    (goto-char (marker-position wormhole-egress-marker))
    (yank)
    (wormhole-reset-point)))

(define-minor-mode wormhole-cut-mode ""
  nil nil wormhole-cut-mode-map
  (if wormhole-cut-mode
      (setq wormhole-egress-marker (point-marker)
            wormhole-egress-overlay (wormhole-make-overlay))
    (setq wormhole-egress-marker nil)
    (delete-overlay wormhole-egress-overlay)))

(provide 'wormhole-cut-mode)
;;; wormhole-cut-mode.el ends here
