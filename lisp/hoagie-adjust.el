;;; hoagie-adjust.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-09-03 14:45]

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

(defvar frame-font-size-cache
  (make-hash-table))

;; Font size adjustment
(defun my/dynamically-adjust-font-size (&optional frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
  If I let Windows handle DPI everything looks blurry."
  (interactive)
  ;; Using display names is unreliable...switched to checking the resolution
  (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
         (monitor-name (cdr (assoc 'name attrs)))
         (width-mm (second (assoc 'mm-size attrs)))
         (width-px (fourth (assoc 'geometry attrs)))
         (height-px (fifth (assoc 'geometry attrs)))
         (size 10)) ;; default for first screen at work
    (when (eq height-px 2880)
      (let ((f (selected-frame)))
        (set-face-attribute 'default f :height 130)
        (set-face-attribute 'mode-line f :height 130)
        ;; (set-face-attribute 'mode-line-inactive f :height 130)
        (setq doom-modeline-height 30))
      (exwm-randr-refresh))
    (when (eq height-px 1504)
      (let ((f (selected-frame)))
        (set-face-attribute 'default f :height 100)
        (set-face-attribute 'mode-line f :height 100)))
    ;; (unless (and (gethash frame frame-font-size-cache)
    ;;              (= size (gethash frame frame-font-size-cache)))
    ;;   (puthash frame size frame-font-size-cache))
    ))

(defun my/adjust-font-size (&optional arg)
  (interactive "P")
  (if (not arg)
      (my/dynamically-adjust-font-size)
    (let ((font-height (read-number "Font Height (%)? "))
          (f (selected-frame)))
      (set-face-attribute 'default f :height font-height)
      (set-face-attribute 'mode-line f :height font-height)
      (setq-default olivetti-body-width
                    (if (and (> font-height 120)
                               (boundp 'olivetti-body-width))
                        80
                      140)))))

;; (remove-hook 'window-size-change-functions #'hoagie-adjust-font-size)

(provide 'hoagie-adjust)
;;; hoagie-adjust.el ends here
