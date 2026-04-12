;;; lsp-frame-hack.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-07-12 14:11]

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

(advice-add #'lsp-ui-doc--move-frame
            :override
            #'my/lsp-ui-doc--move-frame)

(defun my/lsp-ui-doc--move-frame (frame)
  "Place our FRAME on screen."
  (-let* (((left top right _bottom) (window-edges nil t nil t))
          (window (frame-root-window frame))
          (char-h (frame-char-height frame))
          (char-w (frame-char-width frame))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
          (width (+ width (* char-w 4))) ;; margins
          (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
          (width (min width (* lsp-ui-doc-max-width char-w)))
          (frame-right (pcase lsp-ui-doc-alignment
                         ('frame (frame-pixel-width))
                         ('window right)))
          ((left . top) (if (eq lsp-ui-doc-position 'at-point)
                            (lsp-ui-doc--mv-at-point width height left top)
                          (cons (max (- frame-right width char-w) 10)
                                (pcase lsp-ui-doc-position
                                  ('top (+ top char-w))
                                  ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                                              height
                                              10))))))
          (frame-resize-pixelwise t)
          (move-frame-functions nil)
          (window-size-change-functions nil)
          (window-state-change-hook nil)
          (window-state-change-functions nil)
          (window-configuration-change-hook nil)
          (inhibit-redisplay t))
    ;; Dirty way to fix unused variable in emacs 26
    (and window-state-change-functions
         window-state-change-hook)
    ;; Make frame invisible before moving/resizing it to avoid flickering:
    ;; We set the position and size in 1 call, modify-frame-parameters, but
    ;; internally emacs makes 2 different calls, which can be visible
    ;; to the user
    (and (frame-visible-p frame)
         (lsp-ui-doc--size-and-pos-changed frame left top width height)
         (make-frame-invisible frame))
    (modify-frame-parameters
     frame
     `((width . (text-pixels . ,width))
       (height . (text-pixels . ,height))
       (user-size . t)
       (left . (+ ,left))
       (top . (+ ,top))
       (user-position . t)
       (lsp-ui-doc--window-origin . ,(selected-window))
       (lsp-ui-doc--buffer-origin . ,(current-buffer))
       (lsp-ui-doc--no-focus . t)
       (right-fringe . 0)
       (left-fringe . 0)))
    ;; Insert hr lines after width is computed
    (lsp-ui-doc--fix-hr-props)
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(provide 'lsp-frame-hack)
;;; lsp-frame-hack.el ends here
