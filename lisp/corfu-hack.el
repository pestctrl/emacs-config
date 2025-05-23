;;; corfu-hack.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-01-26 15:36]

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
(require 'exwm-screens)
(require 'corfu)

;; (defun get-focused-monitor-geometry ()
;;   "Get the geometry of the monitor displaying the selected frame in EXWM."
;;   (let* ((name
;;           (-->
;;            (format "xrandr | grep %s"
;;                    (exwm-workspace-current-monitor))
;;            (shell-command-to-string it))))
;;     (string-match (rx (+? nonl)
;;                       (group (+ digit)) "x" (group (+ digit)) "+"
;;                       (group (+ digit)) "+" (group (+ digit)))
;;                   name)
;;     (list (string-to-number (match-string 3 name)) ; X
;;           (string-to-number (match-string 4 name)) ; Y
;;           (string-to-number (match-string 1 name)) ; Width
;;           (string-to-number (match-string 2 name)) ; Height
;;           )))

;; Requires libXrandr
(defun get-focused-monitor-geometry ()
  "Get the geometry of the monitor displaying the selected frame in EXWM."
  (let* ((monitor-attrs (frame-monitor-attributes))
         (workarea (assoc 'workarea monitor-attrs))
         (geometry (cdr workarea)))
    (list (nth 0 geometry)              ; X
          (nth 1 geometry)              ; Y
          (nth 2 geometry)              ; Width
          (nth 3 geometry)              ; Height
          )))

;; (get-focused-monitor-geometry)

(defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height)
  "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."
  ;; Get the geometry of the currently focused monitor
  (let* ((monitor-geometry (get-focused-monitor-geometry))
         (monitor-x (nth 0 monitor-geometry))
         (monitor-y (nth 1 monitor-geometry))
         ;; You may want to adjust the logic below if you have specific
         ;; preferences on where on the monitor the posframe should
         ;; appear.  Currently, it places the posframe at its intended
         ;; X and Y, but ensures it's within the bounds of the focused
         ;; monitor.
         (new-x (+ monitor-x x))
         (new-y (+ monitor-y y)))

    ;; Call the original function with potentially adjusted coordinates
    (funcall orig-fun frame new-x new-y width height)))


(advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness)

(provide 'corfu-hack)
;;; corfu-hack.el ends here
