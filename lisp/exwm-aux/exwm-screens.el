;;; exwm-screens.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-19 10:25]

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
(require 'dash)
(require 'exwm-randr)
(require 'exwm-workspace-aux)
(require 'exwm-wallpaper)

(defun my/get-screens ()
  (-> "xrandr | grep ' connected ' | cut -d ' ' -f 1"
      (shell-command-to-string)
      (split-string "\n")
      (reverse)
      (cdr)
      (reverse)
      (cl-sort (lambda (a b)
                 (cond ((string-match-p "^eDP" a) t)
                       ((string-match-p "^eDP" b) nil)
                       (t nil))))))

(defun position-screen (screen relative-to)
  (interactive (cl-destructuring-bind (primary . secondary) (my/get-screens)
                 (list (completing-read "Which screen? " secondary)
                       (completing-read "Against which screen? " (cons primary secondary)))))
  (let ((response (completing-read (format "Resolution for %s? " screen) '("2560x1440" "1920x1080" "3840x2160") nil t "^"))
        (pos (completing-read "Position? " '("left-of" "above") nil t "^")))
    (shell-command (format "xrandr --output %s --mode %s --%s %s" screen response pos relative-to))))

(defun my/setup-screens ()
  (interactive)
  (cl-destructuring-bind (primary . secondaries) (my/get-screens)
    (loop for secondary in secondaries
          do (when (y-or-n-p (format "Monitor %s detected.  Setup? " secondary))
               (position-screen secondary primary))))
  (setup-workspace-monitors)
  (setup-wallpaper)
  (exwm-workspace-after-monitor-change))

(defun my/disconnect-screen (screen)
  (interactive (list (let ((screens (cdr (my/get-screens))))
                       (or (and (zerop (1- (length screens)))
                                (car screens))
                           (completing-read "Which screen? " (cdr (my/get-screens)))))))
  (shell-command (format "xrandr --output %s --off" screen))
  (setq exwm-randr-workspace-monitor-plist nil)
  (exwm-randr-refresh)
  (setup-wallpaper))

(provide 'exwm-screens)
;;; exwm-screens.el ends here
