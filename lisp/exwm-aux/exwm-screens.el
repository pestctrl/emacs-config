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
(require 'subr-x)

(defun remove-no-mode-screens (x)
  (when-let ((entries (remove-if-not (lambda (x) (string-match-p "is not disconnected but" x))
                                     x)))
    (let ((remove-list (append entries
                               (mapcar (lambda (entry)
                                         (string-match "Output \\([A-z0-9-]+\\) is not disconnected but has no modes" entry)
                                         (match-string 1 entry))
                                       entries))))
      (remove-if (lambda (x) (member x remove-list))
                 x))))

(defun my/get-screens ()
  (-> "xrandr | grep ' connected ' | cut -d ' ' -f 1"
      (shell-command-to-string)
      (split-string "\n")
      (remove-no-mode-screens)
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
  (let ((response (completing-read (format "Resolution for %s? " screen) '("2560x1440" "1920x1080" "3840x2160" "1680x1050") nil t "^"))
        (pos (completing-read "Position? " '("left-of" "above") nil t "^")))
    (shell-command (format "xrandr --output %s --mode %s --%s %s" screen response pos relative-to))))

(defun my/setup-screens ()
  (interactive)
  (unwind-protect
      (let ((count 1))
        (setq exwm-workspace-number (length (my/get-screens)))
        (cl-destructuring-bind (primary . secondaries) (my/get-screens)
          (loop for secondary in secondaries
                do (when (y-or-n-p (format "Monitor %s detected.  Setup? " secondary))
                     (position-screen secondary primary)
                     (incf count))))
        (setup-workspace-monitors)
        (setup-wallpaper)
        (setq exwm-workspace-number count)
        (exwm-workspace-after-monitor-change))))

(defun my/minimal-setup-screens ()
  (interactive)
  (setup-workspace-monitors)
  (setup-wallpaper))

(defun my/disconnect-screen (screen)
  (interactive (list (let ((screens (cdr (my/get-screens))))
                       (or (and (zerop (1- (length screens)))
                                (car screens))
                           (completing-read "Which screen? " (cdr (my/get-screens)))))))
  (shell-command (format "xrandr --output %s --off" screen))
  (setq exwm-randr-workspace-monitor-plist nil)
  (exwm-randr-refresh)
  (setup-wallpaper))

(define-minor-mode exwm-presentation-mode
  "Make both screen outputs display the same thing"
  nil nil nil
  (cond (exwm-presentation-mode
         (cl-destructuring-bind (primary . secondary) (my/get-screens)
           (shell-command
            (format "xrandr --output %s --mode 1920x1080 --same-as %s"
                    (car secondary)
                    primary))
           (setq exwm-randr-workspace-monitor-plist nil)
           (exwm-randr-refresh)))
        (t
         (my/setup-screens))))

(provide 'exwm-screens)
;;; exwm-screens.el ends here
