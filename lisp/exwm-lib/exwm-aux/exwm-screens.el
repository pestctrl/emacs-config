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
(require 'cl)
(require 'dash)
(require 'exwm-randr)
(require 'exwm-workspace-aux)
(require 'exwm-wallpaper)
(require 'subr-x)

(defun remove-no-mode-screens (x)
  (let ((entries (remove-if-not (lambda (x) (string-match-p "is not disconnected but" x))
                                x)))
    (if (null entries)
        x
      (let ((remove-list (append entries
                                 (mapcar (lambda (entry)
                                           (string-match "Output \\([A-z0-9-]+\\) is not disconnected but has no modes" entry)
                                           (match-string 1 entry))
                                         entries))))
        (remove-if (lambda (x) (member x remove-list))
                   x)))))

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

(require 'emacs-custom-load-or-ask)

(defclass screen-config ()
  ((name :initarg :name :type string)
   (width :initarg :width :type number)
   (height :initarg :height :type number)
   (xpos :initarg :xpos :type number)
   (ypos :initarg :ypos :type number)))

(defun my/read-screen-configuration ()
  (when (y-or-n-p "Read a screen configuration? ")
    (let ((screens (my/get-screens))
          (x-offset 0)
          screen-configs)
      (while screens
        (let* ((screen-name
                (completing-read
                 "Which screen is on the leftmost side? "
                 screens))
               (resolution
                (-->
                 (format "Screen resolution for '%s'? " screen-name)
                 (completing-read
                  it
                  '("3840x2160" "2560x1440" "1920x1080" "2256x1504"))
                 (string-trim it)
                 (split-string it "x")))
               (y-pos
                (read-number "Y offset (0)? " 0)))
          (setq screens (delete screen-name screens))
          (push (make-instance
                 'screen-config
                 :name screen-name
                 :width (string-to-number (car resolution))
                 :height (string-to-number (cadr resolution))
                 :xpos x-offset :ypos y-pos)
                screen-configs)
          (setq x-offset (+ x-offset (string-to-number (car resolution))))))
      (reverse screen-configs))))

(defun-prompt ec/load-or-ask-screen-config screen-config (sym prompt)
  (customize-save-variable sym (my/read-screen-configuration)))

(ec/load-or-ask-screen-config 'my-ec/screen-config
                              "Read a screen configuration? ")

(defun my/setup-screens ()
  (interactive)
  (unwind-protect
      (progn
        (-->
         #'(lambda (screen-config)
             (format "xrandr --output %s --mode %dx%d --pos %dx%d"
                     (slot-value screen-config 'name)
                     (slot-value screen-config 'width)
                     (slot-value screen-config 'height)
                     (slot-value screen-config 'xpos)
                     (slot-value screen-config 'ypos)))
         (mapconcat it my-ec/screen-config " && ")
         (shell-command it))
        (my/update-exwm-after-monitor-change))))

(defun my/update-exwm-after-monitor-change ()
  (interactive)
  (setup-workspace-monitors)
  (setup-wallpaper)
  (setq exwm-workspace-number (length my-ec/screen-config))
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

(define-minor-mode exwm-presentation-mode
  "Make both screen outputs display the same thing"
  :lighter nil
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
