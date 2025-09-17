;;; exwm-startup.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-23 12:20]

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
(defvar exwm-startup-programs
  '(("megasync" "QT_SCALE_FACTOR=1 megasync")
    "/usr/lib/kdeconnectd"
    ("compton" "compton -f -i .7 -b")
    ;; ("compton -f -i .7 -b --backend glx --blur-background --blur-method kawase --blur-strength 2")
    "start-pulseaudio-x11;pactl upload-sample /usr/share/sounds/gnome/default/alerts/drip.ogg beep; pactl load-module module-x11-bell sample=beep; xset b 100"
    "kdeconnect-indicator"
    "qtox"
    ))

(defvar exwm-computer-programs
  '("dunst"
    "nm-applet"
    "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    "/usr/lib/notification-daemon-1.0/notification-daemon"))

(defun start-minimum-programs ()
  (dolist (program exwm-computer-programs)
    (if (listp program)
        (start-process-shell-command (car program) nil (cadr program))
      (start-process-shell-command (file-name-nondirectory program) nil program))))

(defun call-startup-programs ()
  (interactive)
  (start-minimum-programs)
  (when (y-or-n-p "Run startup programs? ")
    (dolist (program exwm-startup-programs)
      (if (listp program)
          (start-process-shell-command (car program) nil (cadr program))
        (start-process-shell-command (file-name-nondirectory program) nil program)))))

(add-to-list 'exwm-manage-configurations
             '((equal exwm-class-name "MEGAsync")
               floating t))

(add-to-list 'exwm-manage-configurations
             '((equal exwm-title "Picture-in-Picture")
               floating nil))

(provide 'exwm-startup)
;;; exwm-startup.el ends here
