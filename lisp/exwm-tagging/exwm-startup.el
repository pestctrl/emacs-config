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
(require 'exwm-tag)

(defvar exwm-startup-programs
  '("megasync"
    "deadd-notification-center"
    "/usr/lib/kdeconnectd"
    "compton -f -i .7 -b"
    ;; ("compton -f -i .7 -b --backend glx --blur-background --blur-method kawase --blur-strength 2")
    "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    "/usr/lib/notification-daemon-1.0/notification-daemon"
    "nm-applet"
    "start-pulseaudio-x11;pactl upload-sample /usr/share/sounds/gnome/default/alerts/drip.ogg beep; pactl load-module module-x11-bell sample=beep; xset b 100"
    "gitwatch -r origin -b laptop -m 'Gitwatch commit: %d' ~/MEGA/org/2019-05-agenda"
    ))

(defun call-startup-programs ()
  (dolist (program exwm-startup-programs)
    (if (listp program)
        (launch-program (car program) (cadr program))
      (launch-program program))))

(provide 'exwm-startup)
;;; exwm-startup.el ends here
