;;; xephyr-helper.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 08:21]

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
(require 'exwm)
(require 'exwm-launch-program)

(setq i3-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 i3")
(setq xfce4-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 xfce4-session")
(setq kde-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 startplasma-x11")
(setq kde+exwm-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 KDEWM=/usr/bin/emacs startkde")

(defun launch-i3 ()
  (interactive)
  (launch-program i3-string))

(defun launch-xfce ()
  (interactive)
  (launch-program xfce4-string))

(defun launch-kde ()
  (interactive)
  (launch-program kde-string))

(defun launch-kde+emacs ()
  (interactive)
  (launch-program kde-string))

(setq exwm-manage-configurations `(((equal exwm-class-name "Xephyr")
                                    floating nil 
                                    char-mode t
                                    fullscreen t)
                                   ((equal exwm-class-name "plasmashell")
                                    floating t)))
(provide 'xephyr-helper)
;;; xephyr-helper.el ends here
