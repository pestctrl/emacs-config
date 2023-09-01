;;; exwm-wallpaper.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-19 10:33]

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

(defvar wallpaper-path "/home/benson/.emacs.d/res/digital_space_universe_4k_8k-wide.jpg")
(defvar live-wallpaper-path "/home/benson/MEGA/pictures/wallpapers/videos/bg.mp4")

(defun setup-wallpaper ()
  (start-process-shell-command "feh" nil (concat "feh --bg-fill " wallpaper-path)))

(defun setup-live-wallpaper () 
  (when (get-process "xwinwrap")
    (delete-process "xwinwrap"))
  (start-process-shell-command
   "xwinwrap" nil
   (format "xwinwrap -ni -ov -g 1920x1080+1280+0 -s -st -sp -nf -- mpv --loop=inf -wid WID %s"
           live-wallpaper-path)))


(provide 'exwm-wallpaper)
;;; exwm-wallpaper.el ends here
