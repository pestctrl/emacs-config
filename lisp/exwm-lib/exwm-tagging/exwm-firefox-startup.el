;;; exwm-firefox-startup.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-07 12:19]

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

(defun exwm-firefox--ignore-clientmessage (orig raw-data _synthetic)
  (let ((obj (make-instance 'xcb:ClientMessage)))
    (xcb:unmarshal obj raw-data)
    (unless (= xcb:Atom:_NET_ACTIVE_WINDOW
               (slot-value obj 'type))
      (funcall orig raw-data _synthetic))))

(defun exwm-firefox-ignore-focus ()
  (advice-add #'exwm--on-ClientMessage :around
              #'exwm-firefox--ignore-clientmessage))

(defun exwm-firefox-unignore-focus ()
  (advice-remove #'exwm--on-ClientMessage
                 #'exwm-firefox--ignore-clientmessage))

(defun my/exwm-pop-to-rename)

(defun launch-firefox-startup ()
  (interactive)
  (remove-hook 'exwm-manage-finish-hook 'exwm-rename-buffer)
  (exwm-firefox-ignore-focus)
  (launch-program "firefox"))

(provide 'exwm-firefox-startup)
;;; exwm-firefox-startup.el ends here
