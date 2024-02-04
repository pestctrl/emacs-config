;;; emacs-mail.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-05 14:47]

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

;;; Code

;; (require 'mbsync)
;; (require 'notmuch-config)
(require 'mu4e-configuration)

(setq user-mail-address "me@mail.pestctrl.io"
      user-full-name "Benson Chu")

(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(mailcap-add "text/html" "/usr/bin/xdg-open %s ")

(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(provide 'emacs-mail)
;;; emacs-mail.el ends here
