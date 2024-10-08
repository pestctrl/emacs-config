;;; caldav.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-10-08 05:56]

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
(use-package org-caldav)

;; URL of the caldav server
(setq org-caldav-url "https://nextcloud.notak8scluster.ddnsfree.com/remote.php/dav/calendars/lambda")

;; calendar ID on server
(setq org-caldav-calendar-id "lambda-production")

;; Org filename where new entries from calendar stored
(setq org-caldav-inbox (my/agenda-file "calendars/production.org"))

(setq org-caldav-calendars
      (list
       (list :calendar-id "lambda-personal"
             :inbox (my/agenda-file "calendars/personal.org"))
       (list :calendar-id "lambda-production"
             :inbox (my/agenda-file "calendars/production.org"))))

;; Additional Org files to check for calendar events
(setq org-caldav-files nil)

;; Usually a good idea to set the timezone manually
(setq org-icalendar-timezone "America/Chicago")

(setq org-caldav-skip-conditions nil)

(provide 'my-org-caldav)
;;; caldav.el ends here
