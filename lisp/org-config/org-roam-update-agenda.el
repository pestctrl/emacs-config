;;; org-roam-update-agenda.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-07-02 10:25]

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
(require 'org-roam-util)

(defvar orua/agenda-files nil)

(defun my/update-org-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (append
         orua/agenda-files
         (my/get-org-roam-files-by-tags '("Project" "active")))))

(advice-add #'org-agenda
            :before
            #'my/update-org-agenda-files)

(provide 'org-roam-update-agenda)
;;; org-roam-update-agenda.el ends here
