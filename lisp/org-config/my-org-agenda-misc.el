;;; my-org-agenda-misc.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-06 18:37]

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
(require 'org)

(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled 7)
(setq org-agenda-start-on-weekday 6)

(setq org-tags-match-list-sublevels 'indented)
(setq org-agenda-span 'day)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-sticky t)

;;(org-agenda-load-file-list)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-compact-blocks t)

(provide 'my-org-agenda-misc)
;;; my-org-agenda-misc.el ends here
