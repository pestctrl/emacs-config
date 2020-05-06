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
      org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday 6)

(setq org-tags-match-list-sublevels 'indented)
(setq org-agenda-span 'day)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-sticky t)

;;(org-agenda-load-file-list)

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets `((nil :maxlevel . 9)
                           (my/all-agenda-files :maxlevel . 9)
                           ("~/MEGA/org/entries/panic.org" :maxlevel . 9)))

(setq org-refile-use-cache t)

(setq org-refile-target-verify-function
      (lambda () 
        (let ((tags (org-get-tags-at)))
          (and (not (member "ARCHIVE" tags))
               (not (equal "DONE" (org-get-todo-state)))))))

(setq org-agenda-show-future-repeats nil)

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-compact-blocks t)

(provide 'my-org-agenda-misc)
;;; my-org-agenda-misc.el ends here
