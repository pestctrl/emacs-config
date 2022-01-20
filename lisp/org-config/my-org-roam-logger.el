;;; my-org-roam-journal.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-01-19 22:02]

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
(require 'org-roam)
(require 'org-roam-util)

(defvar my/current-logger-cache nil)
(defvar my/org-roam-logger-filter-fun nil)

(defun my/org-roam-logger-capture-current (arg)
  (interactive "P")
  (when (or (null my/current-logger-cache) arg)
    (setq my/current-logger-cache
          (org-roam-node-read nil my/org-roam-logger-filter-fun)))
  (org-roam-capture-
   :node my/current-logger-cache
   :templates '(("d" "default" entry "* %<%H:%M> %?"
                 :unnarrowed t
                 :target
                 (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n"
                                ("Journal" "%<%b %d, %Y>"))))
   :props '(:finalize find-file)))

(provide 'my-org-roam-logger)
;;; my-org-roam-logger.el ends here
