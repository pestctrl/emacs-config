;;; work-org-stuff.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-02-26 09:31]

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

(global-set-key (kbd "C-x C-o") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file "/home/a0487752/org/refile.org")
           "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")))

(provide 'work-org-stuff)
;;; work-org-stuff.el ends here
