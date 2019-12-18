;;; experimental-agenda-views.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-17 20:11]

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
          ;; ("t" "Todo" tags-todo ,dev-tag
          ;;            ((org-agenda-overriding-header "Stuck Projects")
          ;;             (org-agenda-skip-function 'my/dev-show-stuck-projects)
          ;;             (org-tags-match-list-sublevels 'indented)))
          ;; ("t" "Test "tags-todo (concat ,dev-tag "-PEOPLE")
          ;;              ((org-agenda-overriding-header "Active Projects")
          ;;               (org-agenda-skip-function 'my/dev-show-active-projects)
          ;;               (org-tags-match-list-sublevels 'indented)))
          ("T" "Test" tags-todo ,(concat dev-tag "&TODO=\"NEXT\"")
           ((org-agenda-overriding-header "Things to do")))

(provide 'experimental-agenda-views)
;;; experimental-agenda-views.el ends here
