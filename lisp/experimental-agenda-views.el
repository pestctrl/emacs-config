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

    (add-to-list 'org-agenda-custom-commands
                 `("m" "People"
                   ((tags-todo ,(concat "+people" "/!" (mapconcat #'identity (cons "HOLD" my/active-projects-and-tasks) "|"))
                               ((org-agenda-overriding-header "Stuck Projects")
                                (org-agenda-skip-function 'my/show-stuck-projects)
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy
                                 '((agenda category-keep)))))
                    (tags-todo ,(concat "+people" "-short" "/!" (mapconcat #'identity my/active-projects-and-tasks "|"))
                               ((org-agenda-overriding-header "Active Projects")
                                (org-agenda-skip-function 'my/show-active-projects)
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy
                                 '((agenda category-keep)))))
                    (tags-todo ,(concat "+people" "/WAIT")
                               ((org-agenda-overriding-header "Waiting tasks")))
                    (tags-todo ,(concat "+people" "/NEXT")
                               ((org-agenda-overriding-header "Things to do")))
                    (agenda ""
                            ((org-agenda-skip-function 'my/agenda-custom-skip)
                             (org-agenda-span 'day)
                             (org-agenda-tag-filter-preset (quote ("+people")))
                             (org-agenda-skip-deadline-if-done t)
                             (org-agenda-skip-scheduled-if-done t)
                             (org-super-agenda-groups '((:name "Overdue" :and (:deadline past :log nil))
                                                        (:name "Upcoming" :deadline future)
                                                        (:name "Should do" :and (:scheduled past :log nil))
                                                        (:name "Today" :time-grid t
                                                               :and (:not (:and (:not (:scheduled today)
                                                                                      :not (:deadline today))))))))))))

(provide 'experimental-agenda-views)
;;; experimental-agenda-views.el ends here
