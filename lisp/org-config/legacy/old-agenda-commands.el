;;; old-agenda-commands.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-04-16 06:44]

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

(setq org-agenda-custom-commands
      `(("d" ,(format "%s\tDev" (all-the-icons-faicon "code" :face 'all-the-icons-lcyan :v-adjust 0.1))
         ((org-ql-block '(and (tags "dev")
                              (tags "refile"))
                        ((org-ql-block-header "Refile tasks")))
          (org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (property "DELAYED")
                              (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                        ((org-ql-block-header "Previously Delayed")))
          (my/org-ql-stuck-projects "dev"
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects "dev"
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (org-ql-block '(and (tags "dev")
                              (todo "WAIT"))
                        ((org-ql-block-header "Waiting tasks")))
          (org-ql-block '(and (tags "dev")
                              (todo "NEXT"))
                        ((org-ql-block-header "Things to do")))
          (org-ql-block '(and (tags "pinned")
                              (or (my/top-level)
                                  (eq (opr/get-type) 'project)))
                        ((org-ql-block-header "Pinned Projects")
                         (org-ql-indent-levels t)
                         (org-use-tag-inheritance nil)))
          (agenda ""
                  ((org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-function (lambda ()
                                               (when (or (when-let (delayed (org-entry-get (point) "DELAYED"))
                                                           (org-time< (org-matcher-time "<now>") delayed))
                                                         (opr/is-project)
                                                         (not (odl/part-of-current-level-p)))
                                                 (outline-next-heading))))
                   (org-super-agenda-groups '((:name "Delayed" :pred
                                                     ((lambda (item)
                                                        (when-let (marker (or (get-text-property 0 'org-marker item)
                                                                              (get-text-property 0 'org-hd-marker item)))
                                                          (with-current-buffer (marker-buffer marker)
                                                            (goto-char marker)
                                                            (and ;; (not (string-match-p "SCHEDULED" item))
                                                                 (org-entry-get (point) "DELAYED")))))))
                                              (:name "The Plan" :and (:tag "PLAN" :log nil))
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :and (:deadline future :not (:todo "DONE")))
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today))))))))))
         ((org-agenda-start-with-log-mode '(closed))))
        ("M" "\tMinimal"
         ((org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (property "DELAYED")
                              (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                        ((org-ql-block-header "Previously Delayed")))
          (my/org-ql-stuck-projects "dev"
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects "dev"
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (org-ql-block '(and (tags "pinned")
                              (or (my/top-level)
                                  (eq (opr/get-type) 'project)))
                        ((org-ql-block-header "Pinned Projects")
                         (org-ql-indent-levels t)
                         (org-use-tag-inheritance nil)))
          (agenda ""
                  ((org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-function (lambda ()
                                               (when (or (when-let (delayed (org-entry-get (point) "DELAYED"))
                                                           (org-time< (org-matcher-time "<now>") delayed))
                                                         (member (org-get-todo-state) '("HOLD" "TICKLER")))
                                                 (outline-next-heading))))
                   (org-super-agenda-groups '((:name "Delayed" :pred
                                                     ((lambda (item)
                                                        (when-let (marker (or (get-text-property 0 'org-marker item)
                                                                              (get-text-property 0 'org-hd-marker item)))
                                                          (with-current-buffer (marker-buffer marker)
                                                            (goto-char marker)
                                                            (and ;; (not (string-match-p "SCHEDULED" item))
                                                                 (org-entry-get (point) "DELAYED")))))))
                                              (:name "The Plan" :and (:tag "PLAN" :log nil))
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :and (:deadline future :not (:todo "DONE")))
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today))))))))))
         ((org-agenda-start-with-log-mode '(closed))))
        ("l" "\tLeisure"
         ((org-ql-block '(and (tags "leisure")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (property "DELAYED")
                              (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                        ((org-ql-block-header "Previously Delayed")))
          (my/org-ql-stuck-projects "leisure"
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects "leisure"
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (org-ql-block '(and (tags "pinned")
                              (or (my/top-level)
                                  (eq (opr/get-type) 'project)))
                        ((org-ql-block-header "Pinned Projects")
                         (org-ql-indent-levels t)
                         (org-use-tag-inheritance nil)))
          (agenda ""
                  ((org-agenda-tag-filter-preset (quote ("+leisure")))
                   (org-agenda-skip-function (lambda ()
                                               (when (or (when-let (delayed (org-entry-get (point) "DELAYED"))
                                                           (org-time< (org-matcher-time "<now>") delayed))
                                                         (member (org-get-todo-state) '("HOLD" "TICKLER")))
                                                 (outline-next-heading))))
                   (org-super-agenda-groups '((:name "Delayed" :pred
                                                     ((lambda (item)
                                                        (when-let (marker (or (get-text-property 0 'org-marker item)
                                                                              (get-text-property 0 'org-hd-marker item)))
                                                          (with-current-buffer (marker-buffer marker)
                                                            (goto-char marker)
                                                            (and ;; (not (string-match-p "SCHEDULED" item))
                                                                 (org-entry-get (point) "DELAYED")))))))
                                              (:name "The Plan" :and (:tag "PLAN" :log nil))
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :and (:deadline future :not (:todo "DONE")))
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today))))))))))
         ((org-agenda-start-with-log-mode '(closed))
          (org-agenda-files `(,(my/agenda-file "leisure.org")))))
        ("t" "\tTest"
         ((agenda ""
                  ((org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-function (lambda ()
                                               (when (or (when-let (delayed (org-entry-get (point) "DELAYED"))
                                                           (org-time< (org-matcher-time "<now>") delayed))
                                                         (member (org-get-todo-state) '("HOLD" "TICKLER")))
                                                 (outline-next-heading))))
                   (org-super-agenda-groups '((:name "Upcoming" :todo "TASK"))))))
         ((org-agenda-start-with-log-mode '(closed))))
        ("D" "\tdev-without-active"
         ((org-ql-block '(and (tags "dev")
                              (tags "refile"))
                        ((org-ql-block-header "Refile tasks")))
          (my/org-ql-stuck-projects "dev"
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects "dev"
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (org-ql-block '(and (tags "dev")
                              (todo "WAIT"))
                        ((org-ql-block-header "Waiting tasks")))
          (org-ql-block '(and (tags "dev")
                              (todo "NEXT"))
                        ((org-ql-block-header "Things to do")))
          (agenda ""
                  ((org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :deadline future)
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today)))))))))))
        ("h" "\tDev Hold and Delay"
         ((org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (property "DELAYED")
                              (org-time<= (org-entry-get (point) "DELAYED")
                                          (org-matcher-time "<now>")))
                        ((org-ql-block-header "Past Delayed Projects")))
          (org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (property "DELAYED")
                              (org-time> (org-entry-get (point) "DELAYED")
                                         (org-matcher-time "<now>")))
                        ((org-ql-block-header "Delayed projects")))
          (org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (not (property "DELAYED"))
                              (let* ((ts (opr/get-type-and-state))
                                     (type (car ts))
                                     (state (cdr ts)))
                                (and (eq type 'project)
                                     (eq state 'invis))))
                        ((org-ql-block-header "Invisible projects")))
          (org-ql-block '(and (tags "dev")
                              (or (todo "HOLD")
                                  (descendants (todo "HOLD"))))
                        ((org-ql-block-header "Hold projects")
                         (org-ql-indent-levels t)))))
        ("n" "\tNext Tasks List" tags-todo "-REFILE-HOLD-WAIT"
         ((org-agenda-skip-function 'my/show-next-tasks-and-standalone-tasks)
          (org-agenda-overriding-header "Next Tasks list")
          (org-agenda-sorting-strategy '(deadline-up))))
        ("L" "\tLeaf Task List" tags-todo "-REFILE-HOLD-WAIT"
         ((org-agenda-skip-function 'my/show-leaf-tasks)
          (org-agenda-overriding-header "Next Tasks list")
          (org-agenda-finalize-hook '(org-agenda-add-separater-between-project))))
        ("a" . "\tAgendas")
        ("aa" "\tRegular Agenda" agenda "")
        ("ac" . "\tClocking")
        ("acw" "Last week's clock" agenda ""
         ((org-agenda-overriding-arguments
           (list nil (my/last-or-last-last-saturday) 9))
          (org-agenda-start-with-log-mode 'clockcheck)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))))
        ("acd" "Today's clock" agenda ""
         ((org-agenda-start-with-log-mode 'clockcheck)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))))
        ("at" "\tAgenda Today" agenda "")
        ("aw" "\tWeekly view" agenda ""
         ((org-scheduled-past-days 0)
          (org-agenda-overriding-arguments
           (list nil (my/this-or-last-saturday) 9))
          (org-agenda-tag-filter-preset (quote ("+dev")))))
        ("al" "\tLast week" agenda ""
         ((org-agenda-overriding-arguments (list nil (my/last-or-last-last-saturday)
                                                 9))
          (org-agenda-tag-filter-preset (quote ("+dev")))))
        ("ad" "\tDev agenda" agenda ""
         ((org-agenda-skip-function 'my/agenda-custom-skip)
          (org-agenda-tag-filter-preset (quote ("+dev")))
          (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                     (:name "Overdue" :and (:deadline past :log nil))
                                     (:name "Upcoming" :deadline future)
                                     (:name "Should do" :and (:scheduled past :log nil))
                                     (:name "Today" :time-grid t
                                            :and (:not (:and (:not (:scheduled today)
                                                                   :not (:deadline today)))))))))
        ("am" "\tMinimal agenda" agenda ""
         ((org-agenda-tag-filter-preset (quote ("+dev")))
          (org-agenda-skip-function (lambda ()
                                      (when (or (when-let (delayed (org-entry-get (point) "DELAYED"))
                                                  (org-time< (org-matcher-time "<now>") delayed))
                                                (member (org-get-todo-state) '("HOLD" "TICKLER")))
                                        (outline-next-heading))))
          (org-super-agenda-groups '((:name "Delayed" :pred
                                            ((lambda (item)
                                               (when-let (marker (or (get-text-property 0 'org-marker item)
                                                                     (get-text-property 0 'org-hd-marker item)))
                                                 (with-current-buffer (marker-buffer marker)
                                                   (goto-char marker)
                                                   (and ;; (not (string-match-p "SCHEDULED" item))
                                                    (org-entry-get (point) "DELAYED")))))))
                                     (:name "The Plan" :and (:tag "PLAN" :log nil))
                                     (:name "Overdue" :and (:deadline past :log nil))
                                     (:name "Upcoming" :and (:deadline future :not (:todo "DONE")))
                                     (:name "Should do" :and (:scheduled past :log nil))
                                     (:name "Today" :time-grid t
                                            :and (:not (:and (:not (:scheduled today)
                                                                   :not (:deadline today)))))))))
        ("p" . "\tProd")
        ("pa" "\tAll" ,(production-agenda "time"))
        ("ps" "\tschool" ,(production-agenda "school"))
        ("pf" "\tfamily" ,(production-agenda "family"))
        ("g" ,(format "%s\tGeneral View" (all-the-icons-material "all_inclusive" :face 'all-the-icons-cyan-alt))
         ((tags-todo "+sandbox+refile"
                     ((org-agenda-overriding-header "Refile tasks")))
          (tags-todo "+sandbox"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'my/show-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-HOLD+TODO+sandbox/WAIT"
                     (;;(org-agenda-skip-function 'my/only-next-projects-and-tasks)
                      (org-agenda-overriding-header "Tasks in other courts")))
          ;;(org-ql-agenda-function "")
          (agenda ""
                  ((org-agenda-skip-function 'my/agenda-custom-skip)
                   (org-agenda-tag-filter-preset (quote ("+sandbox")))
                   (org-super-agenda-groups '((:name "Overdue" :and (:deadline past :log nil ))
                                              (:name "Upcoming" :deadline future)
                                              (:name "Should do" :and (:scheduled past :log nil ))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today)))))))))))
        ("m" . "\tMaintainence")
        ("md" "\tDone Tasks"
         ((org-ql-block '(and (not (tags "ARCHIVE"))
                              (todo "DONE" "ABANDON")
                              (or (not (my/top-level))
                                  (not (tags "dev"))))
                        ((org-ql-block-header "Done tasks")
                         (org-agenda-files ',(remove-if (lambda (x)
                                                          (member (expand-file-name x) my/non-agenda-files))
                                                        my/all-agenda-files))))))
        ("ma" "\tArchive trees"
         ((org-ql-block '(and (tags "ARCHIVE")
                              (my/top-level)
                              (let ((size (- (save-excursion (org-end-of-subtree t t)) (point))))
                                (>= size 50000)))
                        ((org-ql-block-header "Big archive trees")))
          (org-ql-block '(and (tags "ARCHIVE")
                              (my/top-level)
                              (let ((size (- (save-excursion (org-end-of-subtree t t)) (point))))
                                (< size 50000)))
                        ((org-ql-block-header "Small archive trees"))))
         ((my/delete-blocks nil)
          (org-use-tag-inheritance nil)))
        ("mr" "\tRecategorize dev to sandbox" todo (mapconcat #'identity org-done-keywords-for-agenda "|")
         ((org-agenda-skip-function 'my/show-top-level)
          (org-agenda-files '(,(my/agenda-file "dev.org")))))
        ("mv" "\tInvalid todos"
         ((org-ql-block '(and (tags "prod")
                              (todo "TODO" "META" "META1" "EMPTY" "ONE")
                              (not (my/valid-todo)))
                        ((org-ql-block-header "Production")))
          (org-ql-block '(and (tags "dev")
                              (todo "TODO" "META" "META1" "EMPTY" "ONE")
                              (not (my/valid-todo)))
                        ((org-ql-block-header "dev")))
          (org-ql-block '(and (tags "sandbox")
                              (todo "TODO" "META" "META1" "EMPTY" "ONE")
                              (not (my/valid-todo)))
                        ((org-ql-block-header "sandbox")))))
        ("mA" "\tAmbiguous todos"
         ((org-ql-block '(and (todo)
                              (my/ambiguous-todo))
                        ((org-ql-block-header "sandbox")))))
        ("c" . "\tcalfw")
        ("c2" "\tCalfw 2 weeks" cfw:open-org-2week-calendar "")
        ("f" . "\tFlip through")
        ("fc" "\tComms" tags-todo "datetime"
         ((org-agenda-overriding-header "Comms")))
        ("fj" "\tReviews and Journals" tags "LEVEL=3&ITEM={Review for}|LEVEL=3&journal"
         ((org-agenda-files '(,(my/org-file "entries/reviews.gpg")
                              ,(my/org-file "entries/journal.gpg")))
          (org-agenda-sorting-strategy '(tsia-down))))
        ("fr" "\tReviews" tags "LEVEL=3&ITEM={Review for}"
         ((org-agenda-files '(,(my/org-file "entries/reviews.gpg")
                              ,(my/org-file "entries/journal.gpg")))
          (org-agenda-sorting-strategy '(tsia-down))))
        ("o" "\tOffline" tags-todo "offline"
         ((org-tags-match-list-sublevels nil)))
        ("b" "\tBored" tags-todo "+short-grow"
         ((org-tags-match-list-sublevels nil)))
        ("P" "\tPlan" agenda ""
         ((org-agenda-tag-filter-preset (quote ("+PLAN")))
          (org-super-agenda-groups '((:name "Delayed" :pred
                                            ((lambda (item)
                                               (when-let (marker (or (get-text-property 0 'org-marker item)
                                                                     (get-text-property 0 'org-hd-marker item)))
                                                 (with-current-buffer (marker-buffer marker)
                                                   (goto-char marker)
                                                   (and ;; (not (string-match-p "SCHEDULED" item))
                                                    (org-entry-get (point) "DELAYED")))))))
                                     (:name "The Plan" :and (:tag "PLAN" :log nil))
                                     (:name "Overdue" :and (:deadline past :log nil))
                                     (:name "Upcoming" :and (:deadline future :not (:todo "DONE")))
                                     (:name "Should do" :and (:scheduled past :log nil))
                                     (:name "Today" :time-grid t
                                            :and (:not (:and (:not (:scheduled today)
                                                                   :not (:deadline today)))))))))))

(defun production-agenda (tag)
  `((org-ql-block '(and (tags ,tag)
                        (todo "STUFF"))
                  ((org-ql-block-header "Refile tasks")))
    (my/org-ql-stuck-projects ,tag
                              ((org-ql-block-header "Stuck Projects")
                               (org-ql-indent-levels t)))
    (org-ql-block '(and (tags ,tag)
                        (todo "WAIT"))
                  ((org-ql-block-header "Waiting tasks")))
    (org-ql-block '(and (tags ,tag)
                        (todo "NEXT"))
                  ((org-ql-block-header "Things to do")))
    (agenda ""
            ((org-agenda-skip-function ,(my/agenda-custom-skip))
             (org-agenda-tag-filter-preset (quote (,(concat "+" tag))))
             (org-super-agenda-groups '((:name "Dev things" :file-path "dev.org")
                                        (:name "Overdue" :and (:deadline past :log nil))
                                        (:name "Upcoming" :deadline future)
                                        (:name "Should do" :and (:scheduled past :log nil))
                                        (:name "Today" :time-grid t
                                               :and (:not (:and (:not (:scheduled today)
                                                                      :not (:deadline today)))))))))))



(provide 'old-agenda-commands)
;;; old-agenda-commands.el ends here
