;;; my-org-agenda-commands.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-04-29 21:31]

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

(add-to-list 'load-path 
             "~/.emacs.d/submodule/org-ql")
(require 'org-ql)
(require 'org-ql-search)
(require 'org-ql-custom-stuck-projects)

(defvar org-ql-indent-levels nil)

(defun org-agenda-add-separater-between-project ()
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let ((start-pos (point))
          (previous t))
      (re-search-forward " +agenda: +[^\\. ]" nil t)
      (while (re-search-forward " +agenda: +[^\\. ]" nil t)
        (beginning-of-line)
        (insert "=============================================\n")
        (forward-line)))))

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
            ((org-agenda-skip-function 'my/agenda-custom-skip)
             (org-agenda-span 'day)
             (org-agenda-tag-filter-preset (quote (,(concat "+" tag))))
             (org-agenda-skip-deadline-if-done t)
             (org-agenda-skip-scheduled-if-done t)
             (org-super-agenda-groups '((:name "Dev things" :file-path "dev.org")
                                        (:name "Overdue" :and (:deadline past :log nil))
                                        (:name "Upcoming" :deadline future)
                                        (:name "Should do" :and (:scheduled past :log nil))
                                        (:name "Today" :time-grid t
                                               :and (:not (:and (:not (:scheduled today)
                                                                      :not (:deadline today)))))))))))

(defconst my/non-agenda-files
  `(,(my/org-file "entries/reviews.gpg") 
    ,(my/agenda-file "datetree.org") 
    ,(my/agenda-file "reference.org") 
    ,(my/org-file "entries/journal.gpg")))

(defconst my/all-agenda-files
  (cons (my/agenda-file "eternal.org")
        org-agenda-files))

(defun my/valid-todo ()
  (let (seen-non-todo)
    (save-excursion 
      (not
       (catch 'break
         (while (org-up-heading-safe)
           (if (null (org-get-todo-state))
               (setf seen-non-todo t)
             (when seen-non-todo
               (throw 'break t)))))))))

(defun my/ambiguous-todo ()
  (and (opr/type-of-project)
       (opr/type-of-task)))

(setq org-agenda-custom-commands
      `(("d" ,(format "%s\tDev" (all-the-icons-faicon "code" :face 'all-the-icons-lcyan :v-adjust 0.1))
         ((org-ql-block '(and (tags "dev")
                              (tags "refile"))
                        ((org-ql-block-header "Refile tasks")))
          (org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (my/top-level)
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
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-skip-scheduled-if-done t)
                   (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :deadline future)
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today)))))))))))
        ("M" "\tMinimal"
         ((org-ql-block '(and (tags "dev")
                              (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (my/top-level)
                              (property "DELAYED")
                              (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                        ((org-ql-block-header "Previously Delayed")))
          (my/org-ql-stuck-projects "dev"
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects "dev"
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-skip-scheduled-if-done t)
                   (org-agenda-skip-function (lambda ()
                                               (when (string= "TICKLER" (org-get-todo-state))
                                                 (outline-next-heading))))
                   (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :deadline future)
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today)))))))))))
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
                  ((org-agenda-span 'day)
                   (org-agenda-tag-filter-preset (quote ("+dev")))
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-skip-scheduled-if-done t)
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
                              (org-time> (org-entry-get (point) "DELAYED")
                                         (org-matcher-time "<now>")))
                        ((org-ql-block-header "Delayed projects")))
          (org-ql-block '(and (tags "dev")
                              (or (todo "HOLD")
                                  (descendants (todo "HOLD")))) 
                        ((org-ql-block-header "Hold projects")
                         (org-ql-indent-levels t)))))
        ("n" "\tNext Tasks List" tags-todo "-REFILE-HOLD-WAIT"
         ((org-agenda-skip-function 'my/show-next-tasks-and-standalone-tasks)
          (org-agenda-overriding-header "Next Tasks list")
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(deadline-up))))
        ("L" "\tLeaf Task List" tags-todo "-REFILE-HOLD-WAIT"
         ((org-agenda-skip-function 'my/show-leaf-tasks)
          (org-tags-match-list-sublevels 'indented)
          (org-agenda-overriding-header "Next Tasks list")
          (org-agenda-finalize-hook '(org-agenda-add-separater-between-project))))
        ("a" . "\tAgendas")
        ("aa" "\tRegular Agenda" agenda "")
        ("at" "\tAgenda Today" agenda ""
         ((org-agenda-span 'day)))
        ("aw" "\tWeekly view" agenda ""
         ((org-scheduled-past-days 0)
          (org-agenda-overriding-arguments (list nil (my/this-or-last-saturday)
                                                 9))
          (org-agenda-tag-filter-preset (quote ("+dev")))
          (org-agenda-skip-scheduled-if-done t)
          (org-agenda-skip-deadline-if-done t)))
        ("al" "\tLast week" agenda ""
         ((org-agenda-overriding-arguments (list nil (my/last-or-last-last-saturday)
                                                 9))
          (org-agenda-tag-filter-preset (quote ("+dev")))))
        ("ad" "\tDev agenda" agenda ""
         ((org-agenda-skip-function 'my/agenda-custom-skip)
          (org-agenda-span 'day)
          (org-agenda-tag-filter-preset (quote ("+dev")))
          (org-agenda-skip-deadline-if-done t)
          (org-agenda-skip-scheduled-if-done t)
          (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                     (:name "Overdue" :and (:deadline past :log nil))
                                     (:name "Upcoming" :deadline future)
                                     (:name "Should do" :and (:scheduled past :log nil))
                                     (:name "Today" :time-grid t
                                            :and (:not (:and (:not (:scheduled today)
                                                                   :not (:deadline today)))))))))
        ("p" . "\tProd")
        ("pa" "\tAll" ,(production-agenda "time"))
        ("pw" "\twork" ,(production-agenda "work"))
        ("ps" "\tschool" ,(production-agenda "school"))
        ("g" ,(format "%s\tGeneral View" (all-the-icons-material "all_inclusive" :face 'all-the-icons-cyan-alt))
         ((tags-todo "+sandbox+refile"
                     ((org-agenda-overriding-header "Refile tasks")))
          (tags-todo "+sandbox"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-skip-function 'my/show-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-HOLD+TODO+sandbox/WAIT"
                     (;;(org-agenda-skip-function 'my/only-next-projects-and-tasks)
                      (org-agenda-overriding-header "Tasks in other courts")
                      (org-tags-match-list-sublevels t)))
          ;;(org-ql-agenda-function "")
          (agenda ""
                  ((org-agenda-skip-function 'my/agenda-custom-skip)
                   (org-agenda-span 'day)
                   (org-agenda-tag-filter-preset (quote ("+sandbox")))
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-skip-scheduled-if-done t)
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
        ("fC" "\tLook at clocking" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-start-with-log-mode '(closed clock))
          (org-agenda-clockreport-mode t)))
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
         ((org-tags-match-list-sublevels nil)))))


(defun my/this-or-last-saturday ()
  (org-read-date nil nil
                 (if (string= "6" (format-time-string "%u"))
                     "."
                   "-sat")))

(defun my/last-or-last-last-saturday ()
  (org-read-date nil nil
                (if (<= 6 (string-to-number (format-time-string "%u")))
                    "-2sat"
                  "-sat")))

(defun cfw:open-org-2week-calendar (&rest args)
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (save-excursion
    (let* ((source1 (cfw:org-create-source))
           (curr-keymap (if cfw:org-overwrite-default-keybinding cfw:org-custom-map cfw:org-schedule-map))
           (cp (cfw:create-calendar-component-buffer
                :date (-as->
                       (my/this-or-last-saturday) it
                       (split-string it "-")
                       (append (cdr it) (list (car it)))
                       (mapcar #'string-to-number it))
                :view 'two-weeks
                :contents-sources (list source1)
                :custom-map curr-keymap
                :sorter 'cfw:org-schedule-sorter)))
      (switch-to-buffer (cfw:cp-get-buffer cp))
      (when (not org-todo-keywords-for-agenda)
        (message "Warn : open org-agenda buffer first.")))))
;; (pop org-agenda-custom-commands)

(provide 'my-org-agenda-commands)
;;; my-org-agenda-commands.el ends here
