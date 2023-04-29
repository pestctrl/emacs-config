;;; my-org-agenda-commands.el
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
(require 'use-package)
(use-package quelpa)

(require 'org-agenda)
(require 'org-get-time)
(require 'my-org-agenda-files)
(require 'my-org-agenda-misc)
(require 'my-org-tags)
(require 'all-the-icons)

(use-package org-super-agenda)
(org-super-agenda-mode)

(add-to-list 'load-path
             "~/.emacs.d/submodule/org-ql")
(use-package peg)
(use-package ov)
(use-package dash-functional)
(let ((fetcher (if (or noninteractive my-ec/at-ti) "github" "github-ssh")))
  (quelpa `(org-ql :repo "pestctrl/org-ql" :branch "my-changes" :fetcher ,fetcher)))
(require 'org-ql)
(require 'org-ql-search)
(require 'org-ql-custom-stuck-projects)
(require 'org-scan-tags-indent)

(require 'org-archive-tree-count)

(require 'my-org-calfw)

(advice-add #'cfw:org-schedule-period-to-calendar
            :override
            #'my/cfw:org-schedule-period-to-calendar)

(setq org-ql-indentor-function #'get-parent-indent-level)

;; (add-to-list 'directory-abbrev-alist
;;              '("/templates" . "/home/benson/MEGA/org/templates"))
;; (pop directory-abbrev-alist)

;; (add-to-list 'ido-rewrite-file-prompt-rules
;;              '("^/templates" . ido-set-shortcut))
;; (pop ido-rewrite-file-prompt-rules)

;; (defun ido-set-shortcut (dir)
;;   (setq ido-current-directory "/home/benson/MEGA/org/templates/"))

(define-key org-agenda-mode-map (kbd "a") 'org-agenda)

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

(defmacro my/agenda-custom-skip (tag)
  `(lambda ()
     (when (or (not (member ,tag (org-get-tags)))
               (when-let (delayed (org-entry-get (point) "DELAYED"))
                 (org-time< (org-matcher-time "<now>") delayed))
               (member (org-get-todo-state) '("HOLD" "TICKLER")))
       (outline-next-heading))))

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

(defun my/pinned-indent-level ()
  (save-excursion
    (let ((levels 0))
      (while (and (org-up-heading-safe)
                  (member "pinned" (org-get-tags)))
        (when (not (string= "CAT" (org-get-todo-state)))
          (cl-incf levels)))
      levels)))

(defun my/ambiguous-todo ()
  (and (opr/type-of-project)
       (opr/type-of-task)))

(require 'org-dev-level)

(advice-add #'org-agenda-redo
            :around
            (lambda (orig-fun &rest args)
              (save-window-excursion
                (apply orig-fun args))))
(progn
  (defun agenda-suite (name key tag &optional additional)
    (declare (indent 3))
    `((,key . ,(concat "\t" name))
      (,(concat key "c") ,(concat "\t" name " Compound View")
       ,(org-agenda-compound-view tag)
       ,additional)
      (,(concat key "h") ,(concat "\t" name " Hold and Delay")
       ,(org-agenda-hold-view tag)
       ,additional)))

  (defun minimal-agenda-suite (name key tag &optional additional)
    (declare (indent 3))
    `((,key . ,(concat "\t" name))
      (,(concat key "c") ,(concat "\t" name " Compound View")
       ,(org-agenda-minimal-view tag)
       ,additional)
      (,(concat key "h") ,(concat "\t" name " Hold and Delay")
       ,(org-agenda-hold-view tag)
       ,additional)))

  (defun org-agenda-hold-view (tag)
    `((org-ql-block '(and (tags ,tag)
                          (not (tags "habits"))
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (or (and (property "DELAYED")
                                   (org-time<= (org-entry-get (point) "DELAYED")
                                               (org-matcher-time "<now>")))
                              (and (property "NOT_TODAY")
                                   (org-time<= (org-entry-get (point) "NOT_TODAY")
                                               (org-matcher-time "<now>")))))
                    ((org-ql-block-header "Past Delayed Projects")))
      (org-ql-block '(and (tags ,tag)
                          (not (tags "habits"))
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (property "DELAYED")
                          (org-time> (org-entry-get (point) "DELAYED")
                                     (org-matcher-time "<now>")))
                    ((org-ql-block-header "Delayed projects")))
      (org-ql-block '(and (tags ,tag)
                          (not (tags "habits"))
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (property "NOT_TODAY")
                          (org-time> (org-entry-get (point) "NOT_TODAY")
                                     (org-matcher-time "<now>")))
                    ((org-ql-block-header "Delayed Today projects")))
      (org-ql-block '(and (tags ,tag)
                          (not (tags "habits"))
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (not (property "DELAYED"))
                          (let* ((ts (opr/get-type-and-state))
                                 (type (car ts))
                                 (state (cdr ts)))
                            (and (eq type 'project)
                                 (eq state 'invis))))
                    ((org-ql-block-header "Invisible projects")))
      (org-ql-block '(and (tags ,tag)
                          (tags "habits")
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (not (property "DELAYED"))
                          (let* ((ts (opr/get-type-and-state))
                                 (type (car ts))
                                 (state (cdr ts)))
                            (and (eq type 'project)
                                 (eq state 'invis))))
                    ((org-ql-block-header "Perpetual projects")))
      (org-ql-block '(todo "BLOCKED")
                    ((org-ql-block-header "Blocked projects")))
      (org-ql-block '(and (tags ,tag)
                          (or (todo "HOLD")
                              (descendants (todo "HOLD"))))
                    ((org-ql-block-header "Hold projects")
                     (org-ql-indent-levels t)))))

  (defun my/org-agenda-skip-function (tag)
    (let ((skip-fun-name (intern (format "my/org-agenda-skip-unless-%s-tag" tag))))
      (eval
       `(defun ,skip-fun-name ()
          (let ((tags (org-get-tags)))
            (unless (and (or (member ,tag tags)
                             (member "PLAN" tags))
                         (let ((delayed (org-entry-get (point) "DELAYED")))
                           (or (null delayed)
                               (org-time< delayed (org-matcher-time "<now>"))))
                         (not (member (org-get-todo-state) '("HOLD" "TICKLER"))))
              (outline-next-heading)
              (point)))))
      (eval `(function ,skip-fun-name))))

  (defun org-agenda-minimal-view (tag)
    `((org-ql-block '(and (tags ,tag)
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (property "DELAYED")
                          (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                    ((org-ql-block-header "Previously Delayed")))
      (my/org-ql-stuck-projects ,tag
                                ((org-ql-block-header "Stuck Projects")
                                 (org-ql-indent-levels t)))
      (my/org-ql-active-projects ,tag
                                 ((org-ql-block-header "Active Projects")
                                  (org-ql-indent-levels t)))
      (agenda ""
              ((org-agenda-show-log '(closed clock))
               (org-agenda-skip-function #',(my/org-agenda-skip-function tag))
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
                                          (:name "Upcoming" :and (:deadline future :not (:todo "DONE") :log nil))
                                          (:name "Should do" :and (:scheduled past :log nil))
                                          (:name "Today" :time-grid t
                                                 :and (:not (:and (:not (:scheduled today)
                                                                        :not (:deadline today))
                                                                  :log closed)))))))))

  (defun org-agenda-compound-view (tag)
    `((org-ql-block '(and (tags ,tag)
                          (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                          (property "DELAYED")
                          (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                    ((org-ql-block-header "Previously Delayed")))
      (my/org-ql-stuck-projects ,tag
                                ((org-ql-block-header "Stuck Projects")
                                 (org-ql-indent-levels t)))
      (my/org-ql-active-projects ,tag
                                 ((org-ql-block-header "Active Projects")
                                  (org-ql-indent-levels t)))
      (org-ql-block '(and (tags "pinned")
                          (not (done))
                          (or (my/top-level)
                              (eq (opr/get-type) 'project)))
                    ((org-ql-block-header "Pinned Projects")
                     (org-ql-indent-levels t)
                     (org-use-tag-inheritance nil)
                     (org-ql-indentor-function #'my/pinned-indent-level)))
      (agenda ""
              ((org-agenda-show-log '(closed clock))
               (org-agenda-skip-function #',(my/org-agenda-skip-function tag))
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
                                          (:name "Upcoming" :and (:deadline future :not (:todo "DONE") :log nil))
                                          (:name "Should do" :and (:scheduled past :log nil))
                                          (:name "Today" :time-grid t
                                                 :and (:not (:and (:not (:scheduled today)
                                                                        :not (:deadline today))
                                                                  :log closed)))))))))

  ;; Old dev view would just also see refile and tickler

  (setq org-agenda-custom-commands
        `(("p" . "\tprod")
          ,@(agenda-suite "all" "pa" "prod")
          ,@(agenda-suite "school" "ps" "school")
          ,@(agenda-suite "family" "pf" "family")
          ,@(agenda-suite "dev" "d" "dev")
          ("l" "\tLeisure"
           ,(org-agenda-compound-view "leisure")
           ((org-agenda-show-log '(closed clock))
            (org-agenda-files `(,(my/agenda-file "leisure.org")))))
          ,@(minimal-agenda-suite "Today" "t" "today" `((org-agenda-files ',(cons (my/agenda-file "today.org") org-agenda-files))))
          ,@(agenda-suite "Trial" "r" "test"
              `((org-agenda-files ',(list (my/agenda-file "test.org")))))
          ("L" "\tLog" agenda ""
           ((org-agenda-start-with-log-mode 'only)))
          ("m" . "\tMaintainence")
          ("md" "\tDone Tasks"
           ((org-ql-block '(and (not (tags "ARCHIVE"))
                                (done)
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
          ("c" "\tCalfw 2 weeks" cfw:open-org-2week-calendar "")
          ("C" . "\torg-agenda clocking views")
          ("Ch" "\torg-agenda history" agenda ""
           ((org-agenda-start-with-clockreport-mode nil)
            (org-agenda-start-with-log-mode 'only)
            (org-agenda-log-mode-items '(state clock closed))))
          ("Cc" "\torg-agenda clock-report" agenda ""
           ((org-agenda-start-with-clockreport-mode t)
            (org-agenda-start-with-log-mode 'clockcheck)))
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
          ("P" "\tPlan" agenda ""
           ((org-agenda-tag-filter-preset (quote ("+PLAN")))))
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
          ("E" . "\tExperimental")
          ("En" "\tNext Tasks List" tags-todo "-REFILE-HOLD-WAIT"
           ((org-agenda-skip-function 'my/show-next-tasks-and-standalone-tasks)
            (org-agenda-overriding-header "Next Tasks list")
            (org-agenda-sorting-strategy '(deadline-up))))
          ("EL" "\tLeaf Task List" tags-todo "-REFILE-HOLD-WAIT"
           ((org-agenda-skip-function 'my/show-leaf-tasks)
            (org-agenda-overriding-header "Next Tasks list")
            (org-agenda-finalize-hook '(org-agenda-add-separater-between-project))))
          ("Eo" "\tOffline" tags-todo "offline"
           ((org-tags-match-list-sublevels nil)))
          ("Eb" "\tBored" tags-todo "+short-grow"
           ((org-tags-match-list-sublevels nil))))))


(defun my/this-or-last-saturday ()
  (org-read-date nil nil
                 (if (string= "6" (format-time-string "%u"))
                     "."
                   "-sat")))

(defun my/last-or-last-last-saturday ()
  (org-read-date nil nil
                 (if (< 6 (string-to-number (format-time-string "%u")))
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

(advice-add #'org-agenda-follow-mode
            :before
            #'my/org-agenda-follow-mode)

(defun my/org-agenda-get-name ()
  (-> (rx "*Org Agenda("
          (group (+? anything))
          (or ")"
              (and
               ":"
               (one-or-more anything)))
          "*")
      (s-match-strings-all (buffer-name))
      (cadar)
      (assoc org-agenda-custom-commands)
      (cadr)))

(defun my/org-agenda-follow-mode ()
  (if org-agenda-follow-mode
      (advice-unadvice #'org-agenda-do-context-action)
    (pcase (my/org-agenda-get-name)
      ("\tReviews" (advice-add #'org-agenda-show
                               :override
                               #'my/org-agenda-show-review))
      ("\tReviews and Journals" (advice-add #'org-agenda-show
                                            :override
                                            #'my/org-agenda-show-journal))
      ("\tDev" (advice-add #'org-agenda-show
                            :override
                            #'my/org-agenda-show-project))
      ("\tMinimal" (advice-add #'org-agenda-show
                               :override
                               #'my/org-agenda-show-project))
      ("\tPlan" (advice-add #'org-agenda-show
                            :override
                            #'my/org-agenda-show-review))
      (_ (advice-unadvice #'org-agenda-show)))))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun my/org-agenda-show-project (&optional full-entry)
  "Display the Org file which contains the item at point.
      With prefix argument FULL-ENTRY, make the entire entry visible
      if it was hidden in the outline."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (org-narrow-to-subtree)
    (org-flag-subtree t)
    (call-interactively 'outline-show-branches)
    (org-hide-archived-subtrees (point-min) (point-max))
    (select-window win)))

(defun my/org-agenda-show-review (&optional full-entry)
  "Display the Org file which contains the item at point.
      With prefix argument FULL-ENTRY, make the entire entry visible
      if it was hidden in the outline."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (org-narrow-to-subtree)
    (org-flag-subtree t)
    (call-interactively 'org-show-entry)
    (org-hide-archived-subtrees (point-min) (point-max))
    (select-window win)))

(defun my/org-agenda-show-journal (&optional full-entry)
  "Display the Org file which contains the item at point.
      With prefix argument FULL-ENTRY, make the entire entry visible
      if it was hidden in the outline."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (org-narrow-to-subtree)
    (org-flag-subtree t)
    (call-interactively 'org-show-all)
    (org-hide-archived-subtrees (point-min) (point-max))
    (select-window win)))

(defun org-agenda-goto-disable-follow (&optional highlight)
  (interactive)
  (when org-agenda-follow-mode
    (org-agenda-follow-mode))
  (org-agenda-goto highlight))

(define-key org-agenda-mode-map (kbd "<tab>") #'org-agenda-goto-disable-follow)

(when my/puppet-p
  (advice-add #'org-agenda-redo-all
              :before
              #'org-save-all-org-buffers)
  (advice-add #'org-agenda-redo-all
              :before
              #'org-save-all-org-buffers)

  (run-at-time 60 nil #'org-save-all-org-buffers))

(provide 'my-org-agenda-commands)
;;; my-org-agenda-commands.el ends here
