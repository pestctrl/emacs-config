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
(require 'my-org-misc)
(require 'org-project)
(require 'emacs-custom-load-or-ask)
(use-package dash-functional)
(use-package ts)
(use-package peg)
(use-package org-super-agenda)
(use-package ov)
(require 'org-ql)
(require 'org-ql-custom-stuck-projects)
(require 'org-delay)
;; Redef some of org-ql stuff
(require 'work-org-ql)
(require 'org-scan-tags-indent)
(require 'work-org-autosync)

;; Useful org-capture window stuff
(require 'my-org-capture-shouldnt-mess-windows)

(require 'ob-shell)

(defvar org-ql-indent-levels nil)

(setq org-agenda-compact-blocks t)

(use-package doct)

(defun doct-pad-and-icon-recursive (element)
  (let* ((name (car element))
         (plist (cdr element))
         (icon (or (plist-get plist :icon) ""))
         (children (plist-get plist :children)))
    (setq plist (org-plist-delete plist :icon))
    (when children
      (-as-> children it
             (mapcar #'doct-pad-and-icon-recursive
                     it)
             (plist-put plist :children it)
             (setq plist it)))
    (cons (format "%s\t%s" icon name)
          plist)))

(defun doct-pad-and-icon-all (orig list)
  (funcall orig
           (mapcar #'doct-pad-and-icon-recursive
                   list)))

(advice-add #'doct :around #'doct-pad-and-icon-all)

(defvar org-notes-current-file nil)

(defun org-notes-find-file ()
  (when (or current-prefix-arg
            (not org-notes-current-file))
    (setq org-notes-current-file
          (read-file-name "Notes file? ")))
  (set-buffer (find-file-noselect org-notes-current-file))
  (end-of-buffer))

(ec/load-or-ask-dir 'my/work-org-folder "Where is the work-org root directory? ")

(setq org-capture-templates
      (doct `(("Todo"
               :icon ,(all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01)
               :keys "t"
               :file ,(expand-file-name "refile.org" my/work-org-folder)
               :template "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
              ("Charging Setup"
               :icon ,(all-the-icons-faicon "bolt" :face 'all-the-icons-blue :v-adjust 0.01)
               :keys "c"
               :file ,(expand-file-name "refile.org" my/work-org-folder)
               :template "* %?")
              ("Magit Save Branch"
               :keys "B"
               :file "/scratch/benson/old-git-branches.org"
               :template "* %c")
              ("Logging"
               :icon ,(all-the-icons-material "laptop" :face 'all-the-icons-lblue)
               :keys "g" :children
               (("Source location"
                 :icon ,(all-the-icons-faicon "code" :face 'all-the-icons-lgreen :v-adjust 0.01)
                 :keys "s"
                 :function org-notes-find-file
                 :template "* %?\n%a")
                ("Log entry"
                 :icon ,(all-the-icons-material "laptop" :face 'all-the-icons-blue)
                 :keys "l"
                 :function org-notes-find-file
                 :template "* %?")
                ("Timer event"
                 :icon ,(all-the-icons-material "timer" :face 'all-the-icons-red)
                 :keys "t" :clock-in t :clock-keep t
                 :function org-notes-find-file
                 :template "* %?")))
              ("Protocol"
               :keys "p"
               :file "~/org/refile.org"
               :template "* STUFF %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n#+begin_example\n%i\n#+end_example\n%?")
              ("Protocol Link"
               :keys "L"
               :file "~/org/refile.org"
               :template "* STUFF %? [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:"))))

(defun agenda-suite (name key tag &rest additional)
  (declare (indent 3))
  `((,key . ,(concat "\t" name))
    (,(concat key "c") ,(concat "\t" name " Compound View")
     ,(org-agenda-compound-view tag))
    (,(concat key "h") ,(concat "\t" name " Hold and Delay")
     ,(org-agenda-hold-view tag))
    (,(concat key "w") ,(concat "\t" name " Weekly Report")
     ,(org-agenda-weekly-view tag))
    ,@additional))


(defun org-agenda-weekly-view (tag)
  `((my/org-ql-stuck-projects ,tag
                              ((org-ql-block-header "Stuck Projects")
                               (org-ql-indent-levels t)))
    (my/org-ql-active-projects-plus-tasks ,tag
                                          ((org-ql-block-header "Active Projects")
                                           (org-ql-indent-levels t)))
    (agenda ""
            ((org-agenda-span (car (work/last-week-wednesday)))
             (org-agenda-start-day (cdr (work/last-week-wednesday)))
             (org-agenda-start-on-weekday 3)
             (org-agenda-show-log '(clock state))
             (org-agenda-skip-function (lambda ()
                                         (let ((tags (org-get-tags)))
                                           (unless (and (or (member ,tag tags)
                                                            (member "PLAN" tags))
                                                        (let ((delayed (org-entry-get (point) "DELAYED")))
                                                          (or (null delayed)
                                                              (org-time< delayed (org-matcher-time "<now>"))))
                                                        (not (member (org-get-todo-state) '("HOLD" "TICKLER"))))
                                             (outline-next-heading)))))))))

(defun work/last-week-wednesday ()
  (let ((num (+ 4 (string-to-number (format-time-string "%u")))))
    (cons (1+ num)
          (format "-%dd" num))))

(defun org-agenda-hold-view (tag)
  `((org-ql-block '(and (tags ,tag)
                        (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                        (property "DELAYED")
                        (org-time<= (org-entry-get (point) "DELAYED")
                                    (org-matcher-time "<now>")))
                  ((org-ql-block-header "Past Delayed Projects")))
    (org-ql-block '(and (tags ,tag)
                        (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                        (property "DELAYED")
                        (org-time> (org-entry-get (point) "DELAYED")
                                   (org-matcher-time "<now>")))
                  ((org-ql-block-header "Delayed projects")))
    (org-ql-block '(and (tags ,tag)
                        ;; TODO: What if "EMPTY" project was delayed?
                        ;; Different subclasses of invisible? Maybe should be
                        ;; core part of API.
                        (todo "TODO" "ONE" "META" "META1" "SEQ") ;; "EMPTY"
                        (not (property "DELAYED"))
                        (let* ((ts (opr/get-type-and-state))
                               (type (car ts))
                               (state (cdr ts)))
                          (and (eq type 'project)
                               (eq state 'invis))))
                  ((org-ql-block-header "Invisible projects")))
    (org-ql-block '(and (tags ,tag)
                        (or (todo "HOLD")
                            (descendants (todo "HOLD"))))
                  ((org-ql-block-header "Hold projects")
                   (org-ql-indent-levels t)))))

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
                   (org-use-tag-inheritance nil)))
    (agenda ""
            ((org-agenda-show-log '(closed))
             (org-agenda-skip-function (lambda ()
                                         (let ((tags (org-get-tags)))
                                           (unless (and (or (member ,tag tags)
                                                            (member "PLAN" tags))
                                                        (let ((delayed (org-entry-get (point) "DELAYED")))
                                                          (or (null delayed)
                                                              (org-time< delayed (org-matcher-time "<now>"))))
                                                        (not (member (org-get-todo-state) '("HOLD" "TICKLER"))))
                                             (outline-next-heading)))))
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

(setq org-agenda-span 'day)

(setq org-agenda-custom-commands
      `(("p" . "\tprod")
        ,@(agenda-suite "all" "pa" "prod")))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(setq org-agenda-files
      (list "~/org/refile.org"
            "~/org/all.org"))

(defun my/update-org-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (cons "~/org/refile.org"
              (cons "~/org/all.org"
                    (my/get-org-roam-files-by-tags '("Project" "active"))))))

(setq org-refile-targets `((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(require 'org-protocol)

;; (defun wait-mark-blocking-tasks (change-plist)
;;   (when (string= "WAIT"
;;                  (plist-get change-plist :to))
;;     (let ((ids '()))
;;       (unwind-protect
;;           (while
;;               (progn
;;                 (let ((id (org-id-get-with-outline-path-completion '((nil :maxlevel . 9)))))
;;                   (save-excursion
;;                     (org-id-goto id)
;;                     (org-entry-put (point) "WAIT_PREV_STATE" (org-get-todo-state))
;;                     (org-todo "NEXT"))
;;                   (push id ids))
;;                 (y-or-n-p "Add another heading?"))))
;;       (org-entry-put (point) "WAITING" (mapconcat #'concat ids ", ")))))

;; (add-hook 'org-trigger-hook
;;           #'wait-mark-blocking-tasks)

;; (defun unwait-unblock-tasks (change-plist)
;;   (when (string= "WAIT"
;;                  (plist-get change-plist :from))
;;     (-as-> (org-entry-get (point) "WAITING")
;;            it
;;            (split-string it ", ")
;;            (mapcar (lambda (id)
;;                      (save-excursion
;;                        (org-id-goto id)
;;                        (org-todo (org-entry-get (point) "WAIT_PREV_STATE"))
;;                        (org-entry-delete (point) "WAIT_PREV_STATE")))
;;                    it))))

;; (add-hook 'org-trigger-hook
;;           #'unwait-unblock-tasks)


;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (use-package org-roam
;;   :after org
;;   :hook
;;   (after-init . org-roam-mode)
;;   :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
;;   :custom
;;   (org-roam-directory "~/org/org-roam/")
;;   :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n f" . org-roam-find-file)
;;                ("C-c n g" . org-roam-show-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))))

;; (org-roam-mode t)

;; (use-package el-patch
;;   :straight (:host github
;;                    :repo "raxod502/el-patch"
;;                    :branch "develop"))

;; (eval-when-compile
;;   (require 'el-patch))

;; (use-package deft
;;   :after org
;;   :bind
;;   ("C-c n d" . deft)
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory "~/org/org-roam/")
;;   :config/el-patch
;;   (defun deft-parse-title (file contents)
;;     "Parse the given FILE and CONTENTS and determine the title.
;; If `deft-use-filename-as-title' is nil, the title is taken to
;; be the first non-empty line of the FILE.  Else the base name of the FILE is
;; used as title."
;;     (el-patch-swap (if deft-use-filename-as-title
;;                        (deft-base-filename file)
;;                      (let ((begin (string-match "^.+$" contents)))
;;                        (if begin
;;                            (funcall deft-parse-title-function
;;                                     (substring contents begin (match-end 0))))))
;;                    (org-roam--get-title-or-slug file))))

(require 'org-table-convert)

(setq show-tab-bar-new-tab t)

(defmacro org-babel-comint-with-output (meta &rest body)
  "Only split with \"^$ *\""
  (declare (indent 1))
  (let ((buffer (nth 0 meta))
	    (eoe-indicator (nth 1 meta))
	    (remove-echo (nth 2 meta))
	    (full-body (nth 3 meta)))
    `(org-babel-comint-in-buffer ,buffer
       (let* ((string-buffer "")
	          (comint-output-filter-functions
	           (cons (lambda (text) (setq string-buffer (concat string-buffer text)))
		             comint-output-filter-functions))
	          dangling-text)
	     ;; got located, and save dangling text
	     (goto-char (process-mark (get-buffer-process (current-buffer))))
	     (let ((start (point))
	           (end (point-max)))
	       (setq dangling-text (buffer-substring start end))
	       (delete-region start end))
	     ;; pass FULL-BODY to process
	     ,@body
	     ;; wait for end-of-evaluation indicator
	     (while (progn
		          (goto-char comint-last-input-end)
		          (not (save-excursion
			             (and (re-search-forward
			                   (regexp-quote ,eoe-indicator) nil t)
			                  (re-search-forward
			                   comint-prompt-regexp nil t)))))
	       (accept-process-output (get-buffer-process (current-buffer)))
	       ;; thought the following this would allow async
	       ;; background running, but I was wrong...
	       ;; (run-with-timer .5 .5 'accept-process-output
	       ;; 		 (get-buffer-process (current-buffer)))
	       )
	     ;; replace cut dangling text
	     (goto-char (process-mark (get-buffer-process (current-buffer))))
	     (insert dangling-text)

	     ;; remove echo'd FULL-BODY from input
	     (when (and ,remove-echo ,full-body
		            (string-match
		             (replace-regexp-in-string
		              "\n" "[\r\n]+" (regexp-quote (or ,full-body "")))
		             string-buffer))
	       (setq string-buffer (substring string-buffer (match-end 0))))
	     (split-string string-buffer "^$ *")))))

(require 'ob-plantuml)
(use-package plantuml-mode)
(setq org-plantuml-jar-path "/home/a0487752/bin/plantuml")

(use-package gnuplot-mode)
(use-package gnuplot)
(add-to-list 'exec-path
             "/scratch/benson/gnuplot-5.4.2/bin/bin")

(setenv "GNUPLOT_DRIVER_DIR" "/scratch/benson/gnuplot-5.4.2/bin/libexec/gnuplot/5.4")

(setq straight-vc-git-default-protocol 'ssh)

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (expand-file-name "~/org/org-roam"))
  (org-roam-use-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n T" . org-roam-dailies-goto-today)
         ("C-c n F" . my/org-roam-find-daily)
         ("C-c n t" . org-roam-dailies-capture-today)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . my/org-roam-logger-capture-current)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :commands my/get-org-roam-files-by-tags
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("j" "Journal" entry "* %<%H:%M> %?"
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                                  ("Journal")))
          ("E" "Entry Interrupt" entry (file "~/org/templates/entry-interrupt.org")
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                                  ("Journal")))
          ("e" "Exit Interrupt" entry (file "~/org/templates/exit-interrupt.org")
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                                  ("Journal")))
          ("s" "Standup" plain "%?"
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B>:\n"
                                  ("Standup Notes for %u")))))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("j" "Journal" entry "* %<%H:%M> %?"
           :unnarrowed t
           :target
           (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n"
                          ("Journal" "%<%b %d, %Y>")))
          ("J" "Journal with source" entry "* %<%H:%M> %?\n:PROPERTIES:\n:LOCATION: %a\n:END:"
           :unnarrowed t
           :target
           (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n"
                          ("Journal" "%<%b %d, %Y>")))))

  (require 'org-roam-util)

  (defvar my/project-templates
    '(("p" "project" plain ""
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}: %^{Description}\n#+category: ${title}\n#+filetags: Project active")
       :unnarrowed t)
      ("s" "sandbox" plain ""
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}: %^{Description}\n#+category: ${title}\n#+filetags: Project active\n#+PROPERTY: header-args:bash :dir /scratch/benson/sandbox/${title} :results output verbatim :exports results :noweb yes"))))

  (defun my/org-roam-find-projects ()
    (interactive)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find nil nil
                        (my/org-roam-filter-by-tag '("Project"))
                        nil :templates my/project-templates))

  (defun my/org-roam-find-active-projects ()
    (interactive)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find nil nil
                        (my/org-roam-filter-by-tag '("Project" "active"))
                        nil :templates my/project-templates))

  (defun my/get-org-roam-files-by-tags (tags)
    (->>
     (org-roam-node-list)
     (remove-if-not (my/org-roam-filter-by-tag tags))
     (mapcar #'org-roam-node-file)
     (-uniq)))

  (defun org-agenda-insert-breaks-between (str1 str2)
    (let ((r (rx line-start
                 "  "
                 (+ (not whitespace))
                 (+ whitespace)
                 (not (any whitespace "."))))
          begin end)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward str1 nil t)
        (setq begin (point))
        (re-search-forward str2 nil t)
        (setq end (point))
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (re-search-forward r nil t)
          (while (re-search-forward r nil t)
            (beginning-of-line)
            (insert "  " (make-string (- (window-width) 3) ?-) "\n")
            (next-line))))))

  (defun org-agenda-break-up-subtrees ()
    (let ((buffer-read-only nil))
      (org-agenda-insert-breaks-between
       "^Stuck Projects"
       "^Active Projects")
      (org-agenda-insert-breaks-between
       "^Active Projects"
       (rx line-start
           (or
            "Monday"
            "Tuesday"
            "Wednesday"
            "Thursday"
            "Friday"
            "Saturday"
            "Sunday")))))

  (add-hook 'org-agenda-finalize-hook #'org-agenda-break-up-subtrees)

  (global-set-key (kbd "C-c n p") #'my/org-roam-find-active-projects)
  (global-set-key (kbd "C-c n P") #'my/org-roam-find-projects)

  (require 'my-org-roam-logger)

  (setq my/org-roam-logger-filter-fun nil ;; (my/org-roam-filter-by-tag '("Project" "active"))
        )

  (use-package consult-org-roam
    :demand t
    :commands (my/org-roam-find-daily)
    :config
    (require 'org-roam-util)

    (defun consult-org-roam-file-find (arg)
      "Find org-roam node with preview, if ARG open in other window."
      (interactive "P")
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (symbol-function 'consult-org-roam-node-read)))
        (let ((other-window (if arg t nil)))
          (org-roam-node-find other-window nil #'consult-org-roam--node-file-p))))

    (defun my/org-roam-find-daily ()
      (interactive)
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (symbol-function 'consult-org-roam-node-read)))
        (org-roam-node-find nil nil
                            (my/org-roam-filter-by-tag "dailies")
                            (lambda (x y)
                              (string-lessp (org-roam-node-file (cdr y))
                                            (org-roam-node-file (cdr x)))))))))

(advice-add #'org-agenda-redo
            :around
            (lambda (orig-fun &rest args)
              (save-window-excursion
                (apply orig-fun args))))

(setq org-agenda-hide-tags-regexp
      (rx (or "prod"
              (and symbol-start "_" (+ nonl) "_" symbol-end)
              "Project" "active")))

(defun ti/generate-org-exit-interrupt ()
  (-->
   '(
     "Update org-agenda"
     "Close Tabs"
     "Clean Coffee Cup (if used)"
     )
   (if (not (string= "Friday" (format-time-string "%A")))
       it
     (append
      it
      '(
        "Fill out TICA information"
        "Close Emacs and Putty"
        )))
   ))

(defun ti/show-jira-items ()
  (interactive)
  (shell-command "jira issue list --plain -a'Benson Chu' -s~Closed | grep -v 'Done$'"))

(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(setq org-latex-src-block-backend 'listings)

(provide 'work-org-stuff)
;;; work-org-stuff.el ends here
