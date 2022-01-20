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
;; Redef some of org-ql stuff
(load-file "~/.emacs.d/lisp/work-config/work-specific-org-project-redefs.el")
(require 'org-scan-tags-indent)

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
                 :template "* %?"))))))

(setq org-agenda-custom-commands
      '(("p" "prod"
         ((org-ql-block '(tags "refile")
                        ((org-ql-block-header "Refile tasks")))
          (org-ql-block '(and (todo "TODO" "ONE" "META" "META1" "EMPTY" "SEQ")
                              (my/top-level)
                              (property "DELAYED")
                              (org-time< (property "DELAYED") (org-matcher-time "<now>")))
                        ((org-ql-block-header "Previously Delayed")))
          (my/org-ql-stuck-projects nil
                                    ((org-ql-block-header "Stuck Projects")
                                     (org-ql-indent-levels t)))
          (my/org-ql-active-projects nil
                                     ((org-ql-block-header "Active Projects")
                                      (org-ql-indent-levels t)))
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-tag-filter-preset)
                   (org-agenda-skip-deadline-if-done t)
                   (org-agenda-skip-scheduled-if-done t)
                   (org-super-agenda-groups '((:name "The Plan" :tag "PLAN")
                                              (:name "Overdue" :and (:deadline past :log nil))
                                              (:name "Upcoming" :deadline future)
                                              (:name "Should do" :and (:scheduled past :log nil))
                                              (:name "Today" :time-grid t
                                                     :and (:not (:and (:not (:scheduled today)
                                                                            :not (:deadline today)))))))))))))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)

(setq org-refile-targets `((nil :maxlevel . 9)))

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

(defun gdb-stacktrace-to-org-table ()
  (interactive)
  (let ((regexp (rx (and line-start
                         "#"
                         ;; Stack Number
                         (group (+? digit))
                         (+ " ")
                         (optional
                          (and "0x"
                               (+ alnum)
                               " in "))
                         ;; Function Name
                         (group (+? nonl))
                         (optional " ")
                         "("
                         (*? nonl)
                         ") at "
                         ;; File name
                         (group
                          ;;"/"
                          (+? nonl))
                         ":"
                         ;; Line Number
                         (group
                          (+ digit))
                         line-end
                         ))))
    (beginning-of-buffer)
    (save-excursion
      (while (re-search-forward regexp nil 'noerror)
        (replace-match
         (let ((path (match-string 3)))
           (save-match-data
             (format "|\\1|\\2|%s:\\4|[[\\3::\\4][Link]]|"
                     (cond ((string-match (rx (and "/scratch/benson/tools"
                                                   (* digit)
                                                   "/llvm_cgt/llvm-project/"
                                                   (group
                                                    (+ nonl))))
                                          path)
                            (format "$LLVM_PROJECT/%s" (match-string 1 path)))
                           ((string-match (rx (and "/scratch/benson/tools"
                                                   (* digit)
                                                   "/"
                                                   (group
                                                    (+ nonl))))
                                          path)
                            (format "$SANDBOX/%s" (match-string 1 path)))
                           (t path))))))))
    (org-table-sort-lines nil ?N)
    (save-excursion
      (insert "|-\n|#|Function Name|File & Line Number|Link|\n|-\n")
      (end-of-buffer)
      (insert "\n|-"))
    (org-table-align)))


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
         ("C-c n t" . org-roam-dailies-capture-today)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
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
                                  "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
                                  ("Journal")))
          ("s" "Standup" plain "%?"
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
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
                          ("Journal" "%<%b %d, %Y>")))))

  (require 'org-roam-util)

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "Project")
     :templates
     '(("p" "project" plain ""
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}: %^{Description}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t))))

  (global-set-key (kbd "C-c n p") #'my/org-roam-find-project)

  (require 'my-org-roam-logger)

  (setq my/org-roam-logger-filter-fun (my/org-roam-filter-by-tag "Project"))
  (global-set-key (kbd "C-c n j") #'my/org-roam-logger-capture-current))

(provide 'work-org-stuff)
;;; work-org-stuff.el ends here
