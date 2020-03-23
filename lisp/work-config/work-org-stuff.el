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
(require 'org-ql)
(require 'org-ql-custom-stuck-projects)

(defvar org-ql-indent-levels nil)

(setq org-agenda-compact-blocks t)

(setq org-capture-templates
      '(("t" "Todo" entry (file "/home/a0487752/org/refile.org")
         "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
        ("c" "Charging Setup" entry (file "/home/a0487752/org/refile.org")
         "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")))

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
          (org-ql-block '(todo "WAIT")
                        ((org-ql-block-header "Waiting tasks")))
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

(defun wait-mark-blocking-tasks (change-plist)
  (when (string= "WAIT"
                 (plist-get change-plist :to))
    (let ((ids '()))
      (unwind-protect
          (while
              (progn
                (let ((id (org-id-get-with-outline-path-completion '((nil :maxlevel . 9)))))
                  (save-excursion
                    (org-id-goto id)
                    (org-entry-put (point) "WAIT_PREV_STATE" (org-get-todo-state))
                    (org-todo "NEXT"))
                  (push id ids))
                (y-or-n-p "Add another heading?"))))
      (org-entry-put (point) "WAITING" (mapconcat #'concat ids ", ")))))

(add-hook 'org-trigger-hook
          #'wait-mark-blocking-tasks)

(defun unwait-unblock-tasks (change-plist)
  (when (string= "WAIT"
                 (plist-get change-plist :from))
    (-as-> (org-entry-get (point) "WAITING")
           it
           (split-string it ", ")
           (mapcar (lambda (id)
                     (save-excursion
                       (org-id-goto id)
                       (org-todo (org-entry-get (point) "WAIT_PREV_STATE"))
                       (org-entry-delete (point) "WAIT_PREV_STATE")))
                   it))))

(add-hook 'org-trigger-hook
          #'unwait-unblock-tasks)


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


(provide 'work-org-stuff)
;;; work-org-stuff.el ends here
