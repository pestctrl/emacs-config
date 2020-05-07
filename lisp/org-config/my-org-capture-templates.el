;;; my-org-capture-templates.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-04-29 21:30]

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
(require 'my-org-agenda-files)

(setq org-capture-templates
      `(("t" ,(format "%s\tTodo" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
         entry (file ,(my/agenda-file "refile.org"))
         "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
        ("r" ,(format "%s\tReviews" (all-the-icons-faicon "share" :face 'all-the-icons-lblue)))
        ("ra" "\tAutomatic Review" entry (file+function ,(my/org-file "entries/reviews.gpg") setup-automatic-review)
         (file ,(my/org-file "templates/weekly-review.org")))
        ("rm" "\tManual Review" entry (file+function ,(my/org-file "entries/reviews.gpg") setup-manual-review)
         (file ,(my/org-file "templates/weekly-review.org")))
        ("ro" "\tReorient" entry (file ,(my/org-file "entries/reviews.gpg"))
         (file ,(my/org-file "templates/reorient.org")))
        ;; ("rt" "Review Task" entry (file+headline ,(my/org-file "entries/reviews.gpg") "Tasks")
        ;;  "* TODO %?")
        ;; ("d" "\tDream" entry (file+olp+datetree ,(my/org-file "entries/dream.org"))
        ;;  "* %?")
        ("D" "\tDistracted" entry (file ,(my/agenda-file "dev.org"))
         "* TODO %?" :clock-in t :clock-resume t)
        ("T" "\tNew Task" entry (file ,(my/agenda-file "dev.org"))
         "* TODO %?" :clock-in t :clock-keep t)
        ("m" ,(format "%s\tMoney" (all-the-icons-material "attach_money" :face 'all-the-icons-lgreen)))
        ("mc" ,(format "%s\tCredit Card" (all-the-icons-faicon "credit-card" :face 'all-the-icons-blue))
         plain (file ,(my/org-file "entries/finances/ledger.ledger"))
         (file ,(my/org-file "templates/credit.ledger")) :unnarrowed t :empty-lines 1)
        ("mg" "\tGeneral" plain (file ,(my/org-file "entries/finances/ledger.ledger"))
         (file ,(my/org-file "templates/basic.ledger")) :unnarrowed t :empty-lines 1)
        ("c" "\tRecord Comms Message" entry (file+olp+datetree ,(my/agenda-file "datetree.org"))
         "* TODO %?")
        ("e" "\tEmacs config snippet" entry (file+headline "~/.emacs.d/config-base.org" "New")
         "* %^{Title}\n#+begin_src emacs-lisp\n %?\n#+end_src")
        ("j" "\tJournal")
        ("jd" "\tDecision template" entry (file+olp+datetree ,(my/org-file "entries/journal.gpg"))
         (file ,(my/org-file "templates/decide.org")))
        ("je" "\tJournal Entry" entry (file+olp+datetree ,(my/org-file "entries/journal.gpg"))
         "* %<%R> %?")
        ;; ("jp" "Plan your day" entry (file+olp+datetree ,(my/org-file "entries/journal.gpg"))
        ;;  (file ,(my/org-file "templates/daily-plan.org")))
        ("jp" "\tProgramming Interview Prep Journal" entry (file+olp+datetree ,(my/org-file "entries/journal.gpg"))
         "* ")
        ("C" "\tCreate checklist")
        ("Cc" "\tConference Via Bus" entry (file ,(my/agenda-file "dev.org"))
         (file ,(my/org-file "checklists/conference.org"))
         :conference/airplane nil)
        ("Cm" "\tMorning routine" entry (file ,(my/org-file "entries/routines.org"))
         (file ,(my/org-file "checklists/mornings.org")))
        ("Cn" "\tNightly routine" entry (file ,(my/org-file "entries/routines.org"))
         (file ,(my/org-file "checklists/nights.org")))
        ;; ("y" "Elfeed YouTube" entry (file+olp ,(my/agenda-file "dev.org") "rewards" "Videos")
        ;;  "* TODO %(identity elfeed-link-org-capture)")
        ("p" "\tProtocol" entry (file ,(my/agenda-file "refile.org"))
         "* STUFF %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n#+begin_example\n%i\n#+end_example\n%?")
        ("L" "\tProtocol Link" entry (file ,(my/agenda-file "refile.org"))
         "* STUFF %? [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:")
        ("l" "\tAdd to lists conveniently")
        ("lc" "\tCringe" entry (file ,(my/org-file "entries/cringe.gpg")) "* %?")
        ("lm" "\tMental Model" entry (file ,(my/org-file "entries/mental_models.gpg")) "* %?")
        ("li" "\tImportant Information" entry (file ,(my/org-file "entries/important.gpg")) "* %?")))

(defvar yearly-theme "Thought")

;; Functions required by template
(defun get-journal-entries-from (start-date end-date)
  (let ((string "")
        match)
    (save-window-excursion
      (switch-to-buffer (find-file-noselect "~/MEGA/org/entries/journal.gpg"))
      (goto-char (point-min))
      (while (setq match 
                   (re-search-forward
                    "^\\*\\*\\* \\(2[0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\w+$" nil t))
        (let ((date (match-string 1)))
          (when (and (org-time< start-date date)
                     (or (not end-date) (org-time< date end-date)))
            (org-narrow-to-subtree)
            (setq string (concat string "\n" (buffer-string)))
            (widen))))
      (not-modified)
      (kill-buffer))
    string))

;; Setup stuff, called by capture template
(defun get-last-review-date ()
  (save-window-excursion
    (let ((auto-insert-alist nil))
      (set-buffer (find-file-noselect "~/.emacs.d/last-review.el")))
    (let ((res (buffer-string)))
      (kill-buffer)
      res)))

(defun output-incomplete-date ()
  (save-window-excursion
    (let ((auto-insert-alist nil))
      (switch-to-buffer (find-file-noselect "~/.emacs.d/review-incomplete.el")))
    (erase-buffer)
    (insert (org-read-date nil nil ""))
    (save-buffer)
    (kill-buffer)))

(defvar my/review-date-old nil)
(defun setup-automatic-review ()
  ;; Check for older review
  (when (and (file-exists-p "~/.emacs.d/review-incomplete.el")
             (y-or-n-p "Woah, we found an incomplete review. Would you like to use that date as the start date? "))
    (shell-command "mv ~/.emacs.d/review-incomplete.el ~/.emacs.d/last-review.el"))
  ;; Setup current review
  (let* ((date (org-read-date nil nil (get-last-review-date)))
         (week (format "%02d" 
                       (org-days-to-iso-week
                        (org-time-string-to-absolute date)))))
    (output-incomplete-date)
    (setq my/review-date-old date)
    (setq my/review-visibility-level 6)
    (org-capture-put :start-date date)
    (org-capture-put :start-week week)
    (goto-char (point-min))
    (re-search-forward "Reviews")))

(defun aux-get-week-string ()
  (let ((last (plist-get org-capture-plist :start-week))
        (this (format-time-string "%V")))
    (if (string= this last)
        (concat "Week " this)
      (concat "Weeks " last "-" this))))

(defun setup-manual-review ()
  ;; Read the date manually, and setup review accordingly
  (let* ((org-read-date-prefer-future nil)
         (date (org-read-date))
         (week (format "%02d" 
                       (org-days-to-iso-week
                        (org-time-string-to-absolute date)))))
    (output-incomplete-date)
    (setq my/review-date-old date)
    (setq my/review-visibility-level 6)
    (org-capture-put :start-date date)
    (org-capture-put :start-week week)
    (goto-char (point-min))
    (re-search-forward "Reviews")))

;; Teardown stuff, called through hooks
(defun finalize-review ()
  "Save a copy of the weekly agenda, and write the current date
  as the most current weekly review."
  (let ((desc (plist-get org-capture-current-plist :description)))
    (when (and (string= desc "Automatic Review")
               my/review-date-old)
      (my/save-agenda-week my/review-date-old)
      (shell-command "rm ~/.emacs.d/review-incomplete.el")
      (save-window-excursion
        (switch-to-buffer (find-file-noselect "~/.emacs.d/last-review.el"))
        (erase-buffer)
        (insert (org-read-date nil nil ""))
        (save-buffer)
        (kill-buffer)
        "")
      (setq my/review-date-old nil))))
(add-hook 'org-capture-before-finalize-hook 'finalize-review)

(defun clear-out-review-files ()
  (when (file-exists-p "~/.emacs.d/review-incomplete.el")
    (shell-command "rm ~/.emacs.d/review-incomplete.el")))
(add-hook 'org-capture-after-finalize-hook #'clear-out-review-files)
(defconst my/org-agenda-snapshot-pdf-filename "~/MEGA/org/entries/review/%Y_%m_%d.pdf")
(defconst my/org-agenda-snapshot-html-filename "~/MEGA/org/entries/review/%Y_%m_%d.html")

(defun my/agenda-dates (start &optional end)
  (interactive (list (let ((org-read-date-prefer-future nil))
                       (org-read-date))))
  (when-let (buf (get-buffer "*Org Agenda(a)*"))
    (kill-buffer buf))
  (or end (setq end (org-read-date nil nil ".")))
  (let* ((span (- (org-time-string-to-absolute end)
                  (org-time-string-to-absolute start)))
         (org-agenda-archives-mode t)
         (org-agenda-start-with-log-mode '(closed clock))
         (org-agenda-start-on-weekday nil)
         (org-agenda-start-day start)
         (org-agenda-span span))
    (org-agenda-list nil)
    (put 'org-agenda-redo-command 'org-lprops
         `((org-agenda-archives-mode t)
           (org-agenda-start-with-log-mode '(closed clock))
           (org-agenda-start-on-weekday nil)
           (org-agenda-start-day ,start)
           (org-agenda-span ,span)))))

;; (my/agenda-dates "2019-07-14")

(defun my/save-agenda-week (start &optional end)
  (interactive (list (let ((org-read-date-prefer-future nil))
                       (org-read-date))))
  (let ((end (or end (org-read-date nil nil "."))))
    (save-window-excursion
      (my/agenda-dates start end)
      (org-agenda-write (format-time-string my/org-agenda-snapshot-pdf-filename (org-time-string-to-time end)))
      (org-agenda-write (format-time-string my/org-agenda-snapshot-html-filename (org-time-string-to-time end))))))

;; (my/save-agenda-week "2019-06-03" "2019-08-18")

;; (my/save-agenda-week "2019-07-14")

(defun my/png-ize-buffer (buffer name-without-ext)
  (with-current-buffer (htmlize-buffer buffer)
    (write-file (my/agenda-file (concat name-without-ext ".html"))))
  (shell-command (format "wkhtmltoimage %s %s"
                         (my/agenda-file (concat name-without-ext ".html"))
                         (my/agenda-file (concat name-without-ext ".png")))))

(defun my/write-agendas-for-review ()
  (interactive)
  (let* ((org-agenda-sticky nil)
         (dev (save-excursion 
                (org-agenda nil "d")
                (current-buffer)))
         (week (save-excursion
                 (org-agenda nil "d")
                 (current-buffer))))
    (my/png-ize-buffer dev "org-agenda")
    (my/png-ize-buffer week "org-week")))

(defun my/org-add-tag (tag)
  (org-set-tags (cons tag (org-get-tags nil t))))

(defun org-board-add-offline-tag (&rest args)
  (my/org-add-tag "offline"))

(advice-add #'org-board-archive :after
            #'org-board-add-offline-tag)

(defun my/org-board-prompt ()
  (let ((desc (plist-get org-capture-current-plist :description)))
    (when (and (not org-note-abort)
               (string= desc "\tProtocol Link")
               (y-or-n-p "Do you want to archive the page? "))
      (call-interactively #'org-board-archive))))

(add-hook 'org-capture-before-finalize-hook 'my/org-board-prompt)

;; New Note-taking capture template
(defvar org-notes-current-file nil)

(add-to-list 'org-capture-templates
             '("n" "\tNotes" entry (function org-notes-find-file)
               "* %A\n%?"))

(defun org-notes-find-file ()
  (when (or current-prefix-arg
            (not org-notes-current-file))
    (setq org-notes-current-file
          (read-file-name "Notes file? ")))
  (find-file org-notes-current-file)
  (end-of-buffer))

(provide 'my-org-capture-templates)
;;; my-org-capture-templates.el ends here
