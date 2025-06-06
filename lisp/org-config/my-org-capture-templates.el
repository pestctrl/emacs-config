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
(require 'cl)

(require 'org-protocol)
(require 'my-org-agenda-files)
(require 'my-org-board)
(require 'org-capture)

(require 'org-capture-roam-refile)
(define-key org-capture-mode-map (kbd "C-c M-w") #'org-capture-roam-refile)

;; Defines 3 things:
;; - #'setup-automatic-review
;; - #'finalize-review
;; - #'clear-out-review-files
;; - #'review-hide-journal-entries
(require 'my-org-weekly-review)

;; Provides #'org-notes-find-file
(require 'org-capture-notes)

(defun warn-active-capture-template ()
  (let ((capture-buffers
         (->> (buffer-list)
              (remove-if-not
               (lambda (x)
                 (with-current-buffer x
                   (string-match-p "^CAPTURE-.*" (buffer-name))))))))
    (if (zerop (length capture-buffers))
        t
      (pop-to-buffer (car capture-buffers))
      (yes-or-no-p "Active capture templates exist; exit anyway? "))))

(add-hook 'kill-emacs-query-functions
          #'warn-active-capture-template)

(defvar my/org-protocol-capture-skip-archive nil)

(defun my/org-protocol-capture-skip-archive ()
  (interactive)
  (setq my/org-protocol-capture-skip-archive
        (not my/org-protocol-capture-skip-archive)))

(defun my/org-board-prompt ()
  (let ((desc (plist-get org-capture-current-plist :description)))
    (when (and (not org-note-abort)
               (string= desc "\tProtocol Link")
               (not my/org-protocol-capture-skip-archive)
               (y-or-n-p "Do you want to archive the page? "))
      (call-interactively #'org-board-archive))))

(defun org-click-check (click)
  (interactive "e")
  (let* ((posn     (event-start click))
	     (click-pt (posn-point posn))
	     (window   (posn-window posn)))
    (deactivate-mark)
    (select-window window)
    (goto-char click-pt)
    (when (and (org-at-item-p)
               (or (member (char-after) (list (string-to-char "[")
                                              (string-to-char "]")))
                   (and (member (char-after) (mapcar #'string-to-char
                                                     (list "X" " ")))
                        (eq (char-before) (string-to-char "[")))))
      (let ((p (point)))
        (org-toggle-checkbox)
        (goto-char p)))))

(define-key org-capture-mode-map (kbd "<mouse-1>") #'org-click-check)

(define-key org-capture-mode-map (kbd "S-<return>")
  #'(lambda () (interactive) (org-toggle-checkbox) (next-line)))

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

(defun place-in-current-tree ()
  (org-clock-goto)
  (org-up-heading-safe))

(defun org-plan-goto ()
  (let ((buffer (find-file-noselect (my/agenda-file "plan.org"))))
    (set-buffer buffer)
    (beginning-of-buffer)
    (re-search-forward "* ETERNAL The Plan")
    (org-narrow-to-subtree)
    (let ((today-plan (format-time-string "\\*\\* [A-z]+ \\[[0-9/]+\\] Plan for \\[%Y-%m-%d %a\\]")))
      (when (re-search-forward today-plan nil t)
        (org-narrow-to-subtree)
        (org-show-all)
        (end-of-buffer)
        (when (save-excursion
                (beginning-of-line)
                (not (looking-at-p "- \\[ \\] $")))
          (org-insert-item 'checkbox))
        (display-buffer buffer)
        (user-error "Already have a plan today!")))))

(defun my/org-find-journal ()
  (let* ((files
          (directory-files (my/org-file "journals/")
                           nil "^[^\\.]"))
         (journal (ivy-completing-read "Which journal? " files)))
    (when (not (file-name-extension journal))
      (setq journal (concat journal "")))
    (find-file (my/org-file (format "journals/%s" journal)))
    (outline-next-heading)))

(defun my/rent-next-month ()
  (if (<= 15 (ts-day (ts-now)))
      (format-time-string "%B"
                          (time-convert (time-add (current-time)
                                                  (days-to-time 16))))
    (format-time-string "%B")))

(setq org-capture-templates
      (doct `(("Tasks" :keys "t" :children
               (("New Refile Task"
                 :keys "t"
                 :prepend t
                 :icon ,(all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01)
                 :file ,(my/agenda-file "refile.org")
                 :template "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
                ("New Puppet Refile Task"
                 :keys "p"
                 :icon ,(all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01)
                 :file ,(my/agenda-file "puppet_refile.org")
                 :template "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
                ("Task in the same tree"
                 :keys "s"
                 :function place-in-current-tree
                 :template "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
                ("Distracted"
                 :file ,(my/agenda-file "dev.org")
                 :keys "d" :clock-in t :clock-resume t
                 :template "* TASK %?")
                ("Working on this task now" :file ,(my/agenda-file "dev.org")
                 :keys "n" :clock-in t :clock-keep t
                 :template "* TASK %?\nSCHEDULED: %T")
                ("Do it today" :file ,(my/agenda-file "prod.org") :olp ("Short")
                 :keys "T" :template "* TASK %?\nSCHEDULED: %T")))
              ("Reviews" :keys "r"
               :icon ,(all-the-icons-faicon "share" :face 'all-the-icons-lblue)
               :children
               (("Automatic Review"
                 :keys "a"
                 :file ,(my/org-file "reviews.gpg")
                 :function setup-automatic-review
                 :before-finalize finalize-review
                 :after-finalize clear-out-review-files
                 :hook review-hide-journal-entries
                 :template-file ,(my/org-file "templates/weekly-review.org"))
                ("Manual Review"
                 :keys "m"
                 :file ,(my/org-file "reviews.gpg")
                 :function setup-manual-review
                 :template-file ,(my/org-file "templates/weekly-review.org"))
                ("Reorient"
                 :keys "o"
                 :file ,(my/org-file "reviews.gpg")
                 :template-file ,(my/org-file "templates/reorient.org"))))
              ("Finances" :keys "f"
               :icon ,(all-the-icons-material "attach_money" :face 'all-the-icons-lgreen)
               :children
               (("Credit Card"
                 :keys "c"
                 :icon ,(all-the-icons-faicon "credit-card" :face 'all-the-icons-blue)
                 :file ,(my/plaintext-file "ledger-finance/ledger.ledger")
                 :unnarrowed t :empty-lines 1 :type plain
                 :template-file ,(my/org-file "templates/credit.ledger"))
                ("General" :keys "g"
                 :unnarrowed t :empty-lines 1 :type plain
                 :file ,(my/plaintext-file "ledger-finance/ledger.ledger")
                 :template-file ,(my/org-file "templates/basic.ledger"))
                ("Reconciliation" :keys "r"
                 :file ,(my/org-file "reviews.gpg")
                 :olp ("Reviews")
                 :template-file ,(my/org-file "templates/finance-recon.org"))
                ("Rent" :keys "R"
                 :unnarrowed t :empty-lines 1 :type plain
                 :function (lambda () (org-capture-put :month-rent (my/rent-next-month)) (end-of-buffer))
                 :file ,(my/plaintext-file "ledger-finance/ledger.ledger")
                 :template-file ,(my/org-file "templates/rent.ledger"))))
              ("Record Comms Message"
               :file ,(my/agenda-file "datetree.org")
               :keys "C"
               :datetree t
               :template "* TODO %?")
              ("Emacs config snippet" :keys "e"
               :file "~/.emacs.d/config-base.org"
               :headline "New"
               :template "* %^{Title}\n#+begin_src emacs-lisp\n %?\n#+end_src")
              ("Journal" :keys "j" :children
               (("Decision Template"
                 :file ,(my/org-file "journal.gpg")
                 :datetree t
                 :keys "d"
                 :template-file ,(my/org-file "templates/decide.org"))
                ("Journal Entry" :keys "e"
                 :file ,(my/org-file "journal.gpg")
                 :datetree t
                 :template "* %<%R> %?")
                ("New Journal System" :keys "j"
                 :function my/org-find-journal
                 :datetree t
                 :template "* %U\n%?")
                ("Puppet" :keys "p"
                 :file ,(my/org-file "puppet.gpg")
                 :template "* %T \n%?")
                ("Programming Interview Prep Journal" :keys "P"
                 :file ,(my/org-file "journal.gpg")
                 :datetree t
                 :template "* ")))
              ("Create checklist" :keys "c" :children
               (("Travel Via Bus"
                 :keys "b"
                 :file ,(my/agenda-file "dev.org")
                 :template-file ,(my/org-file "checklists/conference.org")
                 :conference/airplane nil)
                ("Travel Via Plane"
                 :keys "p"
                 :file ,(my/agenda-file "dev.org")
                 :template-file ,(my/org-file "checklists/conference.org")
                 :conference/airplane nil)
                ("Work Travel"
                 :keys "w"
                 :file ,(my/agenda-file "dev.org")
                 :template-file ,(my/org-file "templates/work_travel_checklist.org"))
                ("Morning routine"
                 :keys "m"
                 :file ,(my/org-file "random/routines.org")
                 :template-file ,(my/org-file "templates/morning-routine.org"))
                ("Night routine"
                 :keys "n"
                 :file ,(my/org-file "random/routines.org")
                 :template-file ,(my/org-file "templates/evening-routine.org"))
                ))
              ("Protocol"
               :keys "p"
               :prepend t
               :file ,(my/agenda-file "incoming.org")
               :template "* STUFF %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n#+begin_example\n%i\n#+end_example\n%?")
              ("Protocol Link"
               :keys "L"
               :prepend t
               :file ,(my/agenda-file "incoming.org")
               :before-finalize my/org-board-prompt
               :template "* STUFF %? [[%:link][%:description]]\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:")
              ("Add to lists conveniently" :keys "l" :children
               (("Plan" :keys "p" :children
                 (("Plan week" :keys "w"
                   :file ,(my/agenda-file "plan.org")
                   :headline "The Plan"
                   :template-file ,(my/org-file "templates/weekly-plan.org"))
                  ("Plan your day" :keys "p"
                   :file ,(my/agenda-file "plan.org")
                   :function org-plan-goto
                   :template-file ,(my/org-file "templates/daily-plan.org"))
                  ("Plan specific day" :keys "d"
                   :file ,(my/agenda-file "plan.org")
                   :function org-plan-goto
                   :template-file ,(my/org-file "templates/daily-plan-prompt.org"))))
                ("Cringe" :keys "c"
                 :file ,(my/org-file "cringe.gpg")
                 :template "* %?")
                ;; ("Mental Model" :keys "m"
                ;;  :file ,(my/org-file "mental_models.gpg")
                ;;  :template "* %?")
                ("Important Information"
                 :keys "i"
                 :file ,(my/org-file "important.gpg")
                 :template "* %?")))
              ("Logging"
               :icon ,(all-the-icons-material "add_to_queue" :face 'all-the-icons-lblue)
               :keys "g" :children
               (("Source location"
                 :icon ,(all-the-icons-faicon "code" :face 'all-the-icons-lgreen :v-adjust 0.01)
                 :keys "s"
                 :function org-notes-find-file
                 :template "* %?\n%a")
                ("Log entry"
                 :icon ,(all-the-icons-material "laptop" :face 'all-the-icons-lblue)
                 :keys "l"
                 :function org-notes-find-file
                 :template "* %?")
                ("Timer event"
                 :icon ,(all-the-icons-material "timer" :face 'all-the-icons-lred)
                 :keys "t" :clock-in t :clock-keep t
                 :function org-notes-find-file
                 :template "* %?"))))))


(defvar themes-file "20200605172953_roll_dice_what_to_do.org")

(defun read-short-term-theme ()
  (save-excursion
    (let (filename)
      ;; If org-roam is not loaded, or our themes file is gone
      (if (or (not (boundp 'org-roam-directory))
              (not (file-exists-p
                    (setq filename
                          (expand-file-name themes-file org-roam-directory)))))
          ;; Just read a string
          (read-string "What's the theme for today? ")
        ;; Otherwise, find that file, and do a completing read
        (let* ((buffer (find-file-noselect filename))
               (headings (with-current-buffer buffer
                           (org-map-entries #'org-get-heading)))
               (headings-num
                (let ((index 0))
                  (mapcar #'(lambda (s)
                              (prog1
                                  (concat (number-to-string index)
                                          "-" s)
                                (cl-incf index)))
                          headings)))
               (suggested-theme (nth (mod (random) 7) headings))
               (match (ivy-completing-read (format "What's the theme for today (Suggested: %s)? "
                                                   suggested-theme)
                                           headings-num))
               (i (1+ (string-match "-" match))))
          (substring match i))))))

(defvar yearly-theme "Thought")

(provide 'my-org-capture-templates)
;;; my-org-capture-templates.el ends here
