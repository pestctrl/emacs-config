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
(require 'org-protocol)
(require 'my-org-agenda-files)
(require 'org-capture)

;; Defines 3 things:
;; - #'setup-automatic-review
;; - #'finalize-review
;; - #'clear-out-review-files
;; - #'review-hide-journal-entries
(require 'my-org-weekly-review)

;; Provides #'org-notes-find-file
(require 'org-capture-notes)

(require 'org-capture-emacs-exit-warn)

(defun my/org-board-prompt ()
  (let ((desc (plist-get org-capture-current-plist :description)))
    (when (and (not org-note-abort)
               (string= desc "\tProtocol Link")
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
    (when (org-at-item-p)
      (org-toggle-checkbox))))

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
    (let ((today-plan (format-time-string "\\*\\* [A-z]* Plan for \\[%Y-%m-%d %a\\]")))
      (when (re-search-forward today-plan nil t)
        (org-narrow-to-subtree)
        (org-show-all)
        (end-of-buffer)
        (when (save-excursion
                (beginning-of-line)
                (not (looking-at-p "- \\[ \\] $")))
          (org-insert-item 'checkbox))
        (display-buffer buffer)
        (error "Already have a plan today!")))))

(defun my/org-find-journal ()
  (let* ((files
          (directory-files (my/org-file "journals/")
                           nil "^[^\\.]"))
         (journal (ivy-completing-read "Which journal? " files)))
    (when (not (file-name-extension journal))
      (setq journal (concat journal "")))
    (find-file (my/org-file (format "journals/%s" journal)))
    (outline-next-heading)))

(setq org-capture-templates
      (doct `(("Tasks" :keys "t" :children
               (("New Refile Task"
                 :keys "t"
                 :icon ,(all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01)
                 :file ,(my/agenda-file "refile.org")
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
                 :template "* TASK %?\nSCHEDULED: %T")))
              ("Reviews" :keys "r"
               :icon ,(all-the-icons-faicon "share" :face 'all-the-icons-lblue)
               :children
               (("Automatic Review"
                 :keys "a"
                 :file ,(my/org-file "entries/reviews.gpg")
                 :function setup-automatic-review
                 :before-finalize finalize-review
                 :after-finalize clear-out-review-files
                 :hook review-hide-journal-entries
                 :template-file ,(my/org-file "templates/weekly-review.org"))
                ("Manual Review"
                 :keys "m"
                 :file ,(my/org-file "entries/reviews.gpg")
                 :function setup-manual-review
                 :template-file ,(my/org-file "templates/weekly-review.org"))
                ("Reorient"
                 :keys "o"
                 :file ,(my/org-file "entries/reviews.gpg")
                 :template-file ,(my/org-file "templates/reorient.org"))))
              ("Money" :keys "m"
               :icon ,(all-the-icons-material "attach_money" :face 'all-the-icons-lgreen)
               :children
               (("Credit Card"
                 :keys "c"
                 :icon ,(all-the-icons-faicon "credit-card" :face 'all-the-icons-blue)
                 :file ,(my/org-file "entries/finances/ledger.ledger")
                 :unnarrowed t :empty-lines 1 :type plain
                 :template-file ,(my/org-file "templates/credit.ledger"))
                ("General" :keys "g"
                 :unnarrowed t :empty-lines 1 :type plain
                 :file ,(my/org-file "entries/finances/ledger.ledger")
                 :template-file ,(my/org-file "templates/basic.ledger"))))
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
                 :file ,(my/org-file "gpg/journal.gpg")
                 :datetree t
                 :keys "d"
                 :template-file ,(my/org-file "templates/decide.org"))
                ("Journal Entry" :keys "e"
                 :file ,(my/org-file "gpg/journal.gpg")
                 :datetree t
                 :template "* %<%R> %?")
                ("New Journal System" :keys "j"
                 :function my/org-find-journal
                 :datetree t
                 :template "* %U\n%?")
                ("Programming Interview Prep Journal" :keys "p"
                 :file ,(my/org-file "gpg/journal.gpg")
                 :datetree t
                 :template "* ")))
              ("Create checklist" :keys "c" :children
               (("Conference Via Bus"
                 :keys "c"
                 :file ,(my/agenda-file "dev.org")
                 :template-file ,(my/org-file "checklists/conference.org")
                 :conference/airplane nil)
                ("Morning routine"
                 :keys "m"
                 :file ,(my/org-file "entries/routines.org")
                 :template-file ,(my/org-file "templates/morning-routine.org"))
                ("Night routine"
                 :keys "n"
                 :file ,(my/org-file "entries/routines.org")
                 :template-file ,(my/org-file "templates/evening-routine.org"))))
              ("Protocol"
               :keys "p"
               :file ,(my/agenda-file "refile.org")
               :template "* STUFF %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n#+begin_example\n%i\n#+end_example\n%?")
              ("Protocol Link"
               :keys "L"
               :file ,(my/agenda-file "refile.org")
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
                 :file ,(my/org-file "entries/cringe.gpg")
                 :template "* %?")
                ("Mental Model" :keys "m"
                 :file ,(my/org-file "entries/mental_models.gpg")
                 :template "* %?")
                ("Important Information"
                 :keys "i"
                 :file ,(my/org-file "entries/important.gpg")
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
                                (incf index)))
                          headings)))
               (suggested-theme (nth (mod (random) 7) headings))
               (match (ivy-completing-read (format "What's the theme for today (Suggested: %s)? "
                                                   suggested-theme)
                                           headings-num))
               (i (1+ (string-match "-" match))))
          (substring match i))))))

(defvar yearly-theme "Thought")

(defun my/org-add-tag (tag)
  (org-set-tags (cons tag (org-get-tags nil t))))

(defun org-board-add-offline-tag (&rest args)
  (my/org-add-tag "offline"))

(advice-add #'org-board-archive :after
            #'org-board-add-offline-tag)

(provide 'my-org-capture-templates)
;;; my-org-capture-templates.el ends here
