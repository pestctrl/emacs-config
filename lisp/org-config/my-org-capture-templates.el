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

;; Defines 3 things:
;; - #'setup-automatic-review
;; - #'finalize-review
;; - #'clear-out-review-files
(require 'my-org-weekly-review)

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

(setq org-capture-templates
      (doct `(("Todo"
               :icon ,(all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01)
               :keys "t"
               :file ,(my/agenda-file "refile.org")
               :template "* STUFF %?\n:PROPERTIES:\n:CREATED: %U\n:VIEWING: %a\n:END:")
              ("Reviews" :keys "r"
               :icon ,(all-the-icons-faicon "share" :face 'all-the-icons-lblue)
               :children
               (("Automatic Review"
                 :keys "a"
                 :file ,(my/org-file "entries/reviews.gpg")
                 :function setup-automatic-review
                 :before-finalize finalize-review
                 :after-finalize clear-out-review-files
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
              ("Distracted"
               :file ,(my/agenda-file "dev.org")
               :keys "D" :clock-in t :clock-resume t
               :template "* TASK %?")
              ("New Task" :file ,(my/agenda-file "dev.org")
               :keys "T" :clock-in t :clock-keep t
               :template "* TASK %?")
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
               :keys "c"
               :datetree t 
               :template "* TODO %?")
              ("Emacs config snippet" :keys "e"
               :file "~/.emacs.d/config-base.org"
               :headline "New"
               :template "* %^{Title}\n#+begin_src emacs-lisp\n %?\n#+end_src")
              ("Journal" :keys "j" :children
               (("Decision Template"
                 :file ,(my/org-file "entries/journal.gpg")
                 :datetree t
                 :keys "d"
                 :template-file ,(my/org-file "templates/decide.org"))
                ("Journal Entry" :keys "e"
                 :file ,(my/org-file "entries/journal.gpg")
                 :datetree t
                 :template "* %<%R> %?")
                ("Programming Interview Prep Journal" :keys "p"
                 :file ,(my/org-file "entries/journal.gpg")
                 :datetree t
                 :template "* ")))
              ("Create checklist" :keys "C" :children
               (("Conference Via Bus"
                 :keys "c"
                 :file ,(my/agenda-file "dev.org")
                 :template-file ,(my/org-file "checklists/conference.org")
                 :conference/airplane nil)
                ("Morning routine"
                 :keys "m"
                 :file ,(my/org-file "entries/routines.org")
                 :template-file ,(my/org-file "checklists/mornings.org"))
                ("Nightly routine"
                 :keys "n"
                 :file ,(my/org-file "entries/routines.org")
                 :template-file ,(my/org-file "checklists/nights.org"))))
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
               (("Plan week" :keys "P"
                 :file ,(my/agenda-file "dev.org")
                 :headline "The Plan"
                 :template-file ,(my/org-file "templates/weekly-plan.org"))
                ("Plan your day" :keys "p"
                 :file ,(my/agenda-file "dev.org")
                 :headline "The Plan"
                 :template-file ,(my/org-file "templates/daily-plan.org"))
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
              ("Logging" :keys "g" :children
               (("Source location"
                 :keys "s"
                 :function org-notes-find-file
                 :template "* %?\n%a")
                ("Log entry"
                 :keys "l"
                 :function org-notes-find-file
                 :template "* %?")
                ("Timer event"
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
        (let ((buffer (find-file-noselect filename))
              headings)
          (with-current-buffer buffer
            (let ((num 0))
              (setq headings (mapcar #'(lambda (str)
                                         (concat (int-to-string (incf num))
                                                 "-"
                                                 str))
                                     (org-map-entries #'org-get-heading)))))
          (let* ((match (ivy-completing-read "What's the theme for today? "
                                             headings))
                 (i (1+ (string-match "-" match))))
            (substring match i)))))))

(defvar yearly-theme "Thought")

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

;; New Note-taking capture template
(defvar org-notes-current-file nil)

(defun org-notes-find-file ()
  (when (or current-prefix-arg
            (not org-notes-current-file))
    (setq org-notes-current-file
          (read-file-name "Notes file? ")))
  (find-file org-notes-current-file)
  (end-of-buffer))

(provide 'my-org-capture-templates)
;;; my-org-capture-templates.el ends here
