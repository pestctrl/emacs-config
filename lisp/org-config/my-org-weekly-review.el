;;; my-org-weekly-review.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-06-21 18:01]

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
  (beginning-of-buffer)
  (outline-next-heading)
  (unless current-prefix-arg
    ;; Check for older review
    (if (and (file-exists-p "~/.emacs.d/review-incomplete.el")
             (save-excursion
               (when (file-exists-p "~/MEGA/org/entries/reviews.gpg")
                 (switch-to-buffer (find-file "~/MEGA/org/entries/reviews.gpg"))
                 (end-of-buffer)
                 (org-back-to-heading t)
                 (org-up-heading-safe)
                 (org-narrow-to-subtree)
                 (org-show-all))
               (y-or-n-p (format "Woah, we found an incomplete review: %s. Would you like to use that date as the start date? "
                                 (shell-command-to-string "cat ~/.emacs.d/review-incomplete.el | tr -d '\n'")))))
        (rename-file "~/.emacs.d/review-incomplete.el" "~/.emacs.d/last-review.el" t)
      (delete-file  "~/.emacs.d/review-incomplete.el"))
    ;; Setup current review
    (let* ((date (org-read-date nil nil (get-last-review-date)))
           (week (format "%02d" 
                         (org-days-to-iso-week
                          (org-time-string-to-absolute date)))))
      (output-incomplete-date)
      (setq my/review-date-old date)
      (setq my/review-visibility-level 6)
      (org-capture-put :start-date date)
      (org-capture-put :start-week week))))

(defun review-hide-journal-entries ()
  (save-excursion
    (re-search-forward "Journal")
    (org-back-to-heading t)
    (org-flag-subtree t)
    (org-show-children)))

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
    (when my/review-date-old
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

(defun clear-out-review-files ()
  (when (file-exists-p "~/.emacs.d/review-incomplete.el")
    (shell-command "rm ~/.emacs.d/review-incomplete.el")))
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

(provide 'my-org-weekly-review)
;;; my-org-weekly-review.el ends here
