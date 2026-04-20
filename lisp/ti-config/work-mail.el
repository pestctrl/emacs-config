;;; work-mail.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-06-03 10:49]

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

(require 'mu4e)
(require 'mu4e-generic)

(require 'ti-mail-identity)
(require 'gnus-article-hack)

(define-key mu4e-headers-mode-map (kbd "R") #'mu4e-compose-wide-reply)

(setq mu4e-update-interval (* 60 5))

;; For mbsync, this must be set
(setq mu4e-change-filenames-when-moving t)

;; (global-set-key (kbd "<f8>") 'mu4e)

(setq mu4e-maildir "~/.mail"
      mu4e-sent-folder   "/work/Sent Items"
      mu4e-drafts-folder "/work/Drafts"
      mu4e-refile-folder "/work/Archive"
      mu4e-trash-folder "/work/Trash")

(setq mu4e-maildir-shortcuts
      '((:key ?i :maildir "/work/INBOX")
        (:key ?a :maildir "/work/Done")))

(setq mu4e-bookmarks
      '(( :name  "Inbox"
          :query "maildir:/work/INBOX AND NOT flag:trashed"
          :key ?i)
        ( :name  "Unread messages"
          :query "flag:unread AND NOT maildir:/work/Other* AND NOT flag:trashed"
          :key ?u)
        ( :name  "mailing_lists"
          :query "maildir:/work/INBOX/mailing_lists* AND NOT flag:trashed"
          :key ?m)
        ( :name  "Services"
          :query "maildir:/work/INBOX/Services* AND NOT flag:trashed"
          :key ?s)
        ( :name  "automation"
          :query "maildir:/work/INBOX/automation* AND NOT flag:trashed"
          :key ?a)
        ( :name "Today's messages"
          :query "date:today..now"
          :key ?t)
        ( :name "Last 7 days"
          :query "date:7d..now"
          :hide-unread t
          :key ?w)
        ;; ( :name "Messages with images"
        ;;   :query "mime:image/*"
        ;;   :key ?p)
        ))

(setq mu4e-bookmarks
      (mapcar (lambda (x)
                (let ((str (plist-get x :query)))
                  (when (not (string-match-p "Unnecessary/mailing_lists" str))
                    (setf (plist-get x :query)
                          (concat str " AND NOT maildir:/Unnecessary/mailing_lists*"))))
                x)
              mu4e-bookmarks))

(defun my/mu-init ()
  (interactive)
  (let ((addresses
         '("b-chu1@ti.com")))
    (async-shell-command
     (format "mu init --maildir=~/.mail/ %s"
             (--> addresses
                  (mapcar #'(lambda (x) (concat "--my-address=" x)) it)
                  (string-join it " "))))))

;; (setcar mu4e-headers-thread-last-child-prefix "╰┬►")
;; (setcar mu4e-headers-thread-first-child-prefix "├┬►")
;; (setcar mu4e-headers-thread-connection-prefix "│")

(setq mu4e-attachment-dir "/scratch/benson/mail_attachments")

(provide 'work-mail)
;;; work-mail.el ends here
