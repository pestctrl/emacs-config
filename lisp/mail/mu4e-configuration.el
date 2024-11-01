;;; mu4e-config.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-12 09:51]

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
(require 'mbsync)

(unless (eq system-type 'windows-nt)
  (add-to-list 'load-path
               "/usr/share/emacs/site-lisp/mu4e/")
  (require 'mu4e)

  (add-hook 'mbsync-postsync-hooks
            '(lambda (&rest _ignore)
               (mu4e-update-mail-and-index t)))

  ;; For mbsync, this must be set
  (setq mu4e-change-filenames-when-moving t)

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; (global-set-key (kbd "<f8>") 'mu4e)

  (setq mu4e-maildir "~/.mail"
        mu4e-sent-folder   "/fastmail/Sent"
        mu4e-drafts-folder "/fastmail/Drafts"
        mu4e-refile-folder "/fastmail/Archive"
        mu4e-trash-folder "/fastmail/Trash")

  (setq mu4e-maildir-shortcuts
        '(("/fastmail/INBOX" . ?i)
          ("/fastmail/INBOX/tracking" . ?t)
          ("/fastmail/INBOX/unsorted" . ?u)
          ("/fastmail/Archive" . ?a)))

  (setq mu4e-bookmarks
        '(( :name  "Unread messages"
            :query "flag:unread AND NOT flag:trashed"
            :key ?u)
          ( :name "Today's messages"
            :query "date:today..now"
            :key ?t)
          ( :name "Last 7 days"
            :query "date:7d..now"
            :hide-unread t
            :key ?w)
          ( :name "Messages with images"
            :query "mime:image/*"
            :key ?p)))

  (setq mu4e-bookmarks
        (mapcar (lambda (x)
                  (let ((str (plist-get x :query)))
                    (when (not (string-match-p "Unnecessary/mailing_lists" str))
                      (setf (plist-get x :query)
                            (concat str " AND NOT maildir:/Unnecessary/mailing_lists*"))))
                  x)
                mu4e-bookmarks))

  (defvar my/email-accounts
    '("bensonchu457@fastmail.com"
      "bensonchu457@gmail.com"
      "me@mail.pestctrl.io"
      "dev@mail.pestctrl.io"))

  (defun my/mu-init ()
    (interactive)
    (async-shell-command
     (format "mu init --maildir=~/.mail/ %s"
             (--> my/email-accounts
                  (mapcar #'(lambda (x) (concat "--my-address=" x)) it)
                  (string-join it " ")))))

  (defun my-mu4e-set-account ()
    (setq user-mail-address
          (if (not mu4e-compose-parent-message)
              (completing-read "Compose with account: "
                               my/email-accounts
                               nil t)
            (mu4e-contact-email
             (-any
              #'(lambda (x)
                  (mu4e-message-contact-field-matches-me mu4e-compose-parent-message
                                                         x))
              '(:to :cc :bcc :from))))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  (if (not (string= mu4e-mu-version "1.12.5"))
      (warn "Remove this advice, bug has been fixed")
    (defun my/mu4e-run-pre-hook (compose-type compose-func &optional parent)
      (let ((mu4e-compose-parent-message parent)
            (mu4e-compose-type compose-type))
        (run-hooks 'mu4e-compose-pre-hook)))
    (advice-add #'mu4e--draft
                :before
                #'my/mu4e-run-pre-hook))

  ;; (setcar mu4e-headers-thread-last-child-prefix "╰┬►")
  ;; (setcar mu4e-headers-thread-first-child-prefix "├┬►")
  ;; (setcar mu4e-headers-thread-connection-prefix "│")

  (defun my/mu4e~headers-thread-prefix (thread)
    "Calculate the thread prefix based on thread info THREAD."
    (when thread
      (let* ((prefix       "")
             (level        (plist-get thread :level))
             (has-child    (plist-get thread :has-child))
             (last-child   (plist-get thread :last-child))
             (duplicate    (plist-get thread :duplicate)))
        ;; Do not prefix root messages.
        (when (= level 0)
          (setq mu4e~headers-thread-state '())
          (when has-child
            (setq prefix "┬►")))
        (if (> level 0)
            (let* ((length (length mu4e~headers-thread-state))
                   (padding (make-list (max 0 (- level length)) nil)))
              ;; Trim and pad the state to ensure a message will
              ;; always be shown with the correct indentation, even if
              ;; a broken thread is returned. It's trimmed to level-1
              ;; because the current level has always an connection
              ;; and it used a special formatting.
              (setq mu4e~headers-thread-state
                    (cl-subseq (append mu4e~headers-thread-state padding)
                               0 (- level 1)))
              ;; Prepare the thread prefix.
              (setq prefix
                    (concat
                     ;; Current mu4e~headers-thread-state, composed by
                     ;; connections or blanks.
                     (mapconcat
                      (lambda (s)
                        (if s "│" " "))
                      mu4e~headers-thread-state "")
                     (if last-child "╰" "├")
                     (if has-child "┬" "─")
                     "►"))))
        ;; If a new sub-thread will follow (has-child) and the current
        ;; one is still not done (not last-child), then a new
        ;; connection needs to be added to the tree-state.  It's not
        ;; necessary to a blank (nil), because padding will handle
        ;; that.
        (if (and has-child (not last-child))
            (setq mu4e~headers-thread-state
                  (append mu4e~headers-thread-state '(t))))
        ;; Return the thread prefix.
        (format "%s%s"
                prefix
                (if duplicate
                    (mu4e~headers-thread-prefix-map 'duplicate) "")))))

  (advice-add #'mu4e~headers-thread-prefix
              :override
              #'my/mu4e~headers-thread-prefix))

(provide 'mu4e-configuration)
;;; mu4e-config.el ends here
