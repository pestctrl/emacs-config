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

(unless (eq system-type 'windows-nt)
  (add-to-list 'load-path
               "/usr/share/emacs/site-lisp/mu4e/")
  (require 'mu4e)

  ;; For mbsync, this must be set
  (setq mu4e-change-filenames-when-moving t)

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; (global-set-key (kbd "<f8>") 'mu4e)

  (setq mu4e-maildir "~/.mail/fastmail"
        mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-refile-folder "/Archive"
        mu4e-trash-folder "/Trash")

  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/INBOX/tracking" . ?t)
          ("/INBOX/unsorted" . ?u)
          ("/Archive" . ?a)))

  (defun my/mu-init ()
    (interactive)
    (let ((addresses
           '("bensonchu457@fastmail.com"
             "bensonchu457@gmail.com")))
      (async-shell-command
       (format "mu init --maildir=~/.mail/fastmail/ %s"
               (--> addresses
                    (mapcar #'(lambda (x) (concat "--my-address=" x)) it)
                    (string-join it " "))))))

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
