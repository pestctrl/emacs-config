;;; notmuch-config.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-05 14:42]

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
(require 'use-package)

(setq user-mail-address "bensonchu457@fastmail.com"
      user-full-name "Benson Chu")

(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(mailcap-add "text/html" "/usr/bin/xdg-open %s ")

(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(use-package notmuch
  :commands notmuch
  :bind (;; :map notmuch-message-mode-map
         ;; ("C-c C-c" . #'my/choose-email-address-and-send)
         :map notmuch-search-mode-map
         ("z" . #'notmuch-search-tree-current-thread)
         ("A" . #'notmuch-search-show-all)
         ("D" . #'notmuch-search-delete-all)
         ("f" . #'notmuch-search-filter-for-domain)
         ("F" . #'notmuch-search-filter-for-sender)
         ("d" . #'my/notmuch-search-delete-mail)
         ("<mouse-1>" . nil)
         :map notmuch-tree-mode-map
         ("<tab>" . #'notmuch-tree-explore-here)
         ("<down>" . #'notmuch-tree-next-message)
         ("<up>" . #'notmuch-tree-prev-message)
         ("U" . #'notmuch-tree-unfold-all)
         ("u" . #'notmuch-tree-up-thread)
         ("N" . #'notmuch-tree-next-sibling)
         ("P" . #'notmuch-tree-prev-sibling)
         ("t" . #'notmuch-tree-toggle-folding-thread)
         ("/" . #'notmuch-tree-undo-read)
         ("F" . #'notmuch-tree-focus)
         ("S-SPC" . #'notmuch-tree-scroll-message-window-back))
  :config
  (require 'notmuch-mode-line)
  (require 'notmuch-nav)
  (require 'notmuch-tree-hide)
  (require 'notmuch-tree)
  ;; (require 'notmuch-fold)

  (defun my/notmuch-search-delete-mail ()
    (interactive)
    (cond ((string-match-p "tag:inbox" notmuch-search-query-string)
           (notmuch-search-add-tag '("-inbox" "+archive")))
          (t
           (user-error "Haven't designated a tag for deleting mail in this folder")))
    (next-line))

  (setq notmuch-draft-tags '("+draft"))

  (custom-set-faces
   '(notmuch-tree-match-tree-face ((t (:family "Source Code Pro"))) t)
   '(notmuch-tree-no-match-tree-face ((t (:family "Source Code Pro"))) t))
  (set-face-attribute 'notmuch-search-unread-face nil :foreground "white")
  (set-face-attribute 'notmuch-message-summary-face nil :background "steel blue" :foreground "snow")
  (add-to-list 'notmuch-search-line-faces
               '("deleted" . font-lock-comment-face))

  (defun notmuch-search-show-all ()
    (interactive)
    (let* ((query (replace-regexp-in-string "date:[^ ]+" "" notmuch-search-query-string))
           (noand (replace-regexp-in-string "^ *and +" "" query))
           (noand2 (replace-regexp-in-string " +and *" "" query)))
      (notmuch-search noand2)))

  (defun notmuch-search-tree-current-thread (arg)
    (interactive "P")
    (let* ((thread-id (notmuch-search-find-thread-id))
           (input (notmuch-read-query (concat "Notmuch tree: " thread-id " and "))))
      (notmuch-tree thread-id (unless (zerop (length input)) input) nil nil nil nil nil (unless arg #'notmuch-tree-hide-dead-trees))))

  (defun notmuch-tree-focus (arg)
    (interactive "P")
    (notmuch-tree notmuch-tree-basic-query (notmuch-tree-get-message-id) nil nil nil nil nil (if (not arg) #'notmuch-tree-hide-dead-trees #'notmuch-tree-show-trail-and-alive-children)))

  (defun notmuch-tree-undo-read (arg)
    (interactive "P")
    (if arg
        (save-excursion
          (beginning-of-buffer)
          (while (text-property-search-forward
                  'face 'notmuch-tag-deleted
                  #'(lambda (value prop)
                      (if (consp prop)
                          (member value prop)
                        (eq value prop))))
            (notmuch-tree-add-tag '("+unread"))))
      (notmuch-tree-add-tag '("+unread"))
      (next-line)))

  (setq notmuch-search-oldest-first nil
        notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          ;; (:name "inbox today" :query "date:2020-07-25.. and tag:inbox" :key "t")
          (:name "migration" :query "tag:migration" :key "m")
          (:name "work" :query "tag:work" :key "w")
          (:name "emacs-devel" :query "tag:lists/emacs-devel and date:6M.." :key "e" :sort-order newest-first)
          (:name "emacs bugs" :query "tag:lists/bug-gnu-emacs and date:30d.." :key "E")
          (:name "emacs help" :query "tag:lists/help-gnu-emacs and date:30d.." :key "h")
          (:name "org-mode" :query "tag:lists/emacs-orgmode and date:6M.." :key "o" :sort-order newest-first)
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "cs" :query "tag:cs" :key "c")
          (:name "receipts" :query "tag:receipts" :key "R")
          (:name "tracking" :query "tag:tracking" :key "t")
          (:name "voicemail" :query "from:vm@italkbb.com" :key "v")
          ;; (:name "sent" :query "tag:sent" :key "s")
          ;; (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")))

  (defun my/choose-email-address-and-send ()
    (interactive)
    (let ((resp (completing-read "Which email? " '("bensonchu457@fastmail.com" "bensonchu457@gmail.com") nil t "^")))
      (setq smtpmail-smtp-server
            (pcase resp
              ("bensonchu457@gmail.com" "smtp.gmail.com")
              ("bensonchu457@fastmail.com" "smtp.fastmail.com")))
      (notmuch-mua-send-and-exit)))

  (add-to-list 'notmuch-tagging-keys
               '("R" ("-inbox" "+recruiting") "Recruiting"))

  (setf (cdr (assoc "d" notmuch-tagging-keys))
        '(("+deleted") "Delete"))

  (advice-add #'notmuch-tag-jump :after #'(lambda (&rest args) (next-line)))

  (defun notmuch-search-filter-for-domain ()
    (interactive)
    (notmuch-search-show-thread)
    (let* ((author (notmuch-show-get-from)))
      (notmuch-bury-or-kill-this-buffer)
      (string-match (rx (and (group (+ (not (any "." "@" "<")))
                                    "."
                                    (+ (not (any "." "@" "<" ">"))))
                             (or ">" eol)))
                    author)
      (notmuch-search-filter (format "from:%s" (match-string 1 author)))))

  (defun notmuch-search-filter-for-sender ()
    (interactive)
    (notmuch-search-show-thread)
    (let ((author (notmuch-show-get-from)))
      (notmuch-bury-or-kill-this-buffer)
      (notmuch-search-filter (format "from:%s" author))))

  (defun notmuch-search-delete-all ()
    (interactive)
    (let ((line-count (save-excursion (end-of-buffer) (line-number-at-pos (point)))))
      (when (or (< line-count 20)
                (y-or-n-p "Are you sure? There seems to be a lot of emails... "))
        (notmuch-search-tag-all '("+deleted")))))

  (defun notmuch-add-child (child-id)
    (interactive (list (read-string (format "Message id of new child (default: %s): "
                                            (current-kill 0))
                                    nil nil (current-kill 0))))
    (let ((parent-id (notmuch-show-get-message-id t))
          (child-file
           (save-window-excursion
             (notmuch-show (format "id:%s" child-id))
             (notmuch-show-get-filename))))
      (with-current-buffer (find-file-noselect child-file)
        (beginning-of-buffer)
        (if (save-excursion (re-search-forward "^In-Reply-To: " nil t))
            (error "File already has reply message")
          (save-excursion
            (re-search-forward "^Date: ")
            (end-of-line)
            (insert (format "\nIn-Reply-To: <%s>"
                            parent-id))
            (save-buffer)))
        (re-search-forward "^Message-ID: <\\(.*\\)>$")
        (message (match-string 1)))
      (notmuch-refresh-file child-id)))

  (defun notmuch-refresh-file (id)
    (interactive (list (read-string "Which id? ")))
    (let ((thread-id
           (replace-regexp-in-string
            "\\n" ""
            (shell-command-to-string
             (format "notmuch search --output=threads id:%s" id)))))
      (shell-command (format "notmuch reindex %s" thread-id)))
    (shell-command (format "notmuch reindex id:%s" id)))

  (defun notmuch-show-goto-file ()
    (interactive)
    (find-file (notmuch-show-get-filename))))

(use-exwm
 :config
 ;; (defvar offlineimap-timer nil)
 ;; (defvar offlineimap-process nil)

 ;; (defun run-offlineimap ()
 ;;   (interactive)
 ;;   (if (and (processp offlineimap-process)
 ;;            (process-live-p offlineimap-process))
 ;;       (message "offlineimap already running...")
 ;;     (message "offlineimap starting...")
 ;;     (when (and (timerp offlineimap-timer)
 ;;                (not (timer--triggered offlineimap-timer)))
 ;;       (cancel-timer offlineimap-timer))
 ;;     (call-process-shell-command "timedatectl" nil "*offlineimap-output*")
 ;;     (set-process-sentinel
 ;;      (setq offlineimap-process
 ;;            (start-process-shell-command "offlineimap" "*offlineimap-output*" "offlineimap"))
 ;;      #'(lambda (process event)
 ;;          (when (string-match-p "exited abnormally with code 1" event)
 ;;            (with-current-buffer (process-buffer offlineimap-process)
 ;;              (when (string-match-p "get_password_emacs"(buffer-string))
 ;;                (erase-buffer)
 ;;                (message "Oops, didn't grab a password. ")
 ;;                (setq offlineimap-timer (run-with-timer 300 nil #'run-offlineimap)))))
 ;;          (when (string-match-p "^finished" event)
 ;;            (message "Offlineimap finished")
 ;;            (setq offlineimap-timer (run-with-timer 300 nil #'run-offlineimap)))))))

 ;; (defun stop-offlineimap ()
 ;;   (interactive)
 ;;   (when (timerp offlineimap-timer)
 ;;     (cancel-timer offlineimap-timer))
 ;;   (when (processp offlineimap-process)
 ;;     (set-process-sentinel offlineimap-process
 ;;                           nil)))

 ;; (add-to-list 'exwm-init-hook
 ;;              #'run-offlineimap
 ;;              t)

 (defvar mbsync-timer nil)
 (defvar mbsync-process nil)

 (defun run-mbsync ()
   (interactive)
   (if (and (processp mbsync-process)
            (process-live-p mbsync-process))
       (message "mbsync already running...")
     (message "mbsync starting...")
     (when (and (timerp mbsync-timer)
                (not (timer--triggered mbsync-timer)))
       (cancel-timer mbsync-timer))
     (call-process-shell-command "timedatectl" nil "*mbsync-output*")
     (set-process-sentinel
      (setq mbsync-process
            (start-process-shell-command "mbsync" "*mbsync-output*"
                                         "~/bin/update-mail.sh"))
      #'(lambda (process event)
          (when (string-match-p "exited abnormally with code 1" event)
            (with-current-buffer (process-buffer mbsync-process)
              (when (string-match-p "get_password_emacs"(buffer-string))
                (erase-buffer)
                (message "Oops, didn't grab a password. ")
                (setq mbsync-timer (run-with-timer 300 nil #'run-mbsync)))))
          (when (string-match-p "^finished" event)
            (message "mbsync finished")
            (setq mbsync-timer (run-with-timer 300 nil #'run-mbsync))))))))

(provide 'notmuch-config)
;;; notmuch-config.el ends here
