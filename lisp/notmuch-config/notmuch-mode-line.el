;;; notmuch-mode-line.el ---  -*- lexical-binding: t -*-

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

(defvar notmuch-unread-mode-line-string "")
(defvar notmuch-unread-inbox-count -1)

(defun notmuch-open-inbox ()
  (interactive)
  (notmuch-search "tag:inbox" t))

(defvar mode-line-notmuch-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'notmuch-open-inbox)
    map))

(defconst my-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      (make-sparse-keymap))
    map))

(defun notmuch-count-query (query)
  (->> query
       (notmuch-command-to-string "count")
       (replace-regexp-in-string "\n" "")
       (string-to-number)))

(defun notmuch-get-unread-string ()
  (let ((count
         (setq notmuch-unread-inbox-count
               (notmuch-count-query "tag:inbox AND tag:unread")))
        (all (notmuch-count-query "tag:inbox")))
    (format " %s [%d/%d] "
            (if (zerop count) "" "")
            count all)))

(defun notmuch-update-mode-string ()
  (setq notmuch-unread-mode-line-string
        (notmuch-get-unread-string)))

(add-to-list 'global-mode-string
             ;; TODO: switch to using :propertize
             `(:eval (propertize notmuch-unread-mode-line-string
                                 'help-echo ,(purecopy "mouse-1: Open inbox")
                                 'face (when (not (zerop notmuch-unread-inbox-count)) font-lock-warning-face)
                                 'mouse-face 'mode-line-highlight
		                         'local-map mode-line-notmuch-keymap))
             t)

;; (setq global-mode-string (butlast global-mode-string))

(run-at-time nil 10 'notmuch-update-mode-string)

;; (defcustom doom-modeline-notmuch nil
;;   "Whether display the notmuch notifications."
;;   :type 'boolean
;;   :group 'doom-modeline)

;; (doom-modeline-def-segment notmuch
;;   "Show notifications of any unread emails in `notmuch'."
;;   (when (and doom-modeline-notmuch
;;              (doom-modeline--segment-visible 'notmuch)
;;              (bound-and-true-p notmuch-unread-inbox-count)
;;              (numberp notmuch-unread-inbox-count)
;;              ;; don't display if the unread mails count is zero
;;              ;; (> notmuch-unread-inbox-count 0)
;;              )
;;     (concat
;;      doom-modeline-spc
;;      (propertize
;;       (concat
;;        (doom-modeline-icon 'material "email" "📧" "#"
;;                            :face 'doom-modeline-notification
;;                            :height 1.1 :v-adjust -0.2)
;;        doom-modeline-vspc
;;        (propertize
;;         (if (> notmuch-unread-inbox-count doom-modeline-number-limit)
;;             (format "%d+" doom-modeline-number-limit)
;;           (number-to-string notmuch-unread-inbox-count))
;;         'face '(:inherit
;;                 (doom-modeline-unread-number doom-modeline-notification))))
;;       'mouse-face 'doom-modeline-highlight
;;       'keymap '(mode-line keymap
;;                           (mouse-1 . notmuch-open-inbox))
;;       'help-echo (concat (if (= notmuch-unread-inbox-count 1)
;;                              "You have an unread email"
;;                            (format "You have %s unread emails" notmuch-unread-inbox-count))
;;                          "\nClick here to view "
;;                          (if (= notmuch-unread-inbox-count 1) "it" "them")))
;;      doom-modeline-spc)))

;; (doom-modeline-def-segment notmuch
;;   "Display battery status."
;;   (when (and (doom-modeline--segment-visible 'notmuch)
;;              (bound-and-true-p notmuch-unread-mode-line-string))
;;     (concat doom-modeline-spc
;;             notmuch-unread-mode-line-string
;;             doom-modeline-spc)))

(provide 'notmuch-mode-line)
;;; notmuch-mode-line.el ends here
