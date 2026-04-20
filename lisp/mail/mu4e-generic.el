;;; mu4e-generic.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2026-04-19 20:10]

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
(require 'mu4e-contrib)

(define-key mu4e-headers-mode-map (kbd "M") #'mu4e-headers-mark-all)

(defun mu4e-print-path ()
  (interactive)
  (-->
   (mu4e-message-at-point)
   (mu4e-message-field it :path)
   (progn
     (kill-new it)
     (message "Path Copied: %s" it))))

(define-key mu4e-search-minor-mode-map (kbd "l") #'mu4e-print-path)

(add-hook 'mu4e-headers-found-hook #'mu4e-thread-fold-all)

(define-key mu4e-headers-mode-map (kbd "C-M-a") #'mu4e-view-thread-goto-root)
(define-key mu4e-thread-mode-map (kbd "<tab>") #'mu4e-thread-fold-toggle)

;; Prevent automatic baseline resets to keep (+x) indicators persistent
(defvar my/mu4e-allow-baseline-reset nil
  "When non-nil, allow baseline resets. Otherwise, preserve the baseline.")

(defun my/mu4e--query-items-refresh-no-auto-reset (orig-fun &optional reset-baseline)
  "Advice for `mu4e--query-items-refresh' to prevent automatic baseline resets.
Only reset the baseline if `my/mu4e-allow-baseline-reset' is non-nil."
  (funcall orig-fun (and reset-baseline my/mu4e-allow-baseline-reset)))

(advice-add 'mu4e--query-items-refresh :around
            #'my/mu4e--query-items-refresh-no-auto-reset)

(defun my/mu4e-reset-baseline ()
  "Manually reset the mu4e baseline to clear all (+x) indicators."
  (interactive)
  (let ((my/mu4e-allow-baseline-reset t))
    (mu4e--query-items-refresh 'reset-baseline))
  (message "mu4e baseline reset - (+x) indicators cleared"))

(defun my/goto-mail ()
  (interactive)
  (switch-or-create-tab "mail")
  (delete-other-windows)
  (mu4e))

(global-set-key (kbd "C-x m") #'my/goto-mail)

(with-eval-after-load 'mu4e-view
  (add-hook 'mu4e-view-mode-hook
            #'olivetti-mode))

(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

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
            #'my/mu4e~headers-thread-prefix)

(defun my/mu4e-compose-edit-ignore-draft()
  "Edit an existing draft message."
  (interactive)
  (let* ((msg (mu4e-message-at-point)))
    (mu4e--draft
     'edit
     (lambda ()
       (with-current-buffer
           (find-file-noselect (mu4e-message-readable-path msg))
         (mu4e--delimit-headers)
         (current-buffer))))))

(advice-add #'mu4e-compose-edit
            :override
            #'my/mu4e-compose-edit-ignore-draft)

(provide 'mu4e-generic)
;;; mu4e-generic.el ends here
