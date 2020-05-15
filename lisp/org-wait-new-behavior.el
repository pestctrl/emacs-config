;;; org-wait-new-behavior.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-14 19:20]

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

(defun my/get-heading-string ()
  (looking-at org-complex-heading-regexp)
  (org-trim
   (org-link-display-format
    (replace-regexp-in-string
     "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
     (match-string-no-properties 4)))))

(defun my/org-get-refile-targets (&optional default-buffer)
  (let ((heading (save-excursion (org-back-to-heading)
                                 (org-up-heading-safe)
                                 (my/get-heading-string))))
    (org-ql-select (buffer-file-name) `(and (todo) (ancestors (heading ,heading)))
      :action (lambda (&rest args)
                (let* ((heading
                        (-as-> (org-get-outline-path t t) it
                               (mapcar (lambda (s)
                                         (replace-regexp-in-string
                                          "/" "\\/" s nil t))
                                       it)
                               (cons (file-name-nondirectory
                                      (buffer-file-name
		                       (buffer-base-buffer)))
                                     it)
                               (mapconcat #'identity it "/")))
                       ;; (re (format org-complex-heading-regexp-format
                       ;;             heading))
                       )
                  (list
                   heading
                   (buffer-file-name)
                   nil ;;re
                   (org-refile-marker (point))))))))

(defun org-get-local-id-with-outline-path-completion (&optional targets)
  (cl-letf (((symbol-function 'org-refile-get-targets)
             (symbol-function 'my/org-get-refile-targets)))
    (let ((org-refile-history nil))
      (org-id-get-with-outline-path-completion))))

(defun wait-mark-blocking-tasks (change-plist)
  (setq post-command-hook (delq 'org-add-log-note post-command-hook))
  (when (and (string= "WAIT"
                      (plist-get change-plist :to))
             (not (string= "WAIT"
                           (plist-get change-plist :from))))
    (let ((ids '()))
      (unwind-protect
          (while
              (progn
                (let ((id (org-get-local-id-with-outline-path-completion '((nil :maxlevel . 9)))))
                  (save-excursion
                    (org-id-goto id)
                    (org-entry-put (point) "WAIT_PREV_STATE" (org-get-todo-state))
                    (org-todo "NEXT"))
                  (push id ids))
                (y-or-n-p "Add another heading?"))))
      (org-entry-put (point) "WAITING" (mapconcat #'concat ids ", ")))))

(add-hook 'org-trigger-hook
          #'wait-mark-blocking-tasks)

(defun unwait-unblock-tasks (change-plist)
  (when (and (not (string= "WAIT"
                           (plist-get change-plist :to)))
             (string= "WAIT"
                      (plist-get change-plist :from)))
    (when-let (unblock (org-entry-get (point) "WAITING"))
      (-as-> unblock it
             (split-string it ", ")
             (mapcar (lambda (id)
                       (save-excursion
                         (org-id-goto id)
                         (org-todo (org-entry-get (point) "WAIT_PREV_STATE"))
                         (org-entry-delete (point) "WAIT_PREV_STATE")))
                     it)))))

(add-hook 'org-trigger-hook
          #'unwait-unblock-tasks)

(provide 'org-wait-new-behavior)
;;; org-wait-new-behavior.el ends here
