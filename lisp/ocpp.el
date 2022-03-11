;;; ocpp.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-03-06 09:20]

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

;; Stands for org-clock-push-pop

;;; Code:
(require 'switch-tabs)
(require 'org)
(require 'org-project)
(require 'cl)

(cl-defstruct ocpp-task
  ""
  task-string
  tab-name
  tab-index
  task-marker
  collateral-marker)

(defvar ocpp/clock-stack '())

(defun ocpp/get-current-task ()
  (save-window-excursion
    (org-clock-goto)
    (org-get-heading)))

(defun ocpp/status ()
  (interactive)
  (if (org-clocking-p)
      (message "Currently clocked into: %s"
               (ocpp-task-task-string (car ocpp/clock-stack)))))

(defun ocpp/push (next-task)
  (interactive))

(defun org-completing-read-to-marker (prompt elements)
  (let ((result (completing-read prompt (mapcar #'car elements))))
    (cadr (assoc result elements))))

(defun org-completing-read-children (prompt)
  (interactive)
  (let ((items '()))
    (save-window-excursion
      (org-clock-goto)
      (save-restriction
        (save-excursion
          (org-narrow-to-subtree)
          (outline-next-heading)
          (while (outline-next-heading)
            (unless (eq (cdr (opr/get-type-and-state))
                        'done)
              (push (list (org-get-heading t t t t)
                          (point-marker))
                    items))))))
    (org-completing-read-to-marker prompt items)))

(defun org-completing-read-refile (prompt)
  (cadddr (org-refile-get-location)))

(defun ocpp/query-for-task-to-clock (type)
  (cond ((eq type 'children)
         )
        (t (error "Unknown task type: %s" type))))

(defun ocpp/push-tab--internal (next-task tab-name)
  (let* ((tab-name (thread-last (truncate-string-to-width next-task 20)
                     (replace-regexp-in-string " " "-")
                     (replace-regexp-in-string "[^A-z0-9-]" "")
                     (downcase)
                     (concat "OCPP-"))))

    (switch-or-create-tab tab-name)

    (scratch-buffer)

    (push (make-ocpp-task :task-string next-task
                          :tab-name tab-name
                          :tab-index (1+ (tab-bar--current-tab-index))
                          :task-marker marker)
          ocpp/clock-stack)))

(defun ocpp/push-tab--current-org-item ()
  (interactive)
  (let* ((next-task (org-get-heading t t t t))
         (marker
          (progn
            ;; (org-clock-in)
            (point-marker))))

    (ocpp/push-tab--internal next-task marker)))

(defun ocpp/push-with-tab (ARG)
  (interactive "P")
  (let ((marker (if (null ARG)
                    (org-completing-read-children "")
                  (org-completing-read-refile ""))))
    (ocpp/push-tab--internal
     new-task
     (save-window-excursion
       (set-buffer (marker-buffer new-task))
       (goto-char new-task)
       (org-get-heading t t t t)))))

(defun ocpp/pop ()
  (interactive)
  (let* ((current-task (pop ocpp/clock-stack))
         (previous-task (car ocpp/clock-stack)))
    (when-let ((tab-index (ocpp-task-tab-index current-task)))
      (tab-bar-close-tab tab-index))
    (if (null previous-task)
        (progn
          (org-clock-out)
          (message "Stack fully popped!"))
      ;; Switch tabs
      (when-let ((tab-index (ocpp-task-tab-index previous-task)))
        (tab-bar-select-tab tab-index))
      ;; Clock in
      (save-excursion
        (let ((mark (ocpp-task-task-marker previous-task)))
          (set-buffer (marker-buffer mark))
          (goto-char mark)
          (org-clock-in))))))

(provide 'ocpp)
;;; ocpp.el ends here
