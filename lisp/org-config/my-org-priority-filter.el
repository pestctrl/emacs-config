;;; my-org-priority-filter.el --- Dynamic priority filtering for org-agenda -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2026-05-27]

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

;; This module provides dynamic priority filtering for org-agenda views.
;; Items can have a MY_PRIORITY property (default 0), and you can filter
;; the agenda view to show only items with priority >= a threshold.
;; Priority is inherited from parent headings.

;;; Code:
(require 'org)
(require 'org-agenda)

(defvar my/org-priority-filter-level 0
  "Global priority filter level.
Only items with MY_PRIORITY >= this value will be shown in filtered views.
Priority is inherited from parent headings if not set explicitly.")

(defun my/org-get-priority-with-inheritance ()
  "Get the MY_PRIORITY value for the current heading, with inheritance.
Returns 0 if no MY_PRIORITY property is found in the heading or its ancestors."
  (let ((priority (org-entry-get nil "MY_PRIORITY" t)))
    (if priority
        (string-to-number priority)
      0)))

(defun my/org-part-of-current-priority-p ()
  (interactive)
  (<= my/org-priority-filter-level (my/org-get-priority-with-inheritance)))

(defun my/org-priority-filter-predicate ()
  "Return non-nil if current heading should be filtered out based on priority.
This is a skip function that returns the position to skip to if the item
should be filtered out."
  (let ((item-priority (my/org-get-priority-with-inheritance)))
    (when (< item-priority my/org-priority-filter-level)
      (org-end-of-subtree t))))

(defun my/org-agenda-increase-priority-filter ()
  "Increase the global priority filter level and refresh the agenda."
  (interactive)
  (setq my/org-priority-filter-level (1+ my/org-priority-filter-level))
  (message "Priority filter level: %d (showing items with priority >= %d)"
           my/org-priority-filter-level
           my/org-priority-filter-level)
  (clrhash org-ql-cache))

(defun my/org-agenda-decrease-priority-filter ()
  "Decrease the global priority filter level and refresh the agenda."
  (interactive)
  (setq my/org-priority-filter-level (max 0 (1- my/org-priority-filter-level)))
  (message "Priority filter level: %d (showing items with priority >= %d)"
           my/org-priority-filter-level
           my/org-priority-filter-level)
  (clrhash org-ql-cache))

(defun my/org-agenda-add-priority-to-item (n &optional reset)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         level)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (let ((new-priority
               (if reset
                   0
                 (+ n (my/org-get-priority-with-inheritance)))))
          (progn
            (message "Incremented level to %d" new-priority)
            (org-set-property "MY_PRIORITY" (number-to-string new-priority))))))))

(defun my/org-agenda-increase-priority ()
  (interactive)
  (my/org-agenda-add-priority-to-item 1))

(defun my/org-agenda-decrease-priority ()
  (interactive)
  (my/org-agenda-add-priority-to-item -1))

(defun my/org-agenda-reset-priority-filter ()
  "Reset the global priority filter level to 0 and refresh the agenda."
  (interactive)
  (setq my/org-priority-filter-level 0)
  (message "Priority filter reset to 0 (showing all items)")
  (clrhash org-ql-cache))

;; Add keybindings to org-agenda-mode
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "+") #'my/org-agenda-increase-priority-filter)
  (define-key org-agenda-mode-map (kbd "-") #'my/org-agenda-decrease-priority-filter)
  (define-key org-agenda-mode-map (kbd "=") #'my/org-agenda-reset-priority-filter)

  (define-key org-agenda-mode-map (kbd "U") #'my/org-agenda-increase-priority)
  (define-key org-agenda-mode-map (kbd "D") #'my/org-agenda-decrease-priority) )

(provide 'my-org-priority-filter)
;;; my-org-priority-filter.el ends here
