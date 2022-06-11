;;; org-dev-level.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-01-10 12:57]

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
(require 'org)
(require 'cl)

(defvar org-dev-current-level 0)

(defun odl/incr ()
  (interactive)
  (cl-incf org-dev-current-level))

(defun odl/decr ()
  (interactive)
  (cl-decf org-dev-current-level))

(defun odl/reset ()
  (interactive)
  (setq org-dev-current-level 0))

(defun odl/get-heading-level ()
  (let ((prop (org-entry-get (point) "DEV_LEVEL" t)))
    (if (and prop
             (not (zerop (length prop))))
        (string-to-number prop)
      0)))

(defun odl/part-of-current-level-p ()
  (interactive)
  (<= org-dev-current-level (odl/get-heading-level)))

(defun odl/increment-heading ()
  (let ((new-level (1+ (odl/get-heading-level))))
    (org-entry-put (point) "DEV_LEVEL" (int-to-string new-level))
    new-level))

(defun odl/decrement-heading ()
  (let ((new-level (max 0 (1- (odl/get-heading-level)))))
    (org-entry-put (point) "DEV_LEVEL" (int-to-string new-level))
    new-level))

(defun odl/reset-heading ()
  (org-entry-put (point) "DEV_LEVEL" "0"))

(defun odl/org-agenda-decrement-heading (arg &optional time)
  (interactive "P")
  (if arg
      (odl/decr)
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
          (setq level (odl/decrement-heading))))
      (next-line)
      (message "Incremented level to %d" level))))

(defun odl/org-agenda-increment-heading (arg)
  (interactive "P")
  (if arg
      (odl/incr)
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
          (setq level (odl/increment-heading))))
      (next-line)
      (message "Incremented level to %d" level))))

(defun odl/org-agenda-reset-heading (arg)
  (interactive "P")
  (if arg
      (odl/reset)
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
          (odl/reset-heading)))
      (next-line)
      (message "Reset level to 0" level))))

(define-key org-agenda-mode-map (kbd "+") #'odl/org-agenda-increment-heading)
(define-key org-agenda-mode-map (kbd "-") #'odl/org-agenda-decrement-heading)
(define-key org-agenda-mode-map (kbd "=") #'odl/org-agenda-reset-heading)

(provide 'org-dev-level)
;;; org-dev-level.el ends here
