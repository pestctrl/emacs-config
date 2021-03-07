;;; org-delay.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-19 16:52]

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
(require 'org-agenda)

(require 'org-project)

(defun org-delay (arg &optional time)
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (error "Not supported yet")
    (pcase arg
      ('(4)
       (org-entry-delete (point) "DELAYED")
       "Removed delay on entry")
      (_
       (let* ((new-time
              (or time
                  (org-read-date
                   nil nil nil
                   "Delay until when?")))
              (formatted (format "<%s>" new-time)))
         (if (and (eq 'task (opr/get-type))
                  (org-entry-get (point) "SCHEDULED")
                  nil) ;; Disable this for now
             (org-schedule arg new-time)
           (org-entry-put (point) "DELAYED" formatted)
           (format "Delayed until %s" formatted)))))))

(defun org-delay-until-next-week ()
  (interactive)
  (let ((time (org-read-date nil nil "fri")))
    (org-delay nil time)))

(define-key org-mode-map (kbd "C-c <C-tab>") #'org-delay)
(define-key org-mode-map (kbd "C-c <tab>") #'org-delay-until-next-week)

(defun org-agenda-delay (arg &optional time)
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (setq ts (org-delay arg time)))
      (org-agenda-show-new-time marker ts " D"))
    (next-line)
    (message "%s" ts)))

(defun org-agenda-delay-until-next-week (arg)
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (setq ts (if arg
                     (org-delay arg)
                   (org-delay-until-next-week))))
      (org-agenda-show-new-time marker ts " D"))
    (message "%s" ts)
    (next-line)))

(define-prefix-command '*org-delay-map*)
(define-key org-agenda-mode-map (kbd "d") '*org-delay-map*)

(define-key *org-delay-map* (kbd "d") #'org-agenda-delay-until-next-week)
(define-key *org-delay-map* (kbd "D") #'org-agenda-delay)
(define-key *org-delay-map* (kbd "u") (lambda () (interactive) (org-agenda-delay '(4))))


(provide 'org-delay)
;;; org-delay.el ends here
