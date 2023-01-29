;;; org-delay-today.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-01-22 17:23]

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

(defun org-delay-today (arg)
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (error "Not supported yet")
    (pcase arg
      ('(4)
       (org-entry-delete (point) "NOT_TODAY")
       "Removed delay on entry")
      (_
       (when (eq 'project (opr/get-type))
         (let* ((new-time
                 (org-read-date nil nil "."))
                (formatted (format "[%s]" new-time)))
           (org-entry-put (point) "NOT_TODAY" formatted)
           formatted))))))

(defun org-agenda-delay-today (arg)
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
        (setq ts (org-delay-today arg)))
      (org-agenda-show-new-time marker ts " D"))
    (next-line)
    (message "%s" ts)))

(provide 'org-delay-today)
;;; org-delay-today.el ends here
