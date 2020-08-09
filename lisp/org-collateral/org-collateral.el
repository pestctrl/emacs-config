;;; org-collateral.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-08-09 15:57]

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

(defun org-collateral-new-file (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (insert "\n")
    (insert "* Notes\n\n")
    (insert "* Journal\n\n")
    (insert "* Archive  :ARCHIVE:")
    (save-buffer)
    (kill-buffer)))

(defun org-collateral-new ()
  (save-excursion
    (org-back-to-heading)
    (let* ((date-string (format-time-string "%Y-%m-%d"))
           (name-prompted (replace-regexp-in-string " " "_" (read-string "File name? ")))
           (file-name (format "./col/%s-%s.org" date-string name-prompted)))
      (org-entry-put (point) "COLLATERAL" file-name)
      (org-collateral-new-file file-name)
      file-name)))

(defun org-collateral-goto ()
  (interactive)
  (let ((fname (or (org-entry-get (point) "COLLATERAL")
                   (org-collateral-new))))
    (find-file fname)))

(defun org-collateral-find ()
  (interactive)
  (save-excursion
    (let (fname)
      (while (progn
               (and (not (setf fname
                               (org-entry-get (point) "COLLATERAL")))
                    (org-up-heading-safe))))
      (if fname
          (find-file fname)
        (message "No collateral found")))))

(defun org-collateral-delete ()
  (interactive)
  (when-let (coll (org-entry-get (point) "COLLATERAL"))
    (delete-file coll)
    (org-entry-delete (point) "COLLATERAL")))

(defun org-collateral-clock-jump ()
  (interactive)
  (org-clock-goto)
  (org-collateral-find))

(define-key *root-map* (kbd "J") #'org-collateral-clock-jump)

(provide 'org-collateral)
;;; org-collateral.el ends here
