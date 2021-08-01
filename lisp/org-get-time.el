;;; org-get-time.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2021-08-01 13:47]

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

(defun org-get-time-of-day (s &optional string mod24)
  "Check string S for a time of day.
If found, return it as a military time number between 0 and 2400.
If not found, return nil.
The optional STRING argument forces conversion into a 5 character wide string
HH:MM."
  (save-match-data
    (when
	(and
	 (or (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)\\([AaPp][Mm]\\)?\\> *" s)
	     (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\([AaPp][Mm]\\)\\> *" s))
	 (not (eq (get-text-property 1 'face s) 'org-link)))
      (let* ((h (string-to-number (match-string 1 s)))
	     (m (if (match-end 3) (string-to-number (match-string 3 s)) 0))
	     (ampm (when (match-end 4) (downcase (match-string 4 s))))
	     (am-p (equal ampm "am"))
	     (h1   (cond ((not ampm) h)
			 ((= h 12) (if am-p 0 12))
			 (t (+ h (if am-p 0 12)))))
	     (h2 (if (and string mod24 (not (and (= m 0) (= h1 24))))
		     (mod h1 24) h1))
	     (t0 (+ (* 100 h2) m))
	     (t1 (concat (if (>= h1 24) "+" " ")
			 (if (and org-agenda-time-leading-zero
				  (< t0 1000)) "0" "")
			 (if (< t0 100) "0" "")
			 (if (< t0 10)  "0" "")
			 (number-to-string t0))))
	(if string (concat (substring t1 -4 -2) ":" (substring t1 -2)) t0)))))

(provide 'org-get-time)
;;; org-get-time.el ends here
