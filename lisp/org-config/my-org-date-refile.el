;;; my-org-date-refile.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-09-08 15:51]

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

;; (format-time-string "%d" (org-read-date t t))

(defun modr/parse-timestamp (string)
  (org-read-date t t string))

(defun modr/create-single-datetree (subtree-marker modr)
  (with-current-buffer (marker-buffer subtree-marker)
    (save-excursion
      (goto-char (marker-position subtree-marker))
      (save-restriction
        (org-narrow-to-subtree)
        (if (re-search-forward (rx "** "
                                   (literal
                                    (format-time-string "%b %d, %Y" modr))))
            (point))))))

(provide 'my-org-date-refile)
;;; my-org-date-refile.el ends here
