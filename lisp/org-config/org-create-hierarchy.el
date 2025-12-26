;;; org-create-hierarchy.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-12-26 10:33]

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

(defun org-create-hierarchy (time format-strings &optional depth reverse)
  (cl-labels ((create-hierarchy (time format-strings depth)
                (when-let* ((fmt (car format-strings))
                            (timestr (format-time-string fmt time)))
                  (let ((str (concat (make-string depth ?*) " " timestr)))
                    (or (prog1 (search-forward str nil t)
                          (end-of-line))
                        (progn
                          (unless reverse
                            (when (org-get-next-sibling)
                              (previous-line)
                              (end-of-line)))
                          (insert "\n" str)))
                    (create-hierarchy time (cdr format-strings) (1+ depth))))))
    (goto-char (point-min))
    (create-hierarchy time format-strings (or depth 1))))

;; (org-create-hierarchy (current-time) '("%Y" "%B"))

(provide 'org-create-hierarchy)
;;; org-create-hierarchy.el ends here
