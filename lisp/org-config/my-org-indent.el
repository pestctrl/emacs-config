;;; my-org-indent.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-06 18:35]

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

(setq org-startup-indented t)

(defun my/org-indent-prefixes ()
  "Compute prefix strings for regular text and headlines."
  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
                         (* (1- org-indent-indentation-per-level)
                            (1- n)))))
      ;; Headlines line prefixes.
      (let ((heading-prefix ""))
        (aset org-indent--heading-line-prefixes
              n
              (org-add-props heading-prefix nil 'face 'org-indent))
        ;; Inline tasks line prefixes
        (aset org-indent--inlinetask-line-prefixes
              n
              (cond ((<= n 1) "")
                    ((bound-and-true-p org-inlinetask-show-first-star)
                     (concat org-indent-inlinetask-first-star
                             (substring heading-prefix 1)))
                    (t (org-add-props heading-prefix nil 'face 'org-indent)))))
      ;; Text line prefixes.
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (if (< n 2) n
                                       (1+ indentation)) ?\s)
                        (and (> n 0)
                             (char-to-string org-indent-boundary-char)))
                nil 'face 'org-indent)))))


(advice-add #'org-indent--compute-prefixes
            :override
            #'my/org-indent-prefixes)

(provide 'my-org-indent)
;;; my-org-indent.el ends here
