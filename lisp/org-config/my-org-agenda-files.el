;;; my-org-agenda-files.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-06 18:47]

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
(require 'my-plaintext-files)

(defconst my/org-folder
  (my/plaintext-file "org"))

(defconst my/agenda-folder
  (my/plaintext-file "org/agenda"))

(defun my/org-file (str)
  (if (not (string-match-p "\\.gpg$" str))
      (my/plaintext-file (concat (file-name-as-directory "org") str))
    (let ((path
           (--> my/plaintext-object-folder
                (expand-file-name "gpg" it)
                (expand-file-name str it))))
      (when (file-exists-p path)
        path))))

(my/org-file "entries/reviews.gpg")

(defun my/agenda-file (str)
  (my/org-file (concat (file-name-as-directory "agenda") str)))

(defconst my/non-agenda-files
  `(,(my/agenda-file "datetree.org")
    ,(my/agenda-file "reference.org")
    ,(my/agenda-file "leisure.org")
    ,(my/org-file "entries/journal.gpg")
    ,(my/org-file "entries/reviews.gpg")))

(defconst my/aux-refile-files
  `(,(my/agenda-file "vrchat_things.org")
    ,(my/org-file "journal2.gpg")))

(custom-set-variables
 `(org-agenda-files
   '(,(my/agenda-file "plan.org")
     ,(my/agenda-file "thoughts.org")
     ,(my/agenda-file "refile.org")
     ,(my/agenda-file "sandbox.org")
     ,(my/agenda-file "dev.org")
     ,(my/agenda-file "prod.org")
     ,(my/agenda-file "habits.org")
     ,(my/agenda-file "calendar.org"))))

(defconst my/all-agenda-files
  (cons (my/agenda-file "eternal.org")
        org-agenda-files))

;; TODO fix org-ql-refile's assumption about symbols
(defun my/all-agenda-files ()
  my/all-agenda-files)

(defun my/aux-refile-files ()
  my/aux-refile-files)

(setq my/all-agenda-files
      (mapcar #'(lambda (x)
                  (replace-regexp-in-string
                   "/home/benson/MEGA/"
                   (expand-file-name "~/") x))
              (mapcar #'(lambda (x)
                          (replace-regexp-in-string
                           "/mnt/c/Users/Benson/Documents/MEGAsync/"
                           (expand-file-name "~/") x))
                      my/all-agenda-files)))

(require 'org-id)

(setq org-id-locations-file (my/org-file ".org-id-locations"))

(provide 'my-org-agenda-files)
;;; my-org-agenda-files.el ends here
