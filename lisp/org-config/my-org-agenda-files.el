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
(ec/load-or-ask-file 'my/plaintext-folder "Where's the MEGASync plaintext directory? ")
(ec/load-or-ask-pred 'my/is-org-migration-folder "Is there a migration (source controlled) plaintext directory? ")

(when my/is-org-migration-folder
  (ec/load-or-ask-file 'my/plaintext-migration-folder "Where's the migration directory? "))

(defconst my/org-folder (expand-file-name "org" my/plaintext-folder))
(defconst my/org-migration-folder (and my/is-org-migration-folder (expand-file-name "org" my/plaintext-migration-folder)))

(defconst my/agenda-folder
  (or (and my/org-migration-folder
           (expand-file-name "org/agenda" my/org-migration-folder))
      (expand-file-name "org/" my/org-folder)))

(defun my/org-file (str)
  (let (result)
    (setq result (expand-file-name str my/org-folder))
    (when my/is-org-migration-folder
      (let ((folder (expand-file-name str my/org-migration-folder)))
        (when (file-exists-p folder)
          (setq result folder))))
    result))

(defun my/agenda-file (str)
  (my/org-file (concat (file-name-as-directory "agenda") str)))

(defconst my/non-agenda-files
  `(,(my/org-file "entries/reviews.gpg") 
    ,(my/agenda-file "datetree.org") 
    ,(my/agenda-file "reference.org") 
    ,(my/org-file "entries/journal.gpg")))

(defconst my/all-agenda-files
  (cons (my/agenda-file "eternal.org")
        org-agenda-files))

(setq org-agenda-files
      `(,(my/agenda-file "plan.org")
        ,(my/agenda-file "refile.org")
        ,(my/agenda-file "sandbox.org")
        ,(my/agenda-file "dev.org")))

(provide 'my-org-agenda-files)
;;; my-org-agenda-files.el ends here
