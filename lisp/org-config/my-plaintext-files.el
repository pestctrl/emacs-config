;;; my-plaintext-files.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2021-09-26 17:22]

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
(ec/load-or-ask-pred 'my/is-plaintext-migration-folder "Is there a migration (source controlled) plaintext directory? ")

(when my/is-plaintext-migration-folder
  (ec/load-or-ask-file 'my/plaintext-migration-folder "Where's the migration directory? "))

(defun my/plaintext-file (str)
  (let (result)
    (setq result (expand-file-name str my/plaintext-folder))
    (when my/is-plaintext-migration-folder
      (let ((folder (expand-file-name str my/plaintext-migration-folder)))
        (when (file-exists-p folder)
          (setq result folder))))
    result))

(provide 'my-plaintext-files)
;;; my-plaintext-files.el ends here
