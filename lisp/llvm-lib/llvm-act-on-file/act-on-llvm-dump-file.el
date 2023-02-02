;;; act-on-llvm-dump-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-02-01 17:51]

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

(defun ll/is-dump-file (fname)
  (and (string= "/tmp/"
                (file-name-directory fname))
       (string-match-p (rx "/" (+ (not "/")) "-" (= 6 alphanumeric) "." (+ anything))
                       fname)))

(defun ll/dump-to-sh-file (fname)
  (expand-file-name
   (format "%s.sh"
           (-> fname
               (file-name-nondirectory)
               (file-name-sans-extension)))
   (file-name-directory fname)))

(defun ll/act-on-llvm-dump-file (fname)
  (message "You haven't implemented me yet, stoopid"))

(provide 'act-on-llvm-dump-file)
;;; act-on-llvm-dump-file.el ends here
