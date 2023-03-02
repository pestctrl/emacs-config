;;; act-on-llvm-source-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-01-19 10:52]

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

(defun ll/is-llvm-source-file (file)
  (string-match-p ".*llvm-project.*" file))

(defun ll/act-on-llvm-source-file (file)
  (let ((dir (lls/get-llvm-build-dir)))
    (--> file
         (list (format "touch %s" it)
               (lls/ninja-build-tools dir (list (concat it "^")) t))
         (string-join it " && ")
         (compilation-start it nil
                            (lambda (_)
                              (format "*ninja-%s*"
                                      (file-name-nondirectory file)))))))

(provide 'act-on-llvm-source-file)
;;; act-on-llvm-source-file.el ends here
