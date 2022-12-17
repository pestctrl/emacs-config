;;; llvm-shared.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:31]

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

(defvar llvm-core-count 8)

(defun ll/ninja-build-tools (build-dir tools-list)
  (format "cd %s && ninja -j %d %s"
          build-dir llvm-core-count
          (string-join tools-list " ")))

(defun ll/get-tool (tool-regexp directories)
  (cl-mapcan #'(lambda (dir)
                 (directory-files dir t tool-regexp))
             directories))

(defvar ll/get-build-dir-fun
  #'(lambda ()
      "~/workspace/llvm-project/build/Release/"))

(defvar ll/get-clang-command-fun
  (lambda (compiler file action &optional rest)
    (format "%s %s %s %s"
            compiler
            file
            (concat
             (pcase action
               ('compile "-c ")
               ('assemble "-S ")
               ('preprocess "-E ")
               ('llvm-ir "-S -emit-llvm "))
             "-o -")
            " "
            (string-join rest " "))))

(provide 'llvm-shared)
;;; llvm-shared.el ends here
