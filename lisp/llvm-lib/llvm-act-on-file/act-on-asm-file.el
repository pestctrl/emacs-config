;;; act-on-asm-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-07-12 17:11]

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
(require 'llvm-shared)
(require 'action-map-lib)

(defvar ll/asm-file-action-map
  '((round-trip   :key ?a  :major-mode asm-mode  :buffer-string "round-trip"  :description "[a]ssembly round trip")))

(defun ll/build-asm-command (file action)
  (let ((temp-file (make-temp-file
                    (concat (file-name-nondirectory file) "-")
                    nil ".o")))
    (string-join
     (list
      (string-join
       (list
        (lls/prompt-tool "clang$")
        file
        "-target c29 -c -o"
        temp-file)
       " ")
      (string-join
       (list
        (lls/prompt-tool "objdump$")
        "-d"
        temp-file)
       " "))
     " && ")))

(defun ll/act-on-asm-file (file)
  (let* ((action (aml/read-action-map ll/asm-file-action-map)))
    (compilation-start
     (ll/build-asm-command file action)
     (aml/get-map-prop ll/ll-file-action-map action :major-mode)
     (lambda (_)
       (format "*%s-%s*"
               (file-name-nondirectory file)
               (let ((str (aml/get-map-prop ll/ll-file-action-map action :buffer-string)))
                 (if (not (member action '(stop-before stop-after)))
                     str
                   (format str stop))))))))

(provide 'act-on-asm-file)
;;; act-on-asm-file.el ends here
