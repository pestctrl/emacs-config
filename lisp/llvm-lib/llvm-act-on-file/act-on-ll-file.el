;;; act-on-ll-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:14]

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

(defvar ll/ll-file-action-map
  '((assembly     :key ?a  :major-mode asm-mode  :buffer-string "assembly"  :description "[a]ssembly")
    (stop-after   :key ?s  :major-mode llvm-mode :buffer-string "after-%s"  :description "[s]top-after")
    (stop-before  :key ?S  :major-mode llvm-mode :buffer-string "before-%s" :description "[S]top-before")))

(defun ll/build-llc-command (file _action)
  (funcall lls/get-clang-command-fun file action))

(defun ll/act-on-ll-file (file)
  (let* ((action (aml/read-action-map ll/ll-file-action-map))
         (stop (when (member action '(stop-after stop-before))
                 (read-string "Which pass? "))))
    (compilation-start
     (concat
      (ll/build-llc-command file action)
      (when (member action '(stop-after stop-before))
        (format "--%s=%s" (symbol-name action) stop)))
     (aml/get-map-prop ll/ll-file-action-map action :major-mode)
     (lambda (_)
       (format "*%s-%s*"
               (file-name-nondirectory file)
               (let ((str (aml/get-map-prop ll/ll-file-action-map action :buffer-string)))
                 (if (not (member action '(stop-before stop-after)))
                     str
                   (format str stop))))))))

(provide 'act-on-ll-file)
;;; act-on-ll-file.el ends here
