;;; act-on-c-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 18:54]

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
(require 'llvm-ir-mode)
(require 'llvm-shared)
(require 'action-map-lib)

(defvar ll/c-file-action-map
  '((debug        :key ?d  :major-mode llvm-mode :buffer-string "debug"              :description "[d]ebug pass"             :compiler-action assemble)
    (assembly     :key ?a  :major-mode asm-mode  :buffer-string "assembly"           :description "[a]ssembly"               :compiler-action assemble)
    (preprocess   :key ?P  :major-mode c-mode    :buffer-string "preprocess"         :description "[l]lvm-ir"                :compiler-action preprocess)
    (LLVMIR       :key ?l  :major-mode llvm-mode :buffer-string "llvm-ir"            :description "[P]reprocess"             :compiler-action llvm-ir)
    (before-after :key ?p  :major-mode llvm-mode :buffer-string "print-before-after" :description "[p]rint before/after"     :compiler-action assemble)
    (changed      :key ?A  :major-mode llvm-mode :buffer-string "print-changed"      :description "print before/after [A]ll" :compiler-action assemble)))

(defun ll/build-clang-command (file action)
  (let ((compiler-action (aml/get-map-prop ll/c-file-action-map action :compiler-action))
        (compiler (completing-read
                   "Which clang? "
                   (lls/get-tool "clang$"))))
    (string-join
     (list (funcall lls/get-clang-command-fun compiler file compiler-action)
           (pcase action
             ('debug (format "-mllvm -debug-only=%s" (read-string "Which pass? ")))
             ('before-after (let ((pass (read-string "Which pass? ")))
                              (format "-mllvm -print-before=%s -mllvm -print-after=%s" pass pass)))
             ('changed "-mllvm -print-before-all"))
           " ")
     " ")))

(defun ll/act-on-c-file (file)
  (let* ((action (aml/read-action-map ll/c-file-action-map)))
    (cl-assert action)
    (compilation-start
     (ll/build-clang-command file action)
     (aml/get-map-prop ll/c-file-action-map action :major-mode)
     (lambda (x)
       (format "*%s-%s*"
               (file-name-nondirectory file)
               (aml/get-map-prop ll/c-file-action-map action :buffer-string))))))

(provide 'act-on-c-file)
;;; act-on-c-file.el ends here
