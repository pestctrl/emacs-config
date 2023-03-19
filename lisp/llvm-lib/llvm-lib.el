;;; llvm-lib.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-17 12:54]

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
(require 'llvm-act-on-file)
(require 'llvm-build-tool)
(require 'llvm-gdb-command)
(require 'llvm-show-instr-info)
(require 'llvm-jump-to-tablegen)

(define-prefix-command '*llvm-map*)
(define-key *root-map* (kbd "C-w") '*llvm-map*)

(define-key *llvm-map* (kbd "a") #'ll/act-on-file)
(define-key *llvm-map* (kbd "c") #'ll/llvm-build-tool)
(define-key *llvm-map* (kbd "M-w") #'ll/kill-gdb-command)
(define-key *llvm-map* (kbd "i") #'ll/prompt-for-instr-info)
(define-key *llvm-map* (kbd "t") #'ll/jump-to-tablegen)

(provide 'llvm-lib)
;;; llvm-lib.el ends here
