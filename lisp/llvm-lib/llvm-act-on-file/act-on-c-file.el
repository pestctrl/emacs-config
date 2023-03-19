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
(require 'anaphora)

(defvar ll/c-file-action-map
  '((debug        :key ?d    :major-mode llvm-mode :buffer-string "debug"              :description "[d]ebug pass"             :compiler-action assemble)
    (assembly     :key ?a    :major-mode asm-mode  :buffer-string "assembly"           :description "[a]ssembly"               :compiler-action assemble)
    (output-dis   :key ?A    :major-mode asm-mode  :buffer-string "dissasembly"        :description "output-dis[A]ssemble"     :compiler-action nil)
    (preprocess   :key ?e    :major-mode c-mode    :buffer-string "preprocess"         :description "pr[e]process"             :compiler-action preprocess)
    (LLVMIR       :key ?l    :major-mode llvm-mode :buffer-string "llvm-ir"            :description "[l]lvm-ir"                :compiler-action llvm-ir)
    (before-after :key ?p    :major-mode llvm-mode :buffer-string "print-before-after" :description "[p]rint before/after"     :compiler-action assemble)
    (changed      :key ?P    :major-mode llvm-mode :buffer-string "print-changed"      :description "[P]rint before/after all" :compiler-action assemble)
    (executable   :key ?\^M  :major-mode nil       :buffer-string "executable"         :description "[RET]Executable"               :compiler-action executable)))

(defun ll/ensure-clang-binary-built (dir)
  ;; TODO: assumed build-dir constant, should take as argument and prompt
  ;; further up
  (lls/ninja-build-tools dir '("clang")))

(defun ll/clang-output-disassemble-command (file)
  (let ((compiler (lls/prompt-tool "clang$" (lls/get-llvm-bin-dir)))
        (tmp-file (make-temp-file (file-name-sans-extension (file-name-nondirectory file)))))
    (string-join
     (list (lls/get-clang-command-fun compiler file 'compile
                                      :output tmp-file)
           (lls/get-dis-command-fun tmp-file nil))
     " && ")))

(defun ll/build-clang-command (file action)
  (if (eq action 'output-dis)
      (ll/clang-output-disassemble-command file)
    (let ((compiler-action (aml/get-map-prop ll/c-file-action-map action :compiler-action))
          (compiler (lls/prompt-tool "clang$")))
      (string-join
       (list (lls/get-clang-command-fun compiler file compiler-action)
             (pcase action
               ('debug (format "-mllvm -debug-only=%s" (read-string "Which pass? ")))
               ('before-after (let ((pass (read-string "Which pass? ")))
                                (format "-mllvm -print-before=%s -mllvm -print-after=%s" pass pass)))
               ('changed "-mllvm -print-before-all"))
             " ")
       " "))))

(defun ll/buffer-has-include-error (buffer)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (let ((r
             (rx (and "fatal error: '" (group (+ (not space))) "' file not found"))))
        (aprog1 (re-search-forward r nil t)
          (message "Couldn't find '%s' header file" (match-string 1)))))))

(defun ll/add-include-folder-to-command (command)
  (let ((directory (read-directory-name "directory? ")))
    (string-replace "-I" (format "-I%s -I"
                                 directory)
                    command)))

(defun ll/c-file-sentinel (buffer msg)
  (when (and (string-match-p "exited abnormally with code 1" msg)
             (ll/buffer-has-include-error buffer))
    (when (y-or-n-p "Add another include directory? ")
      (with-current-buffer buffer
        (setq compilation-arguments
              (cons (ll/add-include-folder-to-command
                     (car compilation-arguments))
                    (cdr compilation-arguments)))
        (call-interactively #'recompile)))))

(defun ll/act-on-c-file (file)
  (let* ((action (aml/read-action-map ll/c-file-action-map)))
    (--> (ll/build-clang-command file action)
         (compilation-start
          it
          (aml/get-map-prop ll/c-file-action-map action :major-mode)
          (lambda (x)
            (format "*%s-%s*"
                    (file-name-nondirectory file)
                    (aml/get-map-prop ll/c-file-action-map action
                                      :buffer-string))))
         (with-current-buffer it
           (add-hook 'compilation-finish-local-sticky
                     #'ll/c-file-sentinel)))))

(provide 'act-on-c-file)
;;; act-on-c-file.el ends here
