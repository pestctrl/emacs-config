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

(defun ll/ensure-clang-binary-built (dir)
  ;; TODO: assumed build-dir constant, should take as argument and prompt
  ;; further up
  (lls/ninja-build-tools dir '("clang")))

(defun ll/build-clang-command (file action)
  (let ((compiler-action (aml/get-map-prop ll/c-file-action-map action :compiler-action))
        (compiler (lls/prompt-tool "clang$")))
    (concat ;; (ll/ensure-clang-binary-built
            ;;  (file-name-directory
            ;;   (directory-file-name
            ;;    (file-name-directory compiler))))
            ;; " && "
            (string-join
             (list (funcall lls/get-clang-command-fun compiler file compiler-action)
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
             (rx (and "#include "
                      (or "\"" "<") (group (+ (not space))) (or "\"" ">")))))
        (when-let ((res (re-search-forward r nil t)))
          (message "Couldn't find %s header file" (match-string 1))
          res)))))

(defun ll/add-include-file ()
  (interactive)
  (when (not (or compilation-minor-mode
                 (eq major-mode
                     'compilation-mode)))
    (user-error "Wrong buffer idiot"))

  (when (not (ll/buffer-has-include-error (current-buffer)))
    (user-error "The problem is not an include file..."))

  (let ((new-dir (read-directory-name "directory? ")))
    (setq compilation-arguments
          (cons (concat (car compilation-arguments)
                        (format " -I%s "
                                new-dir))
                (cdr compilation-arguments)))
    (call-interactively #'recompile)))

(defun ll/c-file-sentinel (buffer msg)
  (when (and (string-match-p "exited abnormally with code 1" msg)
             (ll/buffer-has-include-error buffer))
    (when (y-or-n-p "Add another include directory? ")
      (let ((new-dir (read-directory-name "directory? ")))
        (with-current-buffer buffer
          (setq compilation-arguments
                (cons (concat (car compilation-arguments)
                              (format " -I%s "
                                      new-dir))
                      (cdr compilation-arguments)))
          (call-interactively #'recompile))))))

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
           ;; TODO: This doesn't ACTUALLY survive a call to #'recompile
           (make-variable-buffer-local 'compilation-finish-functions)
           (add-to-list 'compilation-finish-functions
                        #'ll/c-file-sentinel)))))

(provide 'act-on-c-file)
;;; act-on-c-file.el ends here
