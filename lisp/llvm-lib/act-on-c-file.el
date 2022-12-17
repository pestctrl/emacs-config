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
(require 'action-map-lib)

;; (advice-add #'shell-command
;;             :filter-args
;;             #'my/shell-command-print-cmd-before)

(defun ll/get-tmp-output-file (file action)
  (let ((fname (file-name-nondirectory file)))
    (format "/tmp/%s"
            (format (pcase action
                      ('compile "%s.obj")
                      ('assemble "%s.S")
                      ('preprocess "%s.pp")
                      ('llvm-ir "%s.ll"))
                    fname))))

;; (defun ll/get-includes-dirs (target)
;;   (list "/scratch/benson/tools4/build/argo/product/linux/include/c"))

(defun ll/pop-up-commit-log-for-clang (tool)
  (interactive
   (list
    (completing-read
     "Which clang? "
     (ll/get-tool-for-target "clang$"))))
  (let ((r (rx line-start "TI clang version " (+ digit) (+ "." (+ digit)) " "
               "(ssh://git@bitbucket.itg.ti.com/code/llvm-project.git " (group (+ (any "0-9a-z"))) ")")))
    (with-temp-buffer
      (shell-command (format "%s --version" tool) (current-buffer))
      (beginning-of-buffer)
      (re-search-forward r)
      (let ((default-directory
             (expand-file-name "llvm_cgt/llvm-project"
                               (ll/current-tools-directory))))
        (magit-log-setup-buffer
         (list "HEAD" (match-string 1))
         (list "--graph" "--decorate" "-n20")
         nil)))))

;; (ll/kill-gdb-command "/scratch/benson/tools4/gurts/FREEBSD/ll/msun/s_scalbnf.c")
;; (ll/get-clang-command "/scratch/benson/tools4/gurts/FREEBSD/ll/msun/s_scalbnf.c" 'compile)

(defun ll/buffer-has-include-error (buffer)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward (rx (and "#include \"" (+ (not space)) "\"")) nil t))))

(defvar ll/c-file-action-map
  '((debug        :key ?d  :major-mode llvm-mode :buffer-string "debug"              :description "[d]ebug pass"             :compiler-action assemble)
    (assembly     :key ?a  :major-mode asm-mode  :buffer-string "assembly"           :description "[a]ssembly"               :compiler-action assemble)
    (preprocess   :key ?P  :major-mode c-mode    :buffer-string "preprocess"         :description "[l]lvm-ir"                :compiler-action preprocess)
    (LLVMIR       :key ?l  :major-mode llvm-mode :buffer-string "llvm-ir"            :description "[P]reprocess"             :compiler-action llvm-ir)
    (before-after :key ?p  :major-mode llvm-mode :buffer-string "print-before-after" :description "[p]rint before/after"     :compiler-action assemble)
    (changed      :key ?A  :major-mode llvm-mode :buffer-string "print-changed"      :description "print before/after [A]ll" :compiler-action assemble)))

(defun ll/build-clang-command (file action)
  (let ((compiler-action (aml/get-map-prop ll/c-file-action-map action :compiler-action))
        (compiler (completing-read "Which clang? "
                                   ;; TODO refactor constant
                                    (ll/get-tool "clang$" '("/usr/bin/")))))
    (concat (funcall ll/get-clang-command-fun compiler file compiler-action)
            (pcase action
              ('debug (format "-mllvm -debug-only=%s" (read-string "Which pass? ")))
              ('before-after (let ((pass (read-string "Which pass? ")))
                              (format "-mllvm -print-before=%s -mllvm -print-after=%s" pass pass)))
              ('changed "-mllvm -print-before-all"))
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
