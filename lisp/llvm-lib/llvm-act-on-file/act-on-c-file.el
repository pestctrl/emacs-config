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
(require 'lib-comp-dev)
(require 'action-map-lib)
(require 'anaphora)
(require 'make-tmp-output-file)

(defvar ll/c-file-action-map
  '((preprocess   :key ?e    :major-mode c-mode    :buffer-string "preprocess"         :description "pr[e]process"    :end-state pp-c)
    (diff         :key ?D    :major-mode nil       :buffer-string "diff"               :description "[D]iff"          :end-state asm)))

;; (executable   :key ?\^M  :major-mode nil       :buffer-string "executable"         :description "[RET]Executable"          :compiler-action executable)

(defun ll/ensure-clang-binary-built (dir)
  ;; TODO: assumed build-dir constant, should take as argument and prompt
  ;; further up
  (comp-dev/build-target dir '("clang")))

(defun ll/get-c-action-map ()
  (append
   (comp-dev/get-c-action-table
    (comp-dev/get-config))
   ll/c-file-action-map))

(defun ll/clang-output-disassemble-command (file)
  (let ((compiler (lls/prompt-tool "clang$"))
        (tmp-file (make-temp-file (file-name-sans-extension (file-name-nondirectory file)))))
    (string-join
     (list (lls/get-clang-command-fun :compiler compiler
                                      :file file
                                      :action 'compile
                                      :output tmp-file)
           (lls/get-dis-command-fun tmp-file nil))
     " && ")))

(defun ll/read-pass-name (prompt)
  (completing-read
   prompt
   '("pipeliner"
     "mi-loop-unroll"
     "machine-scheduler"
     "twoaddressinstruction"
     "greedy"
     "argo-partitioner"
     "finalize-isel"
     "early-ifcvt"
     "early-if-predicator")))

(defun ll/build-clang-command (file action &optional output)
  (if (eq action 'output-dis)
      (ll/clang-output-disassemble-command file)
    (let ((end (aml/get-map-prop
                (ll/get-c-action-map)
                action :end-state))
          (compiler (lls/prompt-tool (comp-dev/tool-name (comp-dev/get-config) 'compiler))))
      (string-join
       (list
        (when (y-or-n-p "Would you like to `rr record`? ")
          "rr record ")
        (-->
         (comp-dev/get-config)
         (comp-dev/process-file
          it 'c end compiler file output
          (list
           (pcase action
             ('debug (format "-mllvm -debug-only=%s" (ll/read-pass-name "Which pass? ")))
             ('before-after (let ((pass (ll/read-pass-name "Which pass? ")))
                              (format "-mllvm -print-before=%s -mllvm -print-after=%s" pass pass)))
             ('changed "-mllvm -print-before-all")))))
        " ")
       " "))))

(defun ll/buffer-has-include-error (buffer)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (let ((r
             (rx (and "fatal error: '" (group (+ (not space))) "' file not found"))))
        (aprog1 (re-search-forward r nil t)
          (when it
            (message "Couldn't find '%s' header file" (match-string 1))))))))

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

(defun ll/diff-on-optionset (file action)
  (let ((comm (ll/build-clang-command file action))
        (extra-option (read-string "Extra option? "))
        (pipe (if (y-or-n-p "Diff assembly (y) or debug (n)? ")
                  ">" "2>")))
    (when (save-window-excursion
            (not
             (and (zerop (shell-command (format "%s %s /tmp/no-option.asm" comm pipe)))
                  (zerop (shell-command (format "%s %s %s /tmp/yes-option.asm" comm extra-option pipe))))))
      (error "One of the commands failed"))
    (ediff-files "/tmp/no-option.asm" "/tmp/yes-option.asm")))

(defun ll/diff-on-compiler (file action)
  (let ((comm (ll/build-clang-command file action))
        (second-command (ll/build-clang-command file action))
        (pipe (if (y-or-n-p "Diff assembly (y) or debug (n)? ")
                  ">" "2>")))
    (when (save-window-excursion
            (not
             (and (zerop (shell-command (format "%s %s /tmp/old.asm" comm pipe)))
                  (zerop (shell-command (format "%s %s /tmp/new.asm" second-command pipe))))))
      (error "One of the commands failed"))
    (ediff-files "/tmp/old.asm" "/tmp/new.asm")))

(defun ll/diff-c-on-two-compilations (file action)
  (let ((choice (read-char "Diff compiler versions (v) or diff options (o)? ")))
    (pcase choice
      (?v (ll/diff-on-compiler file action))
      (?o (ll/diff-on-optionset file action))
      (_ (error "Invalid choice")))))

(defun ll/act-on-c-file (file)
  (let* ((action-map (ll/get-c-action-map))
         (action (aml/read-action-map action-map))
         (output (ll/make-tmp-file
                  file
                  (cond
                   ((eq 'asm
                        (aml/get-map-prop action-map action
                                          :end-state))
                    ".S")
                   (t ".ll")))))
    (if (eq action 'diff)
        (ll/diff-c-on-two-compilations file action)
      (let ((comm (ll/build-clang-command file action output)))
        (aprog1
            (compilation-start
             comm
             (aml/get-map-prop action-map action :major-mode)
             (lambda (x)
               (format "*%s-%s*"
                       (file-name-nondirectory file)
                       (aml/get-map-prop action-map action
                                         :buffer-string))))
          (with-current-buffer it
            (setq ll/act-on-file-output output)))))))

(defun ll/diff-before-after ()
  (interactive)
  (let (b1 e1 b2 e2)
    (compilation-minor-mode -1)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "IR Dump Before")
      (beginning-of-line)
      (setq b1 (point))
      (re-search-forward "# End machine code")
      (end-of-line)
      (setq e1 (point))
      (re-search-forward "IR Dump After")
      (beginning-of-line)
      (setq b2 (point))
      (re-search-forward "# End machine code")
      (end-of-line)
      (setq e2 (point)))

    (ediff-regions-internal
     (current-buffer) b1 e1 (current-buffer) b2 e2
     nil nil nil nil)))

(provide 'act-on-c-file)
;;; act-on-c-file.el ends here
