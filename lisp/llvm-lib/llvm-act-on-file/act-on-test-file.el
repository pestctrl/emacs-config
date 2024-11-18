;;; act-on-test-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:17]

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
(require 'dash)

(defvar ll/test-file-action-map
  '((normal       :key ?\^M :major-mode nil :buffer-string "run"  :description "[RET] run test")
    (verbose      :key ?v   :major-mode nil :buffer-string "verbose"  :description "[v]erbose")
    (all          :key ?a   :major-mode nil :buffer-string "VERBOSE" :description "[a]ll-output")
    (test-action  :key ?A   :major-mode asm-mode :buffer-string "test-action" :description "test [A]ction")))

(defun ll/is-test-file (file)
  (and (member (file-name-extension file)
               '("ll" "c" "mir" "s"))
       (string-match-p ".*llvm-project.*test.*" file)))

(defun ll/get-test-run-commands (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (save-excursion
      (goto-char (point-min))
      (let ((r (rx line-start (+ (or "|" ";" "/" "#")) (+ " ") "RUN:" (+ space) (group(+ nonl) line-end)))
            (temp "")
            l)
        (while (re-search-forward r nil t)
          (let ((str (match-string 1)))
            (set-text-properties 0 (length str) nil str)
            (if (string-match (rx line-start (group (+ nonl)) "\\" line-end) str)
                (setq temp (concat temp (match-string 1 str)))
              (setq temp (concat temp str))
              (push temp l)
              (setq temp ""))))
        l))))

(defun ll/get-required-binaries-for-test (file)
  (->> (ll/get-test-run-commands file)
       (mapcan #'(lambda (x) (split-string x "|")))
       (mapcar #'string-trim)
       (mapcar #'(lambda (x)
                   (let ((str (car (split-string x " "))))
                     (if (member str '("%clang" "%clang_cc1"))
                         "clang"
                       str))))
       (seq-uniq)))

(defun ll/test-ensure-binary-built (file)
  ;; TODO: assumed build-dir constant, should take as argument and prompt
  ;; further up
  (let ((dir (lls/get-llvm-build-dir))
        (tools (ll/get-required-binaries-for-test file)))
    (lls/ninja-build-tools dir tools)))

(defun ll/build-lit-command (file action)
  (format "%s %s %s"
          (lls/prompt-tool "llvm-lit")
          (pcase action
            ('verbose "-v")
            ('all "-a")
            (_ ""))
          file ))

(defun ll/get-cc1-string (path)
  (concat "clang -cc1 "
          "-internal-isystem " (expand-file-name "lib/clang/16.0.0/include" path)
          " -nostdsysteminc"))

(defun ll/prompt-test-action (file action)
  ;; TODO: assumed build-dir constant, should take as argument and prompt
  ;; further up
  (let* ((lookups (make-hash-table :test #'equal))
         (commands
          (--> (ll/get-test-run-commands file)
               ;; TODO: assumed that first command will DWIM
               (mapcar #'(lambda (x) (car (split-string x "|"))) it)
               (mapcar #'(lambda (x) (string-trim x)) it)
               ;; TODO: assuming all commands will be llc
               (mapcar #'(lambda (x)
                           (let ((res x))
                             (setq res (string-replace "not " "" res))
                             (string-match (rx line-start
                                               (group
                                                (optional "%")
                                                (+ (not space))))
                                           x)
                             (setq res
                                   (apcase (match-string 1 x)
                                     ("%clang_cc1"
                                      (replace-match (ll/get-cc1-string it) nil nil x 1))
                                     ("%clang"
                                      (replace-match "clang" nil nil x 1))
                                     (t res)))
                             (string-match (rx line-start
                                               (group (or "clang" "llc" "llvm-mc"))
                                               " ")
                                           res)
                             (let* ((tool (match-string 1 res))
                                    (resolved
                                     (save-match-data
                                       (or (gethash tool lookups)
                                           (aprog1 (lls/prompt-tool (concat tool "$"))
                                             (puthash tool it lookups))))))
                               (replace-match resolved nil nil res 1))))
                       it)
               (mapcar #'(lambda (x)
                           (string-replace "%s" file x))
                       it))))
    (my/completing-read "Which command? " commands)))

(defun ll/build-test-command (file action)
  (let (actions)
    (if (eq action 'test-action)
        (push (ll/prompt-test-action file action) actions)
      (push (ll/build-lit-command file action) actions))
    (when (y-or-n-p "Rebuild before running test? ")
      (push (ll/test-ensure-binary-built file) actions))
    (mapconcat #'identity
               actions
               " && ")))

(defun ll/test-get-missing (buffer)
  (interactive
   (list (current-buffer)))
  (let ((r
         (rx "Did not find "
             (group (+ (any alphanumeric "-")))))
        tools)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward r nil t)
          (let ((str (match-string 1) ))
            (set-text-properties 0 (length str) nil str)
            (push str tools)))))
    (when (called-interactively-p)
      (message (string-join tools ", ")))
    tools))

(defun ll/test-ensure-missing (buffer msg)
  (let ((tools (ll/test-get-missing buffer)))
    (when tools
      (-->
       ;; TODO: Assuming debug folder, (lls/get-llvm-build-dir) doesn't work
       (lls/ninja-build-tools "/scratch/benson/tools3/llvm_cgt/build/Debug/llvm" (seq-uniq tools))
       (compilation-start
        it
        nil
        (lambda (_)
          (format "*ensure-tools-%s*" (buffer-name buffer))))
       (aprog1 it
         (with-current-buffer it
           (add-hook 'compilation-finish-local-transient
                     (lambda (buf msg)
                       (delete-window (get-buffer-window buf))
                       (display-buffer buffer)
                       (with-current-buffer buffer
                         (call-interactively #'recompile))))))))))

(defun ll/act-on-test-file (file)
  (let* ((action (aml/read-action-map ll/test-file-action-map " | ")))
    (--> (ll/build-test-command file action)
         (compilation-start
          it
          nil
          (lambda (_)
            (format "*test-%s-%s*"
                    (file-name-nondirectory file)
                    (aml/get-map-prop ll/test-file-action-map action :buffer-string))))
         (aprog1 it
           (with-current-buffer it
             (add-hook 'compilation-finish-local-sticky
                       #'ll/test-ensure-missing))))))

(provide 'act-on-test-file)
;;; act-on-test-file.el ends here
