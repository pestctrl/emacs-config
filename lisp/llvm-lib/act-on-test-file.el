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
               '("ll" "c"))
       (string-match-p ".*llvm-project.*test.*" file)))

(defun ll/get-test-run-commands (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (save-excursion
      (goto-char (point-min))
      (let ((r (rx line-start (+ (or "|" ";" "/")) (+ " ") "RUN:" (+ space) (group(+ nonl) line-end)))
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
       (mapcan #'(lambda (x) (string-split x "|")))
       (mapcar #'string-trim)
       (mapcar #'(lambda (x)
                   (let ((str (car (string-split x " "))))
                     (if (string= str "%clang_cc1")
                         "clang"
                       str))))
       (seq-uniq)))

(defun ll/test-ensure-binary-built (file)
  (let ((dir (funcall lls/get-build-dir-fun))
        (tools (ll/get-required-binaries-for-test file)))
    (lls/ninja-build-tools dir tools)))

(defun ll/build-test-command (file action)
  (mapconcat #'identity
             `(,(ll/test-ensure-binary-built file)
               ,(format "./bin/llvm-lit %s %s"
                        (pcase action
                          ('verbose "-v")
                          ('all "-a")
                          (_ ""))
                        file))
             " && "))

(defun ll/act-on-test-file (file)
  (let* ((action (aml/read-action-map ll/test-file-action-map " | ")))
    (compilation-start
     (ll/build-test-command file action)
     nil
     (lambda (_)
       (format "*test-%s-%s*"
               (file-name-nondirectory file)
               (aml/get-map-prop ll/test-file-action-map action :buffer-string))))))

(provide 'act-on-test-file)
;;; act-on-test-file.el ends here
