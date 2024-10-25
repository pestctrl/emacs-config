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
    (run-pass     :key ?o  :major-mode llvm-mode :buffer-string "run-%s"  :description "run-[o]ne-pass")
    (stop-after   :key ?a  :major-mode llvm-mode :buffer-string "stop-after-%s"  :description "stop-[a]fter")
    (stop-before  :key ?b  :major-mode llvm-mode :buffer-string "stop-before-%s" :description "stop-[b]efore")
    (start-after  :key ?A  :major-mode llvm-mode :buffer-string "start-after-%s"  :description "start-[A]fter")
    (start-before :key ?B  :major-mode llvm-mode :buffer-string "start-before-%s" :description "start-[B]efore")))

(defun ll/build-llc-command (file action &optional output pass)
  (lls/get-llc-command-fun :file file :action action :output output :pass pass
                           :llc (lls/prompt-tool "llc$")))

(defun ll/act-on-ll-file (file)
  (let* ((action (aml/read-action-map ll/ll-file-action-map))
         (pass (when (member action '(stop-after stop-before start-before start-after run-pass))
                 (ll/read-pass)))
         ;; I just recently noticed that the default directory is changing, but
         ;; I don't know what changed. Should investigate later.
         (default-directory (file-name-directory file))
         (output (make-temp-file
                  (concat (file-name-sans-extension file) "-")
                  nil ".mir")))
    (aprog1
        (compilation-start
         (ll/build-llc-command file action output pass)
         (aml/get-map-prop ll/ll-file-action-map action :major-mode)
         (lambda (_)
           (format "*%s-%s*"
                   (file-name-nondirectory file)
                   (let ((str (aml/get-map-prop ll/ll-file-action-map action :buffer-string)))
                     (if (not (member action
                                      (list
                                       'stop-before 'stop-after
                                       'start-before 'start-after
                                       'run-pass)))
                         str
                       (format str pass))))))
      (with-current-buffer it
        (setq ll/act-on-file-output output)))))

(provide 'act-on-ll-file)
;;; act-on-ll-file.el ends here
