;;; llvm-gdb-command.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-17 16:21]

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

(require 'act-on-c-file)
(require 'llvm-shared)
(require 'anaphora)

(defun ll/get-cc1-command (clang command)
  (let ((buffer (get-buffer-create "*cc1*")))
    (while
        (progn
          (shell-command command buffer)
          (aprog1 (ll/buffer-has-include-error buffer)
            (when it
              (setq command
                    (ll/add-include-folder-to-command command))))))
    (with-current-buffer (get-buffer buffer)
      (beginning-of-buffer)
      (re-search-forward (rx "\"" (literal clang) "\" "
                             (group "-cc1" (+ nonl))))
      (match-string 1))))

(defun ll/get-clang-command-for-file (clang file)
  (string-join
   (list (funcall
          lls/get-clang-command-fun
          clang file 'compile
          :output (make-temp-file nil nil ".o"))
         "-v")
   " "))

(defun ll/kill-gdb-command ()
  (interactive)
  (let* ((buf (current-buffer))
         (fname (buffer-file-name buf))
         (clang (lls/prompt-tool "clang$"))
         (command
          (with-current-buffer buf
            (cond ((or compilation-minor-mode
                       (eq major-mode 'compilation-mode))
                   (concat (car compilation-arguments) " -v"))
                  ((string= "c" (file-name-extension fname))
                   (ll/get-clang-command-for-file clang fname))
                  (t
                   (ll/get-clang-command-for-file clang
                    (read-file-name "File? ")))))))
    (kill-new
     (format "r %s"
             (ll/get-cc1-command clang command)))))

(provide 'llvm-gdb-command)
;;; llvm-gdb-command.el ends here
