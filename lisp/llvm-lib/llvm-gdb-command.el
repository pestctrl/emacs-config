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

(require 'llvm-shared)

(defun ll/get-cc1-command (file)
  (let* ((fname (file-name-nondirectory file))
         (buffer (format "*cc1-%s*" fname))
         (clang (lls/prompt-tool "clang$")))
    (save-window-excursion
      (let ((command (string-join
                      (list (funcall
                             lls/get-clang-command-fun
                             clang file 'compile
                             (make-temp-file nil nil ".o"))
                            "-v")
                      " ")))
        (shell-command command buffer))
      (with-current-buffer (get-buffer buffer)
        (beginning-of-buffer)
        (re-search-forward (rx "\"" (literal clang) "\" "
                               (group "-cc1" (+ nonl))))
        (match-string 1)))))

(defun ll/kill-gdb-command (file)
  (interactive
   (list
    (--> (current-buffer)
         (buffer-file-name)
         (read-file-name "Which file? " nil))))
  (let ((fname (file-name-nondirectory file)))
    (kill-new
     (format "r %s"
             (ll/get-cc1-command file)))))

(provide 'llvm-gdb-command)
;;; llvm-gdb-command.el ends here
