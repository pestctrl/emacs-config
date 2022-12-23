;;; llvm-build-command.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-23 12:04]

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

(defun ll/get-llvm-source-build-command (file)
  (interactive
   (list
    (buffer-file-name (current-buffer))))

  (when (not (string-match-p "llvm-project" file))
    (user-error "This is not an LLVM file!"))

  (let ((build-dir (lls/get-llvm-build-dir)))
    (compilation-start (-->
                        (list
                         (format "touch %s" file)
                         (lls/ninja-build-tools
                          build-dir (list (format "%s^" file)) t))
                        (string-join it " && "))
                       nil
                       `(lambda (_)
                          ,(format "*build-%s*" (file-name-nondirectory file))))))

(provide 'llvm-build-command)
;;; llvm-build-command.el ends here
