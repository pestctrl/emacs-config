;;; llvm-build-tool.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-17 12:48]

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
(require 'tmux-cmd)

(defvar lls/name-llvm-build-buffer
  (lambda (directory tools)
    (format "*%s-%s*"
            (file-name-nondirectory directory)
            (string-join tools ","))))

;; (defun ll/llvm-build-tool (directory tools)
;;   (interactive
;;    (list
;;     (my/completing-read "build directory" (lls/get-llvm-build-dirs))
;;     (-->
;;      (completing-read-multiple "ninja -j X "
;;                                '("clang"
;;                                  "llc")))))
;;   (let* ((buffer-name (funcall lls/name-llvm-build-buffer directory tools)))
;;     (compilation-start
;;      (lls/ninja-build-tools (lls/un-trampify directory) tools)
;;      nil
;;      (lambda (_) buffer-name))))

(defun-tmux-cmd-2 ll/llvm-build-tool (directory tools)
  (:tmux-type transient
   :interactive
   ((list
      (my/completing-read "build directory" (lls/get-llvm-build-dirs))
      (completing-read-multiple "ninja -j X "
                                '("clang"
                                  "llc")))))
  (name (funcall lls/name-llvm-build-buffer directory tools))

  (lls/ninja-build-tools (lls/un-trampify directory) tools))

(provide 'llvm-build-tool)
;;; llvm-build-tool.el ends here
