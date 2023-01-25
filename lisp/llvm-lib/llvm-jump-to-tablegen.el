;;; llvm-jump-to-tablegen.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-01-25 15:20]

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
(require 'llvm-show-instr-info)

(defconst ll/def-keywords
  (rx (or "class" "multiclass"
          "def" "defm"
          "defvar")
      (+ (or space "\n"))
      (group (+ (or alphanumeric "_")))
      (+ (or space "\n"))
      (or "{" "<" ":"))) ;; defset is special

(defvar ll/def-class-hash (make-hash-table :test #'equal))

(defun ll/get-defs-and-classes (target &optional invalidate)
  (when invalidate
    (remhash target ll/def-class-hash))

  (or (gethash target ll/def-class-hash)
      (let ((l (make-hash-table :test #'equal)))
        (-->
         (lls/get-llvm-root-dir)
         (expand-file-name "llvm/lib/Target/" it)
         (expand-file-name target it)
         (dolist (file (directory-files it t ".*\\.td$"))
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward ll/def-keywords nil t)
                 (let ((str (match-string 1)))
                   (set-text-properties 0 (length str) nil str)
                   (puthash str (point-marker) l)))))))
        (puthash target l ll/def-class-hash)
        l)))

(defun ll/jump-to-tablegen (arg)
  (interactive "P")
  (let* ((target (completing-read "Target? "
                                  (ll/get-codegen-targets)))
         (p (ll/get-defs-and-classes target arg))
         (symbol (completing-read "Symbol? " (hash-table-keys p)))
         (marker (gethash symbol p)))
    (display-buffer-same-window (marker-buffer marker) nil)
    (goto-char (marker-position marker))
    (recenter)))

(provide 'llvm-jump-to-tablegen)
;;; llvm-jump-to-tablegen.el ends here
