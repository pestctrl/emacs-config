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
          "defvar"
          (seq "defset"
               (+ (or space "\n"))
               (+ (or alphanumeric ">" "<" "_"))))
      (+ (or space "\n"))
      (group (+ (or alphanumeric "_")))
      (* (+ (or space "\n")))
      (or "{" "<" ":" "="))) ;; defset is special

(defvar ll/def-class-hash (make-hash-table :test #'equal))

(defun ll/get-defs-and-classes (target &optional invalidate)
  (when invalidate
    (remhash target ll/def-class-hash))

  (or (gethash target ll/def-class-hash)
      (let* ((l (make-hash-table))
             (td-regexp (rx (+ anything) ".td" line-end))
             (target-dir
              (-->
               (lls/get-llvm-root-dir)
               (expand-file-name "llvm/lib/Target/" it)
               (expand-file-name target it)
               (directory-files it t td-regexp)))
             (gen-dir
              (-->
               (lls/get-llvm-root-dir)
               (expand-file-name "llvm/include/llvm/Target" it)
               (directory-files it t td-regexp))))

        (dolist (file (append gen-dir target-dir))
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward ll/def-keywords nil t)
                (goto-char (match-beginning 1))
                (puthash (intern (match-string 1)) (point-marker) l)))))
        (puthash target l ll/def-class-hash)
        l)))

(defun ll/read-tablegen-target ()
  (if (string-match (rx "llvm/lib/Target/" (group (+ alphanumeric)) "/") default-directory)
      (match-string 1 default-directory)
    (completing-read "Target? "
                     (ll/get-codegen-targets))))

(defun ll/read-tablegen-symbol (target arg)
  (let ((map (ll/get-defs-and-classes target arg))
        (cur (symbol-at-point)))
    (or (gethash cur map)
        (-->
         (completing-read "Symbol? " (hash-table-keys map) nil t)
         (intern it)
         (gethash it map)))))

(defun ll/jump-to-tablegen (arg)
  (interactive "P")
  (let* ((target (ll/read-tablegen-target))
         (marker (ll/read-tablegen-symbol target arg)))
    (xref--push-markers)
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))
    (recenter)))

(provide 'llvm-jump-to-tablegen)
;;; llvm-jump-to-tablegen.el ends here
