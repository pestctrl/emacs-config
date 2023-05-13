;;; my-llvm-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-05-13 08:14]

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

(defun load-llvm-mode (root-dir)
  (load-file (expand-file-name "llvm/utils/emacs/llvm-mode.el" root-dir))
  (load-file (expand-file-name "llvm/utils/emacs/emacs.el" root-dir))

  (defface llvm-separator-face `((t (:background "gray25" :extend t :inherit font-lock-warning-face)))
    nil)

  (progn
    (-->
     llvm-font-lock-keywords
     (remove '("%[-a-zA-Z$._][-a-zA-Z$._0-9]*" . font-lock-variable-name-face) llvm-font-lock-keywords)
     (setq llvm-font-lock-keywords it))

    (add-to-list 'llvm-font-lock-keywords
                 `(,(rx "%" (+ (or "." "_" alphanumeric)) (optional (+ ":" (+ (or "." "_" alphanumeric)))))
                   . font-lock-variable-name-face)))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx line-start (optional "# ") "***" (+ nonl) "***" (optional ":") "\n") . 'llvm-separator-face))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx " = " (optional "nsw ") (group (+ (or "_" alphanumeric)))) (1 font-lock-keyword-face)))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx "$" (+ alphanumeric)) . font-lock-variable-name-face))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx (or "renamable" "implicit-def" "implicit" "debug-location" "nsw" "align")) . 'shadow))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx "!" (+ alphanumeric)) . 'font-lock-variable-name-face))

  (-->
   "\\b[-]?[0-9]+\\b"
   (assoc it llvm-font-lock-keywords)
   (cl-position it llvm-font-lock-keywords)
   (nth it llvm-font-lock-keywords)
   (setf it
         `(,(rx word-boundary (optional "-") )))))

(provide 'my-llvm-mode)
;;; my-llvm-mode.el ends here
