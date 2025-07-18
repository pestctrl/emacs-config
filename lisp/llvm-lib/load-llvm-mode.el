;;; load-llvm-mode.el ---  -*- lexical-binding: t -*-

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
  (interactive (list (lls/conf-get 'root-dir)))
  (when (not (require 'my-tablegen-mode nil t))
    (load-file (expand-file-name "llvm/utils/emacs/tablegen-mode.el" root-dir)))
  (when (not (require 'my-llvm-mode nil t))
    (load-file (expand-file-name "llvm/utils/emacs/llvm-mode.el" root-dir)))
  (load-file (expand-file-name "llvm/utils/emacs/emacs.el" root-dir))

  (defface llvm-separator-face `((t (:background "gray25" :extend t :inherit font-lock-warning-face)))
    nil)

  (progn
    (-->
     llvm-font-lock-keywords
     (remove '("\\_<\\(a\\(?:fn\\|rcp\\)\\|contract\\|fast\\|n\\(?:inf\\|nan\\|sz\\)\\|reassoc\\)\\_>" . font-lock-keyword-face) llvm-font-lock-keywords)
     (setq llvm-font-lock-keywords it))

    (add-to-list 'llvm-font-lock-keywords
                 `(,(regexp-opt '("nnan" "ninf" "nsz" "arcp" "contract" "afn" "reassoc" "fast") 'symbols) . 'shadow)))

  (add-to-list 'llvm-font-lock-keywords
               `(,(rx line-start (optional (any "#;") " ") "***" (+ nonl) "***" (optional ":") "\n") . 'llvm-separator-face))

  (-->
   "\\b[-]?[0-9]+\\b"
   (assoc it llvm-font-lock-keywords)
   (cl-position it llvm-font-lock-keywords)
   (nth it llvm-font-lock-keywords)
   (setf it
         `(,(rx word-boundary (optional "-") ))))

  (pop c-mode-common-hook)
  (add-hook 'c-mode-common-hook
	        (function
	         (lambda nil
	           (if (and buffer-file-name (string-match "llvm" buffer-file-name))
		           (progn
		             (c-set-style "llvm.org")))))))

(provide 'load-llvm-mode)
;;; load-llvm-mode.el ends here
