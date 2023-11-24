;;; work-asm-config.el ---

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-10-17 15:58]

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
(require 'use-package)
(use-package asm-mode)

(modify-syntax-entry ?- "-" asm-mode-syntax-table)
(modify-syntax-entry ?+ "-" asm-mode-syntax-table)
(modify-syntax-entry ?. "_" asm-mode-syntax-table)
(modify-syntax-entry ?< "-" asm-mode-syntax-table)

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8)))

(setq asm-font-lock-keywords
      `(;; Labels
        (,(rx line-start (group (>= 2 (any "0-9A-f"))) " " (group (+ (or (syntax word) (syntax symbol)))) symbol-end ":")
         (1 font-lock-warning-face) (2 font-lock-function-name-face))
        ;; Hex Encoding for instructions
        (,(rx line-start (group (>= 2 (any "0-9A-f"))) ":" (* space) (group (+ " " (and  (= 4 (any "0-9A-f"))))))
         (1 font-lock-warning-face) (2 font-lock-constant-face))
        ;; Instructions
        (,(rx line-start (* space) (optional (+ (any "0-9A-Fa-f")) ":") (* space) (optional "||" (+ space)) (group (+ (or (syntax word) (syntax symbol) "."))))
         (1 font-lock-keyword-face))
        ;; Dot directives
        (,(rx line-start (group (+ (or "." (syntax word) (syntax symbol)))) ":") (1 font-lock-function-name-face))
        (,(rx symbol-start (or "DIRM" (and "ADDR" digit)) symbol-end) . font-lock-type-face)
        (,(rx word-start
              (or (and (or "TDM" "TA" "UNC")
                       (optional digit)
                       (optional (or ".MAP" ".NZ" ".Z")))
                  (and (or "XM" "XD")
                       (+ digit))
                  (and (or "M" "A" "D")
                       (+ digit)))
              word-end)
         . font-lock-variable-name-face)
        (,(rx word-start
              (and (or "M" "A" "D")
                   (or ".GT" ".LT" ".GEQ" ".LEQ" ".EQ" ".NEQ" ".HI" ".LO"))
              word-end)
         . font-lock-keyword-face)
        (,(rx "@" (optional "(") (group (optional "(") (+ (or (syntax word) (syntax symbol)))) (or ")" symbol-end)) (1 font-lock-function-name-face))
        (,(rx "#" (optional "-") "0x" (+ alphanumeric)) . font-lock-constant-face)
        ;; (,(rx (or (* space)) "\n" (* space) "||" (+ space)) . 'asm-vliw-bar)
        (,(rx line-start (group (* space)) (syntax word)) . (1 'asm-vliw-bar))
        ))

(defun my/asm-back-to-label (arg)
  (interactive "P")
  (if arg
      (re-search-backward (rx "<" (not "$") (* nonl) ">:"))
    (re-search-backward (rx "<" (+ nonl) ">:")))
  (beginning-of-line))

(define-key asm-mode-map (kbd "C-M-a") #'my/asm-back-to-label)

(defface asm-vliw-bar
  `((t ,(list
         :background "gray25"
         :extend t
         :inherit font-lock-comment-face
         :box '(:line-width 1 :color "gray30" :style raised))))
  nil)

(defun asm-clean-up ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp (rx (group (or alphanumeric ")")) "," (group (or alphanumeric "#" "*")))
                    "\\1,  \\2")))

(defun asm-narrow-to-function ()
  (interactive)
  (let ((r (rx line-start (= 8 (any "0-9a-f")) " <" (not "$")))
        start end)
    (save-excursion
      (next-line)
      (re-search-backward r)
      (beginning-of-line)
      (setq start (point))

      (next-line)
      (re-search-forward r)
      (previous-line)
      (beginning-of-line)
      (setq end (point)))
    (narrow-to-region start end)))

(add-hook 'asm-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-multiline) t)
            ;; (add-hook 'font-lock-extend-region-functions
            ;;           'test-font-lock-extend-region)
            ))

;; (defun test-font-lock-extend-region ()
;;   "Extend the search region to include an entire block of text."
;;   ;; Avoid compiler warnings about these global variables from font-lock.el.
;;   ;; See the documentation for variable `font-lock-extend-region-functions'.
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (save-excursion
;;     (let ((beg font-lock-beg)
;;           (end font-lock-end))
;;       (goto-char font-lock-beg)
;;       (setq font-lock-beg (point-at-bol))
;;       (forward-line)
;;       (beginning-of-line)
;;       (when (looking-at (rx (+ space) "||" (+ space)))
;;         (setq font-lock-end (match-end 0)))
;;       ;; (or (not (= beg font-lock-beg))
;;       ;;     (not (= end font-lock-end)))
;;       nil))
;;   nil)

(provide 'work-asm-config)
;;; work-asm-config.el ends here
