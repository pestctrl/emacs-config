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

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 10)))

;; (setq asm-font-lock-keywords
;;       (append
;;        (list '("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
;;                (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
;;              label started from ".".
;;              `(,(rx line-start (optional (= 8 (any "0-9A-f"))) (group "." (+ (or (syntax word) (syntax symbol)))) symbol-end ":")
;;                (1 font-lock-function-name-face))
;;              `(,(rx line-start (* space) "||" (+ space)) . 'asm-vliw-bar)
;;              `(,(rx line-start
;;                     (optional (* space) (+ alphanumeric)) (+ space)
;;                     (optional "||" (+ space))
;;                     (group
;;                      (group
;;                       (+ (or (syntax symbol)
;;                              (and (optional ".")
;;                                   (syntax word)))))
;;                      (group
;;                       (* (and "." (+ (syntax word))))))
;;                     (+ space)
;;                     nonl)
;;                (1 font-lock-keyword-face))
;;              `(,(rx line-start
;;                     (optional (group "(" (+ (syntax word)) ")"))
;;                     (+ (syntax whitespace))
;;                     (group (group (+ (or (and (optional ".") (syntax word))
;;                                          (syntax symbol))))
;;                            (* (group "." (+ (syntax word))))))
;;                (2 font-lock-keyword-face))
;;              directive started from ".".
;;              '("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
;;                1 font-lock-keyword-face)
;;              %register
;;              `(,(rx "ADDR1") . font-lock-type-face)
;;              `(,(rx (or "TA" "TDM" "M" "A" "D")
;;                     (or (+ digit) ".GT" ".LT" ".EQ"))
;;                . font-lock-variable-name-face)
;;              `(,(rx "@" (optional "($") (group (+ (or (syntax word) (syntax symbol)))) (or ")" symbol-end)) (1 font-lock-function-name-face))
;;              `(,(rx "#" (optional "-") "0x" (+ alphanumeric)) . font-lock-constant-face))
;;        cpp-font-lock-keywords))

(setq asm-font-lock-keywords
      `(;; Labels
        (,(rx line-start (group (>= 2 (any "0-9A-f"))) " " (group (+ (or (syntax word) (syntax symbol)))) symbol-end ":")
         (1 font-lock-warning-face) (2 font-lock-function-name-face))
        ;; Hex Encoding for instructions
        (,(rx line-start (group (>= 2 (any "0-9A-f"))) ":" (* space) (group (+ " " (and  (= 4 (any "0-9A-f"))))))
         (1 font-lock-warning-face) (2 font-lock-constant-face))
        ;; Instructions
        (,(rx line-start (* space) (* (+ (any "0-9A-f")) (+ (or ":" space))) (+ space) (optional "||" (+ space)) (group (+ (or (syntax word) (syntax symbol) "."))))
         (1 font-lock-keyword-face))
        ;; Dot directives
        (,(rx line-start (group (+ (or "." (syntax word) (syntax symbol)))) ":") (1 font-lock-function-name-face))
        (,(rx "ADDR" digit) . font-lock-type-face)
        (,(rx word-start
              (or (and (or "TDM" "TA" "UNC")
                       (optional digit)
                       (optional (or ".MAP" ".NZ" ".Z")))
                  (and (or "XM" "XD")
                       (+ digit))
                  (and (or "M" "A" "D")
                       (or (+ digit)
                           ".GT" ".LT" ".GEQ" ".LEQ" ".EQ" ".NEQ" ".HI" ".LO")))
              word-end)
         . font-lock-variable-name-face)
        (,(rx "@" (optional "(") (group (optional "(") (+ (or (syntax word) (syntax symbol)))) (or ")" symbol-end)) (1 font-lock-function-name-face))
        (,(rx "#" (optional "-") "0x" (+ alphanumeric)) . font-lock-constant-face)
        (,(rx (or (* space)) "\n" (* space) "||" (+ space)) . 'asm-vliw-bar)
        ))

(defface asm-vliw-bar `((t (:background "gray25" :extend t :inherit font-lock-comment-face))) nil)

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
