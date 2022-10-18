;;; work-asm-config.el ---  -*- lexical-binding: t -*-

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

(setq asm-font-lock-keywords
      (append
       (list '("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
               (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
             ;; label started from ".".
             `(,(rx line-start (group "." (+ (or (syntax word) (syntax symbol)))) symbol-end ":")
               (1 font-lock-function-name-face))
             `(,(rx line-start (* space) "||" (+ space)) . 'asm-vliw-bar)
             `(,(rx line-start
                    (optional (* space) (+ alphanumeric)) (+ space)
                    (optional "||" (+ space))
                    (group
                     (group
                      (+ (or (syntax symbol)
                             (and (optional ".")
                                  (syntax word)))))
                     (group
                      (* (and "." (+ (syntax word))))))
                    (+ space)
                    nonl)
               (1 font-lock-keyword-face))
             `(,(rx line-start
                    (optional (group "(" (+ (syntax word)) ")"))
                    (+ (syntax whitespace))
                    (group (group (+ (or (and (optional ".") (syntax word))
                                         (syntax symbol))))
                           (* (group "." (+ (syntax word))))))
               (2 font-lock-keyword-face))
             ;; directive started from ".".
             '("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
               1 font-lock-keyword-face)
             ;; %register
             `(,(rx "ADDR1") . font-lock-type-face)
             `(,(rx (or "TA" "TDM" "M" "A" "D")
                    (or (+ digit) ".GT" ".LT" ".EQ"))
               . font-lock-variable-name-face)
             `(,(rx "@" (optional "($") (group (+ (or (syntax word) (syntax symbol)))) (or ")" symbol-end)) (1 font-lock-function-name-face))
             `(,(rx "#" (optional "-") "0x" (+ alphanumeric)) . font-lock-constant-face))
       cpp-font-lock-keywords))

(defface asm-vliw-bar `((t (:background "gray25" :extend t :inherit font-lock-comment-face))) nil)

(defun asm-clean-up ()
  (interactive)
  (save-excursion
    (replace-regexp (rx (group (or alphanumeric ")")) "," (group (or alphanumeric "#" "*")))
                    "\\1,  \\2")))

(provide 'work-asm-config)
;;; work-asm-config.el ends here
