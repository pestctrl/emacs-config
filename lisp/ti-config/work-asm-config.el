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

(setq asm-font-lock-keywords
      (append
       (list '("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
               (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
             ;; label started from ".".
             (list
              (rx line-start (group "." (+ (or (syntax word) (syntax symbol)))) symbol-end ":")
              1 font-lock-function-name-face)
             ;; '("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
             ;;   2 font-lock-keyword-face)
             (list (rx line-start
                       (optional
                        (* space)
                        (+ alphanumeric))
                       (+ space)
                       (group
                        (and "."
                             (+ (or "_" alphanumeric))))
                       (or (and (+ space)
                                nonl)
                           line-end))
                   1 font-lock-keyword-face)
             (list (rx line-start
                       (optional
                        (* space)
                        (+ alphanumeric))
                       (+ space)
                       (group
                        (+ (or "_"
                               (and (optional ".")
                                    alphanumeric))))
                       (+ space)
                       nonl)
                   1 font-lock-keyword-face)
             (list (rx line-start
                       (optional
                        (* space)
                        (+ alphanumeric))
                       (+ space)
                       (group
                        (optional "||")
                        (+ space))
                       (group
                        (+ (or "_"
                               (and (optional ".")
                                    alphanumeric))))
                       (+ space)
                       nonl)
                   '(1 font-lock-comment-face)
                   '(2 font-lock-keyword-face))
             ;; directive started from ".".
             '("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
               1 font-lock-keyword-face)
             ;; %register
             (cons (rx (or "TA" "TDM" "M" "A")
                       (or (+ digit)
                           ".GT"
                           ".LT"))
                   font-lock-variable-name-face))
       cpp-font-lock-keywords))

(provide 'work-asm-config)
;;; work-asm-config.el ends here
