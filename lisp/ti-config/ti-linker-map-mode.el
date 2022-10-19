;;; ti-linker-map-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-10-19 14:41]

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

(defconst ti-map-lock-keywords
  `((,(rx (optional "0x") (= 8 (any "0-9A-Fa-f")) symbol-end) . font-lock-constant-face)
    ;; (,(rx (+ (any "A-z0-9_.")) ".o" (optional "bj") symbol-end) . font-lock-variable-name-face)
    (,(rx symbol-start (not "_") (+ (any "A-Z_")) symbol-end (not ".")) . font-lock-keyword-face)
    (,(rx "--HOLE--") . font-lock-keyword-face)
    (,(rx line-start (or "reset_vector"
                         (and  "." (+ (or "_" (any "a-z")))))
          symbol-end)
     . font-lock-variable-name-face)
    (,(rx "(" (+ (or "TI" (any "A-z0-9_.:"))) ")") . font-lock-variable-name-face)
    (,(rx (+ (any "A-z0-9_.")) (or ".lib" ".a")) . font-lock-function-name-face)))

(define-derived-mode ti-linker-map-mode prog-mode "TI Linker Map Mode"
  nil nil
  (setq-local font-lock-defaults '(ti-map-lock-keywords)))

(provide 'ti-linker-map-mode)
;;; ti-linker-map-mode.el ends here
