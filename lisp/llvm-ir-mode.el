;;; llvm-ir-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-09-29 16:20]

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

(setq kwds
      `(("\\<IR Dump\\>" . font-lock-function-name-face)
        ("\\<Before\\>" . font-lock-doc-face)
        ("\\<After\\>" . font-lock-doc-face)
        ("\\<Machine code for function\\>" . font-lock-function-name-face)
        ("\\<End machine code for function\\>" . font-lock-doc-face)))

;; (face-remap-add-relative 'stripe-highlight '(:foreground "black" :background "yellow"))

(define-minor-mode llvm-ir-minor-mode
  "Doc string."
  nil "blah" nil
  (font-lock-add-keywords nil kwds)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))

  (font-lock-mode 1))

(provide 'llvm-ir-mode)
;;; llvm-ir-mode.el ends here
