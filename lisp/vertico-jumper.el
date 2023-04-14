;;; vertico-jumper.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-31 09:21]

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
(use-package vertico)
(require 'vertico-flat)

(define-prefix-command '*vertico-jump-map*)

(define-key vertico-flat-map (kbd "M-j") '*vertico-jump-map*)

(define-key *vertico-jump-map* (kbd "h") #'vertico--jump-to-home)
(define-key *vertico-jump-map* (kbd "H") #'vertico--force-jump-to-home)

(defun vertico--jump-to-home ()
  (interactive)
  (insert "~/")
  (vertico--exhibit))

(defun vertico--force-jump-to-home ()
  (interactive)
  (call-interactively #'move-beginning-of-line)
  (kill-line)
  (insert "~/"))

(provide 'vertico-jumper)
;;; vertico-jumper.el ends here
