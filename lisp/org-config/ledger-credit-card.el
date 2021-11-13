;;; ledger-credit-card.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2021-11-07 17:37]

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
(require 'ledger-mode)

(defvar ledger-cc-account nil)

(make-variable-buffer-local 'ledger-cc-account)

(defvar ledger-cc-mode-map nil)

(unless ledger-cc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'ledger-cc-quick-add)
    (setq ledger-cc-mode-map map)))

(defun ledger-cc-quick-add ()
  (interactive)
  (let ((date (ledger-read-date "Date: "))
        (transaction (read-string "Description? "))
        (amount (read-string "Amount? ")))
    (end-of-buffer)
    (insert (format "\n%s %s\n    Exp:Resolve  $%s\n    %s\n\n"
                    date transaction amount ledger-cc-account))))

(define-minor-mode ledger-cc-quick-add-mode ""
  nil nil ledger-cc-mode-map
  (when ledger-cc-quick-add-mode
    (setq ledger-cc-account
          (ledger-read-account-with-prompt "Which account? "))))

(provide 'ledger-credit-card)
;;; ledger-credit-card.el ends here
