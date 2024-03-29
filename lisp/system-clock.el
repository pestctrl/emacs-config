;;; system-clock.el --- This is my system interrupt -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-17 13:53]

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

(defvar current-system-clock nil)

(defvar system-clock-time 30)

(define-minor-mode system-clock-mode
  nil nil nil
  :global t
  (cond (system-clock-mode
         (setq current-system-clock (run-at-time nil system-clock-time (lambda () (interactive) (let ((visible-bell t)) (ding))))))
        (t
         (cancel-timer current-system-clock))))

(defun activate-system-clock (seconds)
  (interactive "p")
  (when system-clock-mode
    (system-clock-mode nil))
  (setq system-clock-time seconds)
  (system-clock-mode))

(define-key *root-map* (kbd "S") #'activate-system-clock)

(provide 'system-clock)
;;; system-clock.el ends here
