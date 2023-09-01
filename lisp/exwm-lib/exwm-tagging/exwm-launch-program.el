;;; exwm-launch-program.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-24 19:35]

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
(use-package dmenu)

(defun dmenu-refresh-applist ()
  (interactive)
  (make-thread
   #'dmenu--cache-executable-files))

(dmenu-refresh-applist)

(add-to-list 'vertico-multiform-commands
             '(exwmx-launch-program flat (vertico-cycle . t)))

(defun read-program ()
  (completing-read
   "$ "
   (append dmenu--history-list
           (cl-remove-if (lambda (x)
                           (member x dmenu--history-list))
                         dmenu--cache-executable-files))))

(defun exwmx-launch-program (command &optional process-name)
  (interactive (list (read-program)))
  (setq dmenu--history-list (cons command (remove command dmenu--history-list)))
  (when (> (length dmenu--history-list)
           dmenu-history-size)
    (setcdr (nthcdr (- dmenu-history-size 1)
                    dmenu--history-list)
            nil))
  (my/exwmx-quickrun command))

(provide 'exwm-launch-program)
;;; exwm-launch-program.el ends here
