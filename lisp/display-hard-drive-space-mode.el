;;; display-hard-drive-space-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 08:37]

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
(defvar update-harddrive-timer nil)
(defvar hard-drive-space "")

(defun get-hard-drive-space ()
  (let ((default-directory "/"))
    (shell-command-to-string "df -h -P -l / | tail -n 1 | tr -s ' ' | cut -d ' ' -f 4 | tr -d '\n'")))

(defun update-hard-drive-space-string ()
  (->> (get-hard-drive-space)
       (format " [%s]")
       (setq hard-drive-space)))

(define-minor-mode display-hard-drive-space-mode
  nil nil nil
  :global t
  (cond (display-hard-drive-space-mode
         (add-to-list 'global-mode-string 'hard-drive-space t)
         (setq update-harddrive-timer
               (run-at-time nil 15 'update-hard-drive-space-string)))
        (t
         (cancel-timer update-harddrive-timer)
         (setq global-mode-string
               (delq 'hard-drive-space global-mode-string)))))

(provide 'display-hard-drive-space-mode)
;;; display-hard-drive-space-mode.el ends here
