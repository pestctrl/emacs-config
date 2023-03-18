;;; mbsync.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-18 12:00]

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

(defvar mbsync-timer nil)
(defvar mbsync-process nil)

(defvar mbsync-presync-hooks '())
(defvar mbsync-postsync-hooks '())

(defun mbsync-process-sentinel (process event)
  (when (string-match-p "exited abnormally with code 1" event)
    (with-current-buffer (process-buffer mbsync-process)
      (when (string-match-p "get_password_emacs"(buffer-string))
        (erase-buffer)
        (message "Oops, didn't grab a password. ")
        (setq mbsync-timer (run-with-timer 300 nil #'run-mbsync)))))
  (when (string-match-p "^finished" event)
    (message "mbsync finished")
    (run-hooks 'mbsync-postsync-hooks)
    (setq mbsync-timer (run-with-timer 300 nil #'run-mbsync))))

(defun run-mbsync ()
  (interactive)
  (if (and (processp mbsync-process)
           (process-live-p mbsync-process))
      (message "mbsync already running...")
    (run-hooks 'mbsync-presync-hooks)
    (message "mbsync starting...")
    (when (and (timerp mbsync-timer)
               (not (timer--triggered mbsync-timer)))
      (cancel-timer mbsync-timer))
    (call-process-shell-command "timedatectl" nil "*mbsync-output*")
    (set-process-sentinel
     (setq mbsync-process
           (start-process-shell-command "mbsync" "*mbsync-output*" "mbsync fm"))
     #'mbsync-process-sentinel)))

(provide 'mbsync)
;;; mbsync.el ends here
