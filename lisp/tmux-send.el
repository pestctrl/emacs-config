;;; tmux-send.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-05-17 14:57]

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

(defun ts/existing-windows ()
  (let ((regex (rx line-start
                   (+ nonl)
                   ": "
                   (group
                    (+ (any "-" alphanumeric))))))
    (-->
     (shell-command-to-string "tmux list-windows -t emacs-async -F '#{window_name}'")
     (split-string it "\n")
     (reverse it)
     (cdr it))))

(defun ts/send-transient-command (name command)
  (interactive
   (let ((command (read-string "Command? ")))
     (list (progn
             (string-match
              (rx (optional (or "." "..") "/") (group (+ (not " "))) " ")
              command)
             (match-string 1 command))
           command)))
  (ts/send-sticky-command name (concat command " && echo '======== DONE ========' && sleep 5 && exit")))

(defun ts/send-sticky-command (name command)
  (interactive
   (let ((command (read-string "Command? ")))
     (list (first (split-string command " "))
           command)))
  (let ((session-window (format "%s:%s" "emacs-async" name)))
    (-->
     (list
      (if (member name (ts/existing-windows))
          (format "tmux send-keys -t %s C-c" session-window)
        (format "tmux new-window -t \"emacs-async\" ';' rename-window '%s'" name))
      (format "tmux send-keys -t '%s' '%s' C-m"
              session-window
              command))
     (string-join it " && ")
     (shell-command it))))

;; (ts/send-transient-command "run-hello" "echo hello && sleep 10")

(defun ts/send-command-with-notify (command)
  )

(require 'dired)

(defun dired-tmux (command &optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (let ((full-command (dired-shell-stuff-it command file-list t arg)))
    (ts/send-transient-command command full-command)))

(define-key dired-mode-map (kbd "G") #'dired-tmux)

(provide 'tmux-send)
;;; tmux-send.el ends here
