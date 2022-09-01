;;; ssh-key-management.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-09-01 08:09]

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

(defvar rb/ssh-default-key (format "~/.ssh/devices/%s/id_rsa" (system-name))
  "My default SSH key.")

(defun rb/ssh-add (&optional arg)
  "Add the default ssh-key if it's not present.

With a universal argument, prompt to specify which key."
  (interactive "P")
  (when (or arg
            (not (rb/ssh-agent-has-keys-p)))
    (rb/ssh-add-in-emacs
     (if (not arg)
         rb/ssh-default-key
       (read-file-name
        "Add key: \n" "~/.ssh" nil 't nil
        (lambda (x)
          (not (or (string-suffix-p ".pub" x)
                   (string= "known_hosts" x)))))))))

(defun rb/ssh-agent-has-keys-p ()
  "Return t if the ssh-agent has a key."
  (when (not
         (string-match-p
          "No identities"
          (shell-command-to-string "ssh-add -l")))
    t))

(defun rb/ssh-add-in-emacs (key-file)
  "Run ssh-add to add a key to the running SSH agent."
  (let ((process-connection-type t)
        process)
    (unwind-protect
        (progn
          (setq process
                (start-process
                 "ssh-add" nil "ssh-add"
                 (expand-file-name key-file)))
          (set-process-filter
           process 'rb/ssh-add-process-filter)
          (while (accept-process-output process)))
      (if (eq (process-status process) 'run)
          (kill-process process)))))

(defun rb/ssh-add-process-filter (process string)
  (save-match-data
    (if (string-match ":\\s *\\'" string)
        (process-send-string process
                             (concat
                              (read-passwd string)
                              "\n"))
      (message "ssh-add: %s" string))))

(provide 'ssh-key-management)
;;; ssh-key-management.el ends here
