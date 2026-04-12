;;; tramp-hack.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-07-20 10:01]

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

(advice-add tramp-ssh-controlmaster-options
            :override
            #'my/tramp-ssh-controlmaster-options)

(defun my/tramp-ssh-controlmaster-options (vec)
  "Return the Control* arguments of the local ssh."
  (cond
   ;; No options to be computed.
   ((or (null tramp-use-connection-share)
	    (null (assoc "%c" (tramp-get-method-parameter vec 'tramp-login-args))))
    "")

   ;; Use plink option.
   ((string-match-p
     (rx "plink" (? ".exe") eol)
     (tramp-get-method-parameter vec 'tramp-login-program))
    (if (eq tramp-use-connection-share 'suppress)
	    "-noshare" "-share"))

   ;; There is already a value to be used.
   ((and (eq tramp-use-connection-share t)
         (stringp tramp-ssh-controlmaster-options))
    tramp-ssh-controlmaster-options)

   ;; We can't auto-compute the options
   ((ignore-errors
      (not (tramp-ssh-option-exists-p vec "ControlMaster=auto")))
    "")

   ;; Determine the options.
   (t (ignore-errors
        ;; ControlMaster and ControlPath options are introduced in OpenSSH 3.9.
	    (concat
         "-o ControlMaster="
         (if (eq tramp-use-connection-share 'suppress)
             "no" "auto")

         " -o ControlPath="
         (if (eq tramp-use-connection-share 'suppress)
             "none"
           ;; Hashed tokens are introduced in OpenSSH 6.7.
           (if (tramp-ssh-option-exists-p vec "ControlPath=tramp.%C")
               "tramp.%%C" "tramp.%%r@%%h:%%p"))

         ;; ControlPersist option is introduced in OpenSSH 5.6.
         (when (and (not (eq tramp-use-connection-share 'suppress))
                    (tramp-ssh-option-exists-p vec "ControlPersist=no"))
           " -o ControlPersist=no"))))))
(provide 'tramp-hack)
;;; tramp-hack.el ends here
