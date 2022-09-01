;;; my-org-autosync.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-09-01 10:09]

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
(require 'use-package)
(use-package git-auto-commit-mode)
(use-package keychain-environment)

(setq gac-automatically-add-new-files-p nil)
(setq-default gac-debounce-interval 300)

(defmacro defun-cached (name args &rest body)
  (let ((var-name (intern (concat (symbol-name name) "--cached"))))
    `(progn
       (defvar ,var-name t)

       (defun ,name ,args
         (when ,var-name
           (setq ,var-name nil)
           (run-with-timer 15 nil #'(lambda () (setq ,var-name t)))
           ,@body)))))

(defun gac-use-magit-push (buffer)
  (let ((default-directory (file-name-directory (buffer-file-name buffer))))
    (magit-push-current-to-pushremote nil)))

(advice-add #'gac-push :override #'gac-use-magit-push)

  ;; (defvar gac-auto-merge-branch-list nil)
  ;; (make-variable-buffer-local 'gac-auto-merge-branch-list)

(add-hook 'git-auto-commit-mode-hook
          #'gac-after-save-func t t)

(add-hook 'git-auto-commit-mode-hook
          #'keychain-refresh-environment)

(add-to-list 'safe-local-variable-values
             '(gac-automatically-push-p . t))

(add-to-list 'safe-local-variable-values
             '(gac-automatically-push-p . nil))

(defun gac-commit-message (filename)
  (format "%s autocommit: %s\n\n%s"
          (system-name)
          (format-time-string "%Y/%m/%d %H:%M:%S")
          filename))

(setq gac-default-message
      #'gac-commit-message)

(defun my/gac--debounced-save ()
  (let* ((actual-buffer (current-buffer)))
    (when-let (current-timer (gethash actual-buffer gac--debounce-timers))
      (remhash actual-buffer gac--debounce-timers)
      (cancel-timer current-timer))
    (puthash actual-buffer
             (run-at-time gac-debounce-interval nil
                          #'gac--after-save
                          actual-buffer)
             gac--debounce-timers)))

(advice-add #'gac--debounced-save
            :override
            #'my/gac--debounced-save)

(defun gac-debounce-again-if-magit-in-progress (buf)
  (with-current-buffer buf
    (if (ga/should-be-automatic)
        t
      (gac--debounced-save)
      nil)))

(advice-add #'gac--after-save
            :before-while
            #'gac-debounce-again-if-magit-in-progress)

(provide 'my-org-autosync)
;;; my-org-autosync.el ends here