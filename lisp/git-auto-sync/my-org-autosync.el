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
(use-package magit)
(use-package git-auto-commit-mode)
(require 'git-auto-fast-forward-mode)
(use-package keychain-environment)
(require 'ssh-key-management)
(require 'anaphora)

(setq rb/ssh-default-key (format "~/.ssh/devices/%s/id_rsa" (system-name)))
(require 'my-org-agenda-files)
(setq gaff/watch-directories (list (list (file-name-as-directory my/org-folder)
                                         "origin/desktop" "origin/gaming-laptop" "origin/puppet" "origin/mobile"
                                         "origin/laptop" "origin/nixframe")
                                   (list (file-name-as-directory (my/plaintext-file "ledger-finance"))
                                         "origin/gaming-laptop" "origin/puppet" "origin/desktop" "origin/nixframe")))

;; TODO: apparently this messes with exwm initialization somehow. Not
;; sure how. Even running 10 seconds after messes with exwm init.

;;(run-at-time nil 300 #'gaff/trigger)

(setq gac-automatically-add-new-files-p nil)
(setq-default gac-debounce-interval 120)

(defmacro defun-cached (time name args &rest body)
  (let ((var-name (intern (concat (symbol-name name) "--cached"))))
    `(progn
       (defvar ,var-name t)

       (defun ,name ,args
         (when ,var-name
           (setq ,var-name nil)
           (run-with-timer ,time nil #'(lambda () (setq ,var-name t)))
           ,@body)))))

(defun gac-use-magit-push (buffer)
  (unless (string= "NixDawn" (system-name))
    (let ((default-directory (file-name-directory (buffer-file-name buffer)))
          (magit-pre-start-git-hook (memq 'magit-maybe-save-repository-buffers
                                          magit-pre-start-git-hook)))
      (magit-push-current-to-pushremote nil))))

(advice-add #'gac-push :override #'gac-use-magit-push)

  ;; (defvar gac-auto-merge-branch-list nil)
;; (make-variable-buffer-local 'gac-auto-merge-branch-list)

(defun-cached 60 gac-run-gaff ()
  (gaff/trigger))

(defun-cached 15 gac-run-ssh-add ()
  (rb/ssh-add))

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

(defun my/gac--wait-for-idle (buffer)
  (run-with-idle-timer 3 nil #'gac--after-save buffer))

(defun my/gac--debounced-save ()
  (let* ((actual-buffer (current-buffer)))
    (when-let (current-timer (gethash actual-buffer gac--debounce-timers))
      (remhash actual-buffer gac--debounce-timers)
      (cancel-timer current-timer))
    (puthash actual-buffer
             (run-at-time gac-debounce-interval nil
                          #'my/gac--wait-for-idle
                          actual-buffer)
             gac--debounce-timers)))

(advice-add #'gac--debounced-save
            :override
            #'my/gac--debounced-save)

(defun gac-no-lint-errors (buf)
  (let ((lint-errors (with-current-buffer buf (org-lint))))
    (if (--> lint-errors
             (remove-if-not (lambda (x)
                              (string= "high"
                                       (aref (cadr x)
                                             1)))
                            it)
             (length it)
             (zerop it))
        (progn
          (when (not (zerop (length lint-errors)))
            (message "Minor lint errors were found, but we're letting it slide"))
          t)
      (message "Uh oh, lint errors were found! ")
      (with-current-buffer buf
        (call-interactively #'org-lint))
      nil)))

(defun gac-debounce-again-if-magit-in-progress (buf)
  ;; If all of the following conditions apply, we don't need to
  ;; debounce.
               ;; We should not be in an exwm buffer
  (aprog1 (and (not (eq major-mode 'exwm-mode))
               ;; The buffer should be a real file
               (buffer-file-name buf)
               ;; Check if we should be automatically pushing/pulling
               (ga/should-be-automatic (file-name-directory (buffer-file-name buf)))
               ;; there's no org-lint errors if we're in an org buffer
               (or (not (with-current-buffer buf (eq major-mode 'org-mode)))
                   (gac-no-lint-errors buf)))
    ;; One of the conditions was false, so we should debounce the save
    ;; again.
    (when (not it)
      (with-current-buffer buf
        (gac--debounced-save)))))

(advice-add #'gac--after-save
            :before-while
            #'gac-debounce-again-if-magit-in-progress)

(defun gac-activate-all-timers (&rest _)
  (interactive)
  (dolist (k (hash-table-keys gac--debounce-timers))
    (let ((timer (gethash k gac--debounce-timers)))
      (setf (timer--function timer) #'gac--after-save)
      (timer-event-handler timer))
    (remhash k gac--debounce-timers)))

(when (not noninteractive)
  (when my/puppet-p
    (add-to-list 'emacs-startup-hook
                 #'(lambda ()
                     (run-at-time 30 300 #'gaff/trigger)))

    (add-hook 'emacs-startup-hook
              #'gac-run-ssh-add)

    (add-hook 'emacs-startup-hook
              #'keychain-refresh-environment))

  (add-hook 'git-auto-commit-mode-hook
            #'gac-run-gaff)

  (add-hook 'git-auto-commit-mode-hook
            #'gac-after-save-func t t)

  (add-hook 'git-auto-commit-mode-hook
            #'gac-run-ssh-add)

  (add-hook 'git-auto-commit-mode-hook
            #'keychain-refresh-environment)

  (add-hook 'kill-emacs-hook
            'gac-activate-all-timers))

;; (add-hook 'gaff/after-merge-hook
;;           (lambda ()
;;             (org-id-update-id-locations
;;              (directory-files-recursively
;;               "~/plaintext/org/" "^[^#].*.org$"))))

(provide 'my-org-autosync)
;;; my-org-autosync.el ends here
