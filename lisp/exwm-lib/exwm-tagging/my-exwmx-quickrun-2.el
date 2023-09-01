;;; my-exwmx-quickrun-2.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-08-31 14:21]

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

;; TODO: add buffer name to appconfig

;; (add-hook 'exwm-update-class-hook #'exwmx-grocery--rename-exwm-buffer)
;; (add-hook 'exwm-update-title-hook #'exwmx-grocery--rename-exwm-buffer)

;; Manage `exwm-manage-finish-hook'
;; (add-hook 'exwm-manage-finish-hook #'exwmx-grocery--manage-finish-function)

(require 'exwmx-appconfig-predicates)

(defun my/exwmx-quickrun (command &optional search-alias ruler no-manage)
  "Run `command' to launch an application, if application's window is found,
just switch to this window, when `search-alias' is t, `command' will be regard
as an appconfig alias and search it from `exwmx-appconfig-file', by default,
:class and :instance is used to search application, user can override
it by argument `ruler', ruler can be a plist with keys: :class, :instance
and :title or just a key list."
  (cl-assert (or (not ruler) (exwmx--plist-p ruler)))
  (let* ((keys
          ;; Deal with ruler which is like (:class :instance :title)
          (or (and ruler (exwmx--clean-keylist ruler))
              '(:class :instance :pretty-name :pretty-name)))
         (name (plist-get ruler :pretty-name))
         (appconfigs (exwmx-appconfig--get-all-appconfigs))
         (matched-appconfig (exwmx-appconfig--search
                             (if search-alias
                                 (cons `(:alias ,command)
                                       (and name `((:pretty-name ,name))))
                               (cons `(:command ,command)
                                     (and name `((:pretty-name ,name)))))))
         (cmd (if (not search-alias)
                  command
                (or (plist-get matched-appconfig :command)
                    (when appconfigs
                      (let ((appconfig (exwmx-appconfig--select-appconfig)))
                        (plist-put appconfig :alias command)
                        (exwmx-appconfig--add-appconfig appconfig)
                        (plist-get appconfig :command))))))
         (buffer (exwmx-find-buffer
                  (or ruler
                      (exwmx-appconfig--get-subset matched-appconfig keys)))))
    (if (and search-alias (not cmd))
        (message "EXWM-X: please run `exwmx-appconfig' to add appconfig.")
      (message "EXWM-X Quick Run: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (let ((exwm-layout-show-all-buffers nil))
          (exwm-workspace-switch-to-buffer buffer))
      (when cmd
        (when (not no-manage)
          (exwmx-notify-x-window matched-appconfig))
        (exwmx-shell-command cmd)))))

(defmacro quickrun-lambda (cmd name)
  (let ((name-gensym (intern (format "quickrun-%s"
                                     (or name cmd)))))
    `(defun ,name-gensym ()
       (interactive)
       (my/exwmx-quickrun ,cmd nil '(:pretty-name ,name)))))

(defmacro exec (body)
  `#'(lambda ()
       (interactive)
       ,body))

;;------------------------------------------------------------------;;
(defvar exwmx/destination-windows (make-hash-table :test #'equal))

(defun exwmx-select-window ()
  (if (= 1 (length (window-list)))
      (selected-window)
    (let ((index (switch-window--prompt "Run command in window: ")))
      (cl-loop for c from 1
               for win in (switch-window--list)
               until (= c index)
               finally return win))))

(defun exwmx-notify-x-window (appconfig)
  (puthash appconfig (exwmx-select-window) exwmx/destination-windows))

(defvar exwmx/disable-my-management nil)

(defun exwmx-manage-x-window ()
  (unless (-any
           (lambda (it)
             (member it exwm-manage-finish-hook))
           exwmx/disable-my-management)
    (if-let* ((appconfigs (exwmx-appconfig-candidates))
              (matched-config
               (-any (lambda (c) (and (gethash c exwmx/destination-windows)
                                      c))
                     appconfigs))
              (name (plist-get matched-config :pretty-name))
              (window (gethash matched-config exwmx/destination-windows)))
        (unwind-protect
            (progn
              (exwm-workspace-rename-buffer name)
              (when (and (window-live-p window)
                         (not (eq window (selected-window))))
                (let ((curr (current-buffer)))
                  (save-selected-window
                    (previous-buffer)
                    (window--display-buffer curr window 'reuse nil)))))
          (setq-local exwmx-pretty-name name)
          (remhash matched-config exwmx/destination-windows))
      (exwm-workspace-rename-buffer
       (or exwm-class-name exwm-instance-name exwm-title)))))

(add-hook 'exwm-manage-finish-hook 'exwmx-manage-x-window)

;;------------------------------------------------------------------;;
;; (let ((debug/appconfig (exwmx-appconfig--search `((:command "pcmanfm"))))
;;       (debug/buffer (get-buffer "*EXWM*")))
;;   (cl-assert (exwmx-buffer-match-p debug/appconfig debug/buffer))
;;   (cl-assert (exwmx-buffer-match-p '(:class "Pcmanfm") debug/buffer))
;;   (cl-assert (exwmx-buffer-match-p '(:instance "pcmanfm") debug/buffer))
;;   (cl-assert (exwmx-find-buffer debug/appconfig))
;;   (cl-assert (eq debug/buffer (exwmx-find-buffer debug/appconfig))))

(provide 'my-exwmx-quickrun-2)
;;; my-exwmx-quickrun-2.el ends here
