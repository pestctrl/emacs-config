;;; my-exwmx-quickrun.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 08:19]

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
(require 'exwmx-core)

(defun exwmx-quickrun (command &optional search-alias ruler)
  (exwmx--switch-window)
  (let* ((ruler-plist-p (and ruler (exwmx--plist-p ruler)))
         (keys
          ;; Deal with ruler which is like (:class :instance :title)
          (if (and ruler (listp ruler) (not ruler-plist-p))
              (exwmx--clean-keylist ruler)
            '(:class :instance)))
         (appconfigs (exwmx-appconfig--get-all-appconfigs))
         (cmd (if search-alias
                  (or (plist-get (exwmx-appconfig--search
                                  `((:alias ,command)))
                                 :command)
                      (when appconfigs
                        (let ((appconfig (exwmx-appconfig--select-appconfig)))
                          (plist-put appconfig :alias command)
                          (exwmx-appconfig--add-appconfig appconfig)
                          (plist-get appconfig :command))))
                command))
         (buffer (or (if search-alias
                         (exwmx-quickrun--find-buffer
                          (if ruler-plist-p
                              ruler
                            (exwmx-appconfig--get-subset
                             (exwmx-appconfig--search
                              `((:alias ,command)))
                             keys)))
                       (exwmx-quickrun--find-buffer
                        (if ruler-plist-p
                            ruler
                          (exwmx-appconfig--get-subset
                           (exwmx-appconfig--search
                            `((:command ,command)))
                           keys)))))))
    (if (and search-alias (not cmd))
        (message "EXWM-X: please run `exwmx-appconfig' to add appconfig.")
      (message "EXWM-X Quick Run: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (when cmd
        (launch-program-with-name cmd (plist-get ruler :instance))))))

(defun exwmx-quickrun--find-buffer (ruler)
  "Find a exwm buffer which match `ruler', ruler is
          a plist with three keys: :class, :instance and :title."
  (let ((current (current-buffer))
        (buffers (buffer-list))
        (result '()))
    (while buffers
      (let ((buffer (pop buffers))
            (class (plist-get ruler :class))
            (instance (plist-get ruler :instance))
            (title (plist-get ruler :title)))
        (with-current-buffer buffer
          (when (and (or class instance title)
                     (exwmx--string-match-p (or class ".*") exwm-class-name)
                     (exwmx--string-match-p (or (concat "^" instance "$") ".*") exwm-instance-name)
                     (exwmx--string-match-p (or title ".*") exwm-title))
            (push buffer result)))))
    (setq result (reverse result))
    ;; If two more buffers are found, switch between these buffer.
    (if (and (cadr result)
             (eq (car result) current))
        (cadr result)
      (car result))))

(defmacro quickrun-lambda (cmd instance)
  (if (null instance)
      `(lambda ()
         (interactive)
         (exwmx-quickrun ,cmd))
    `(lambda ()
       (interactive)
       (exwmx-quickrun ,cmd nil '(:class ".*" :instance ,instance)))))

(defun exwmx-launch-program (command &optional process-name)
  (interactive (list (read-program)))
  (setq dmenu--history-list (cons command (remove command dmenu--history-list)))
  (when (> (length dmenu--history-list)
           dmenu-history-size)
    (setcdr (nthcdr (- dmenu-history-size 1)
                    dmenu--history-list)
            nil))
  (exwmx-quickrun command))

(add-hook 'exwm-manage-finish-hook 'exwm-rename-buffer)

(defun launch-program-with-name (cmd name)
  (interactive)
  (let ((name (or name cmd)))
    (setq my/window-name name)
    (start-process-shell-command cmd nil cmd)))

(defun exwm-rename-buffer ()
  (interactive)
  (let ((name (or my/window-name exwm-class-name)))
    (exwm-workspace-rename-buffer name)
    (setq-local exwm-instance-name name)
    (setq my/window-name nil)))

(defvar my/window-name nil)

(defmacro exec (body)
  `#'(lambda ()
       (interactive)
       ,body))

(provide 'my-exwmx-quickrun)
;;; my-exwmx-quickrun.el ends here
