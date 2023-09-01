;;; exwmx-firefox.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-09-01 14:47]

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
(require 'exwmx-appconfig-predicates)
(require 'my-exwmx-quickrun-2)

(add-to-list 'exwmx/disable-my-management
             #'exwmx/add-firefox-buffers)

(defvar exwmx/firefox-buffers nil)
(defvar exwmx/buffer-name-timer nil)

(setq exwm-input--update-focus-interval 0.1)

(defun exwmx/launch-firefox-windows ()
  (interactive)
  (unless (zerop (length (exwmx-find-buffers '(:class "firefox"))))
    (user-error "Firefox already launched"))
  (add-hook 'exwm-manage-finish-hook #'exwmx/add-firefox-buffers)
  (my/exwmx-quickrun "firefox" nil nil t))

(defun exwmx/add-firefox-buffers ()
  (push (current-buffer) exwmx/firefox-buffers)
  (when (timerp exwmx/buffer-name-timer)
    (cancel-timer exwmx/buffer-name-timer))
  (setq exwmx/buffer-name-timer
        (run-at-time 4 nil #'exwmx/rename-firefox-windows)))

;; (setq exwm-manage-finish-hook
;;       (delq #'exwmx/add-firefox-buffers exwm-manage-finish-hook))

(defun display-buffers-split-vertically (lob)
  (delete-other-windows)
  (let ((last (car (last lob))))
    (dolist (b lob)
      (display-buffer-same-window b nil)
      (unless (eq b last)
        (select-window (split-window-vertically)))))
  (balance-windows))

;; (defun exwmx/rename-firefox-windows ()
;;   (interactive)
;;   (cl-labels ((rename (buffer name)
;;                 (with-current-buffer buffer
;;                   (exwm-workspace-rename-buffer name)
;;                   (setq-local exwmx-pretty-name name))))
;;     (let ((buffers
;;            (remove-if-not
;;             #'(lambda (buffer)
;;                 (with-current-buffer buffer
;;                   (and (eq 'exwm-mode major-mode)
;;                        (string-match-p "firefox" exwm-class-name))))
;;             (buffer-list)))
;;           yt others)
;;       (while buffers
;;         (let ((buffer (pop buffers)))
;;           (with-current-buffer buffer
;;             (cond ((string-match-p "youtube" (downcase exwm-title))
;;                    (setq yt buffer))
;;                   (t (push buffer others))))))
;;       (when yt
;;         (rename yt "youtube"))
;;       (dotimes (i (length others))
;;         (let ((buffer (nth i others)))
;;           (rename buffer
;;                   (if (eq 0 i)
;;                       "firefox"
;;                     (concat "firefox" (number-to-string i)))))))))

(defun exwmx/rename-firefox-windows ()
  (unwind-protect
      (let ((appconfigs (exwmx-appconfig--search-all '((:class "firefox"))))
            (buffers exwmx/firefox-buffers))
        (when-let ((b (exwmx-find-buffer '((:title "YouTube" string-match-p)) t)))
          (when (member b buffers)
            (exwmx-name-buffer "youtube" b )
            (setq buffers
                  (delete b buffers))
            (setq appconfigs
                  (remove-if #'(lambda (x)
                                 (string= "youtube" (plist-get x :pretty-name)))
                             appconfigs))))
        (if (= 1 (length buffers))
            (exwmx-name-buffer "firefox" (car buffers))
          (save-window-excursion
            (delete-other-windows)
            (let ((last (car (last buffers))))
              (display-buffers-split-vertically buffers)
              (dolist (b buffers)
                (let ((win (get-buffer-window b)))
                  (select-window win)
                  (-->
                   (funcall-interactively #'exwmx-name-buffer nil b appconfigs)
                   (setq appconfigs
                         (remove-if #'(lambda (x)
                                        (string= it
                                                 (plist-get x :pretty-name)))
                                    appconfigs)))
                  (unless (eq b last)
                    (delete-window win)))))))
        (when-let ((res
                    (-some
                     (lambda (buffer)
                       (let ((candidates (exwmx-appconfig-candidates buffer)))
                         (cl-assert (= 1 (length candidates)))
                         (when-let ((window (gethash (car candidates)
                                                     exwmx/destination-windows)))
                           (cons buffer window))))
                     exwmx/firefox-buffers)))
          (window--display-buffer (car res) (cdr res) 'reuse nil)))
    (remove-hook 'exwm-manage-finish-hook #'exwmx/add-firefox-buffers)
    (setq exwmx/firefox-buffers nil)))

(provide 'exwmx-firefox)
;;; exwmx-firefox.el ends here
