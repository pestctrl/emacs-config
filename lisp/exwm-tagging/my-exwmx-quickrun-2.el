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

(defun exwmx-appconfig ()
  "EXWM-X's application configure tool, which will pop to a buffer.
and insert an appconfig template to let user edit. then user can
use `exwmx-appconfig-file' to save the appconfig to `exwmx-appconfig-file'
or use `exwmx-appconfig-ignore' ignore."
  (interactive)
  (if (not (derived-mode-p 'exwm-mode))
      (message "EXWM-X: Current window is not a window of application.")
    (unless (file-readable-p exwmx-appconfig-file)
      (append-to-file "" nil exwmx-appconfig-file))
    (let* ((buffer (get-buffer-create exwmx-appconfig-buffer))
           (hash (md5 (concat exwm-class-name exwm-instance-name (or exwmx-pretty-name ""))))
           (history (exwmx-appconfig--search
                     `((:key ,hash))))
           (appconfig (list :command exwm-instance-name
                            :alias exwm-instance-name
                            :pretty-name exwmx-pretty-name
                            :paste-key exwmx-sendstring-default-paste-key
                            :class exwm-class-name
                            :instance exwm-instance-name
                            :title exwm-title
                            :floating nil
                            :size-and-position 'default
                            :workspace 'current-workspace
                            :add-prefix-keys nil
                            :remove-prefix-keys nil
                            :ignore-simulation-keys nil
                            :eval nil)))
      (while history
        (let ((prop (pop history))
              (value (pop history)))
          (when (keywordp prop)
            (plist-put appconfig prop value))))
      (plist-put appconfig :key hash)
      (with-current-buffer buffer
        (text-mode)
        (exwmx-appconfig-mode)
        (setq truncate-lines t)
        (erase-buffer)
        (exwmx-appconfig--insert-plist appconfig)
        (goto-char (point-min))
        (setq header-line-format
              (substitute-command-keys
               (concat
                "\\<exwmx-appconfig-mode-map>"
                "Appconfig: "
                "Finish with `\\[exwmx-appconfig-finish]', "
                "Ignore with `\\[exwmx-appconfig-ignore]'. "))))
      (pop-to-buffer buffer))))

(defun exwmx-buffer-match-p (appconfig buffer)
  (cl-assert
   (eq 'exwm-mode
       (with-current-buffer buffer
         major-mode)))
  (cl-assert
   (plistp appconfig))
  (let ((class (plist-get appconfig :class))
        (instance (plist-get appconfig :instance))
        (title (plist-get appconfig :title))
        (pretty (plist-get appconfig :pretty-name)))
    (with-current-buffer buffer
      (and
       (or class instance title pretty)
       (or (not class) (exwmx--string-match-p class exwm-class-name))
       (or (not instance) (exwmx--string-match-p instance exwm-instance-name))
       (or (not title) (exwmx--string-match-p instance exwm-title))
       (or (not pretty) (exwmx--string-match-p (format "^%s$" pretty) exwmx-pretty-name))))))

(defun exwmx-buffer-match-alist (alist buffer)
  (cl-assert
   (eq 'exwm-mode
       (with-current-buffer buffer
         major-mode)))
  (with-current-buffer buffer
    (-every
     (lambda (rule)
       (let* ((key (nth 0 rule))
              (search-string (nth 1 rule))
              (test-function (or (nth 2 rule) #'equal))
              (prop-value
               (pcase key
                 (:class exwm-class-name)
                 (:instance exwm-instance-name)
                 (:title exwm-title)
                 (:pretty-name exwmx-pretty-name))))
         (and (functionp test-function)
              (funcall test-function search-string prop-value))))
     alist)))

(defun exwmx-find-buffer (appconfig &optional alist)
  (let ((current (current-buffer))
        (buffers (buffer-list))
        (result '()))
    (while buffers
      (let ((buffer (pop buffers)))
        (when (and (eq 'exwm-mode (with-current-buffer buffer
                                    major-mode))
                   (if alist
                       (exwmx-buffer-match-alist appconfig buffer)
                     (exwmx-buffer-match-p appconfig buffer)))
          (push buffer result))))
    (setq result (reverse result))
    ;; If two more buffers are found, switch between these buffer.
    (if (and (cadr result)
             (eq (car result) current))
        (cadr result)
      (car result))))

(defun exwmx-find-buffers (appconfig &optional alist)
  (let ((current (current-buffer))
        (buffers (buffer-list))
        (result '()))
    (while buffers
      (let ((buffer (pop buffers)))
        (when (and (eq 'exwm-mode (with-current-buffer buffer
                                    major-mode))
                   (if alist
                       (exwmx-buffer-match-alist appconfig buffer)
                     (exwmx-buffer-match-p appconfig buffer)))
          (push buffer result))))
    (reverse result)))

(defun exwmx-select-window ()
  (if (= 1 (length (window-list)))
      (selected-window)
    (let ((index (switch-window--prompt "Run command in window: ")))
      (cl-loop for c from 1
               for win in (switch-window--list)
               until (= c index)
               finally return win))))

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
          (exwmx-register-x-window
           matched-appconfig (exwmx-select-window)))
        (exwmx-shell-command cmd)))))

(defmacro quickrun-lambda (cmd name)
  (let ((name-gensym (intern (format "quickrun-%s"
                                     (or name cmd)))))
    `(defun ,name-gensym ()
       (interactive)
       (my/exwmx-quickrun ,cmd nil '(:pretty-name ,name)))))

(add-to-list 'vertico-multiform-commands
             '(exwmx-launch-program flat (vertico-cycle . t)))

(defun read-program ()
  (completing-read
   "$ "
   (append dmenu--history-list
           (cl-remove-if (lambda (x)
                           (member x dmenu--history-list))
                         dmenu--cache-executable-files))))

(defun exwmx-launch-program (command &optional process-name)
  (interactive (list (read-program)))
  (setq dmenu--history-list (cons command (remove command dmenu--history-list)))
  (when (> (length dmenu--history-list)
           dmenu-history-size)
    (setcdr (nthcdr (- dmenu-history-size 1)
                    dmenu--history-list)
            nil))
  (my/exwmx-quickrun command))

(defmacro exec (body)
  `#'(lambda ()
       (interactive)
       ,body))

(defun exwmx/rename-firefox-windows ()
  (interactive)
  (cl-labels ((rename (buffer name)
                (with-current-buffer buffer
                  (exwm-workspace-rename-buffer name)
                  (setq-local exwmx-pretty-name name))))
    (let ((buffers
           (remove-if-not
            #'(lambda (buffer)
                (with-current-buffer buffer
                  (and (eq 'exwm-mode major-mode)
                       (string-match-p "firefox" exwm-class-name))))
            (buffer-list)))
          yt others)
      (while buffers
        (let ((buffer (pop buffers)))
          (with-current-buffer buffer
            (cond ((string-match-p "youtube" (downcase exwm-title))
                   (setq yt buffer))
                  (t (push buffer others))))))
      (when yt
        (rename yt "youtube"))
      (dotimes (i (length others))
        (let ((buffer (nth i others)))
          (rename buffer
                  (if (eq 0 i)
                      "firefox"
                    (concat "firefox" (number-to-string i)))))))))

;;------------------------------------------------------------------;;
(defvar exwmx/destination-windows (make-hash-table :test #'equal))

(defun exwmx-register-x-window (appconfig window)
  (puthash appconfig window exwmx/destination-windows))

(defun exwmx-appconfig--search-all (search-ruler-alist)
  (let ((appconfigs (exwmx-appconfig--get-all-appconfigs))
        appconfig-matched)
    (while appconfigs
      (let ((appconfig (pop appconfigs)))
        (dolist (rule search-ruler-alist)
          (let* ((key (nth 0 rule))
                 (search-string (nth 1 rule))
                  (test-function (or (nth 2 rule) #'equal))
                 (prop-value (plist-get appconfig key)))
            (when (and (functionp test-function)
                       (funcall test-function search-string prop-value))
              (push appconfig appconfig-matched))))))
    appconfig-matched))

(defun exwmx-appconfig-candidates ()
  (exwmx-appconfig--search-all
   `((:class ,exwm-class-name))))

(defun exwmx-manage-x-window ()
  (when (not (member #'exwmx/add-firefox-buffers exwm-manage-finish-hook))
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

(defvar exwmx/firefox-buffers nil)
(defvar exwmx/buffer-name-timer nil)

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
        (run-at-time 3 nil #'exwmx/rename-firefox-windows)))

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

(defun exwmx/rename-firefox-windows ()
  (unwind-protect
      (save-window-excursion
        (let ((appconfigs (exwmx-appconfig--search-all '((:class "firefox")))))
          (when-let ((b (exwmx-find-buffer '((:title "YouTube" string-match-p)) t)))
            (when (member b exwmx/firefox-buffers)
              (exwmx-name-buffer "youtube" b )
              (setq exwmx/firefox-buffers
                    (delete b exwmx/firefox-buffers))
              (setq appconfigs
                    (remove-if #'(lambda (x)
                                   (string= "youtube" (plist-get x :pretty-name)))
                               appconfigs))))
          (if (= 1 (length exwmx/firefox-buffers))
              (exwmx-name-buffer "firefox" (car exwmx/firefox-buffers))
            (delete-other-windows)
            (let ((last (car (last exwmx/firefox-buffers))))
              (display-buffers-split-vertically exwmx/firefox-buffers)
              (dolist (b exwmx/firefox-buffers)
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
                    (delete-window win))))))))
    (remove-hook 'exwm-manage-finish-hook #'exwmx/add-firefox-buffers)
    (setq exwmx/firefox-buffers nil)))

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
