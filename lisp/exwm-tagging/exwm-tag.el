;;; exwm-tag.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-23 08:11]

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
(use-package exwm-x)
(require 'exwmx-quickrun) 

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

(use-package dmenu)            

(make-thread 
 #'dmenu--cache-executable-files)


(defun read-program ()
  (funcall #'ido-completing-read "$ "
           (append dmenu--history-list
                   (cl-remove-if (lambda (x)
                                   (member x dmenu--history-list))
                                 dmenu--cache-executable-files))))

(defun launch-program (command &optional process-name)
  (interactive (list (read-program)))
  (setq dmenu--history-list (cons command (remove command dmenu--history-list)))
  (when (> (length dmenu--history-list)
           dmenu-history-size)
    (setcdr (nthcdr (- dmenu-history-size 1)
                    dmenu--history-list)
            nil))
  (let ((name (or process-name command)))
    (start-process-shell-command name nil command)))

(defun exwmx-launch-program (command &optional process-name)
  g    (interactive (list (read-program)))
  (setq dmenu--history-list (cons command (remove command dmenu--history-list)))
  (when (> (length dmenu--history-list)
           dmenu-history-size)
    (setcdr (nthcdr (- dmenu-history-size 1)
                    dmenu--history-list)
            nil))
  (exwmx-quickrun command))

;; (exwmx-quickrun "firefox" nil '(:class ".*" :instance "School"))

;; (exwmx-quickrun--find-buffer '(:class ".*" :instance "Hello"))


(setq i3-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 i3")
(setq xfce4-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 xfce4-session")
(setq kde-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 startkde")
(setq kde+exwm-string "Xephyr -br -ac -noreset -resizeable -screen 1920x1080 :8 & sleep 1s; DISPLAY=:8 KDEWM=/usr/bin/emacs startkde")
(defvar exwm-startup-programs
  '("megasync"
    "deadd-notification-center"
    "/usr/lib/kdeconnectd"
    ("compton -f -i .7 -b")
    ;; ("compton -f -i .7 -b --backend glx --blur-background --blur-method kawase --blur-strength 2")
    ("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1")
    ("/usr/lib/notification-daemon-1.0/notification-daemon")
    ("nm-applet")
    ))
(defvar hard-drive-space "")

(defun launch-i3 ()
  (interactive)
  (launch-program i3-string))

(defun launch-xfce ()
  (interactive)
  (launch-program xfce4-string))

(defun launch-kde ()
  (interactive)
  (launch-program kde-string))

(defun launch-kde+emacs ()
  (interactive)
  (launch-program kde-string))

(defun lock-screen ()
  (interactive)
  (shell-command "~/Github/my-projects/i3lock-fancy/i3lock-fancy & disown"))

(setq enable-recursive-minibuffers t)
(defun counsel-shell-command ()
  "Forward to `shell-command'."
  (interactive)
  (ivy-read "Shell Command: "
            shell-command-history
            :caller 'counsel-shell-command))

(defun dmenu-run ()
  (interactive)
  (shell-command "dmenu" nil "dmenu_run -b"))

(defun call-startup-programs ()
  (dolist (program exwm-startup-programs)
    (if (listp program)
      (launch-program (car program) (cadr program))
      (launch-program program))))

(defun get-hard-drive-space ()
  (shell-command-to-string "df -h -P -l / | tail -n 1 | tr -s ' ' | cut -d ' ' -f 4"))

(defun update-hard-drive-space-string ()
  (setq hard-drive-space
        (let ((space-left (get-hard-drive-space)))
          (propertize (concat " "
                              (substring space-left
                                         0
                                         (1- (length space-left))))
                      'face 'sml/time))))

(defun display-hard-drive-space-mode ()
  (if (not (member 'hard-drive-space
                   global-mode-string))
      (add-to-list 'global-mode-string
                   'hard-drive-space
                   t)))

(defvar my/window-name nil)

(defun exwmx-name-buffer ()
  (interactive)
  (let* ((xprograms (mapcar (lambda (a) (plist-get a :instance)) (exwmx-appconfig--get-all-appconfigs)))
         (name (completing-read "Name: " xprograms)))
    (if (and (get-buffer name)
             (not (equal (get-buffer name) (current-buffer)))
             (y-or-n-p (format "Already a buffer named \"%s\". Would you like to swap?" name)))
        (let ((oname (completing-read "Name of other buffer: " xprograms)))
          (exwm-workspace-rename-buffer "This is a stupid name that no one would ever choose for a buffer, hopefully")
          (save-window-excursion
            (switch-to-buffer (get-buffer name))
            (exwm-workspace-rename-buffer oname)
            (setq-local exwm-instance-name oname))
          (exwm-workspace-rename-buffer name)
          (setq-local exwm-instance-name name))
      (exwm-workspace-rename-buffer name)
      (setq-local exwm-instance-name name))))

(defun exwm-rename-buffer ()
  (interactive)
  (when my/window-name
    (exwm-workspace-rename-buffer my/window-name)
    (setq-local exwm-instance-name my/window-name)
    (setq my/window-name nil)))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-manage-finish-hook 'exwm-rename-buffer)

(defun launch-program-with-name (cmd name)
  (interactive)
  (when name (setq my/window-name name))
  (start-process-shell-command cmd nil cmd))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name (string= exwm-class-name "Emacs"))
              (exwm-input-set-local-simulation-keys nil))))

(defmacro exec (body)
  `(lambda ()
     (interactive)
     ,body))

(defun toggle-notifications ()
  (interactive)
  (shell-command "kill -s USR1 $(pidof deadd-notification-center)"))

(add-to-list 'exwm-input-prefix-keys ?\C-t)
(defun simulate-C-t (arg)
  (interactive "P")
  (if (eq major-mode 'exwm-mode)
      (exwm-input--fake-key ?\C-t)
    (transpose-chars arg)))
(use-package zeal-at-point)
(define-key *root-map* (kbd "C-d") (quickrun-lambda "zeal" "zeal"))
(define-key *root-map* (kbd "d") #'zeal-at-point)
(define-key *root-map* (kbd "C-t") 'simulate-C-t)
(define-key *root-map* (kbd "C-p") 'exwmx-launch-program)
(define-key *root-map* (kbd "e") (quickrun-lambda "emacs" "emacs"))
(define-key *root-map* (kbd "s") (quickrun-lambda "steam" nil))
(define-key *root-map* (kbd "V") (quickrun-lambda "VBoxManage startvm \"Windows 7\"" "VirtualBox Machine"))
(define-key *root-map* (kbd "r") 'exwmx-name-buffer)
(define-key *root-map* (kbd ")") (lambda () (interactive) (leaving-computer) (shell-command "sleep 2s ; xset dpms force off")))

(define-prefix-command '*window-map*)
(define-key *root-map* (kbd "w") '*window-map*)
(define-key *window-map* (kbd "y") 'youtube-split)
(define-key *window-map* (kbd "Y") 'big-youtube-split)
(define-key *window-map* (kbd "j") 'side-bottom-window)
(define-key *window-map* (kbd "h") 'side-left-window)
(define-key *window-map* (kbd "l") 'side-right-window)
(define-key *window-map* (kbd "d") 'window-toggle-side-windows)

(define-prefix-command '*firefox-map*)
(define-key *firefox-map* (kbd "c") (quickrun-lambda "google-chrome-stable" "chrome"))
(define-key *firefox-map* (kbd "f") (quickrun-lambda "firefox" "firefox"))
(define-key *firefox-map* (kbd "1") (quickrun-lambda "firefox" "firefox1"))
(define-key *firefox-map* (kbd "2") (quickrun-lambda "firefox" "firefox2"))
(define-key *firefox-map* (kbd "3") (quickrun-lambda "firefox" "firefox3"))
(define-key *firefox-map* (kbd "4") (quickrun-lambda "firefox" "firefox4"))
(define-key *firefox-map* (kbd "d") (quickrun-lambda "firefox" "development"))
(define-key *firefox-map* (kbd "s") (quickrun-lambda "firefox" "school"))
(define-key *firefox-map* (kbd "w") (quickrun-lambda "firefox" "work"))
(define-key *firefox-map* (kbd "y") (quickrun-lambda "firefox" "youtube"))

(define-key *root-map* (kbd "f") '*firefox-map*)

(define-prefix-command '*music-map*)
(define-key *music-map* (kbd "SPC") (exec (shell-command "clementine -t")))
(define-key *music-map* (kbd "n") (exec (shell-command "clementine --next")))
(define-key *music-map* (kbd "p") (exec (shell-command "clementine --previous")))
(define-key *music-map* (kbd "r") (exec (shell-command "clementine --restart-or-previous")))
(defhydra clementine-volume-hydra (*music-map* "v")
  "Clementine volume up and down"
  ("j" (lambda () (interactive) (shell-command "clementine --volume-down")))
  ("J" (lambda () (interactive) (shell-command "clementine --volume-decrease-by 25")))
  ("k" (lambda () (interactive) (shell-command "clementine --volume-up")))
  ("K" (lambda () (interactive) (shell-command "clementine --volume-increase-by 25")))
  ("q" nil))

(define-key *root-map* (kbd "m") '*music-map*)

(provide 'exwm-tag)
;;; exwm-tag.el ends here
