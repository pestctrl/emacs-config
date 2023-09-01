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
(use-package hydra)
(require 'exwmx-appconfig)
(require 'my-exwmx-quickrun-2)
(require 'exwmx-firefox)
(require 'exwm-launch-program)

(defun lock-screen ()
  (interactive)
  (shell-command "~/Github/my-projects/i3lock-fancy/i3lock-fancy & disown"))

(defun exwmx-prompt-pretty-name (prompt appconfigs)
  (completing-read
   prompt
   (mapcar (lambda (a) (plist-get a :pretty-name))
           appconfigs)))

(defun exwmx-name-buffer (&optional name buffer appconfigs)
  (interactive)
  (setq appconfigs (or appconfigs (exwmx-appconfig--get-all-appconfigs))
        buffer (or buffer (current-buffer))
        name (or name (exwmx-prompt-pretty-name "Name: " appconfigs)))
  (with-current-buffer buffer
    (cl-labels ((name-pretty (buffer name)
                  (with-current-buffer buffer
                    (exwm-workspace-rename-buffer name)
                    (setq-local exwmx-pretty-name name))
                  name))
      (if (equal name (buffer-name))
          (setq-local exwmx-pretty-name name)
        (let ((obuffer (get-buffer name)))
          (if (not obuffer)
              (name-pretty (current-buffer) name)
            (if (not (y-or-n-p (format "Already a buffer named \"%s\". Would you like to swap?" name)))
                exwmx-pretty-name
              (let ((oname (exwmx-prompt-pretty-name
                            "Name of other buffer: "
                            (remove-if #'(lambda (x)
                                           (string= name (plist-get x :pretty-name)))
                                       appconfigs))))
                (name-pretty (current-buffer)
                             "This is a stupid name that no one would ever choose for a buffer, hopefully")
                (name-pretty obuffer oname)
                (name-pretty (current-buffer)
                             name)))))))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)

(defun simulate-C-t (arg)
  (interactive "P")
  (if (eq major-mode 'exwm-mode)
      (exwm-input--fake-key ?\C-t)
    (transpose-chars arg)))

;; (key-binding (kbd "C-t C-t"))

(use-package zeal-at-point
  :bind (:map *root-map*
              ("d" . #'zeal-at-point)))

(defun my/reset-default-directory ()
  (interactive)
  (setq default-directory (expand-file-name "~/")))

(define-key *root-map* (kbd "C-t") #'simulate-C-t)
(define-key *root-map* (kbd "C-p") #'exwmx-launch-program)
(define-key *root-map* (kbd "r") #'exwmx-name-buffer)
(define-key *root-map* (kbd "D") #'my/reset-default-directory)

(define-key *root-map* (kbd "C-d") (quickrun-lambda "zeal" "zeal"))
(define-key *root-map* (kbd "e") (quickrun-lambda "emacs" "emacs"))
(define-key *root-map* (kbd "s") (quickrun-lambda "steam" nil))
(define-key *root-map* (kbd "V") (quickrun-lambda "VBoxManage startvm \"Windows 7\"" "VirtualBox Machine"))
(define-key *root-map* (kbd ")") (exec ;;(leaving-computer)
                                  (shell-command "sleep 2s ; xset dpms force off")))

;; Firefox window management
(define-prefix-command '*firefox-map*)
(define-key *root-map* (kbd "f") '*firefox-map*)

(defmacro quickrun-firefox (command tag)
  (let ((name-gensym (intern (format "quickrun-comm-%s"
                                     (or tag command)))))
    `(defun ,name-gensym ()
       (interactive)
       (if (not (zerop (length (exwmx-find-buffers '(:class "firefox")))))
           (my/exwmx-quickrun ,command nil '(:pretty-name ,tag))
         (exwmx-notify-x-window (exwmx-appconfig--search '((:pretty-name ,tag))))
         (exwmx/launch-firefox-windows)))))

(define-key *firefox-map* (kbd "c") (quickrun-lambda "google-chrome-stable" "chrome"))
(define-key *firefox-map* (kbd "f") (quickrun-firefox "firefox" "firefox"))
(define-key *firefox-map* (kbd "1") (quickrun-firefox "firefox" "firefox1"))
(define-key *firefox-map* (kbd "2") (quickrun-firefox "firefox" "firefox2"))
(define-key *firefox-map* (kbd "3") (quickrun-firefox "firefox" "firefox3"))
(define-key *firefox-map* (kbd "4") (quickrun-firefox "firefox" "firefox4"))
(define-key *firefox-map* (kbd "d") (quickrun-firefox "firefox" "development"))
(define-key *firefox-map* (kbd "s") (quickrun-firefox "firefox" "school"))
(define-key *firefox-map* (kbd "w") (quickrun-firefox "firefox" "work"))
(define-key *firefox-map* (kbd "y") (quickrun-firefox "firefox" "youtube"))

;; Musics
(define-prefix-command '*music-map*)
(define-key *root-map* (kbd "m") '*music-map*)

(define-key *music-map* (kbd "SPC") (exec (run-clementine-command "-t")))
(define-key *music-map* (kbd "n") (exec (run-clementine-command "--next")))
(define-key *music-map* (kbd "p") (exec (run-clementine-command "--previous")))
(define-key *music-map* (kbd "r") (exec (run-clementine-command "--restart-or-previous")))
(defhydra clementine-volume-hydra (*music-map* "v")
  "Clementine volume up and down"
  ("j" (lambda () (interactive) (run-clementine-command "--volume-down")))
  ("J" (lambda () (interactive) (run-clementine-command "--volume-decrease-by 25")))
  ("k" (lambda () (interactive) (run-clementine-command "--volume-up")))
  ("K" (lambda () (interactive) (run-clementine-command "--volume-increase-by 25")))
  ("q" nil))

(use-package pulseaudio-control)

(defhydra volume-hydra (*root-map* "v")
  "volume up and down"
  ("j" (lambda () (interactive) (pulseaudio-control-decrease-volume)))
  ("k" (lambda () (interactive) (pulseaudio-control-increase-volume)))
  ("q" nil))

(defun run-clementine-command (arg)
  (let ((pid (shell-command-to-string "pgrep clementine")))
    (if (< 0 (length pid))
        (shell-command (format "clementine %s" arg))
      (launch-program "clementine")
      (run-with-timer 4 nil #'run-clementine-command arg))))

(add-to-list 'hydra-props-alist
             '(clementine-volume-hydra :verbosity 0))

(provide 'exwm-tag)
;;; exwm-tag.el ends here
