;;; dashboard.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-04-15 16:58]

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

(defvar lambda-dashboard-specs
  '(("Your Todo's"
     ("[d]ev agenda" . (lambda () (interactive) (org-agenda nil "M")))
     ("[p]rod agenda" . (lambda () (interactive) (org-agenda nil "pa"))))
    ("Your Calendars"
     ("[b]iweekly calendar" . (lambda () (interactive) (org-agenda nil "c2"))))
    ("Your Reports")
    ("Other"
     ("e[m]ail" . notmuch))))

(defvar lambda-dashboard-mode-map nil)

(defvar lambda-dashboard-quotes '("Desperate times call for desperate measures"
                                  "To bring order out of chaos"
                                  "The key to navigating complexity is to know what to ignore"
                                  "Elegant Weapons for a more civilized age"))

(defun lambda-dashboard-bind-keys (spec)
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'(lambda () (interactive) (kill-buffer (current-buffer))))
    (dolist (front spec)
      (dolist (sub (cdr front))
        (let ((title (car sub))
              (function (cdr sub)))
          (string-match "\\[\\(.\\)\\]" title)
          (define-key map (match-string 1 title)
            function))))
    (setq lambda-dashboard-mode-map map)))

(defun lambda-dashboard-construct-buffer (spec)
  (let ((buffer (get-buffer-create "*lambda-dashboard*")))
    (set-buffer buffer)
    (insert
     (format "* %s - %s\n\n"
             (propertize "lambda-dashboard"
                         'face 'message-header-name)
             (propertize (nth (random (length lambda-dashboard-quotes)) lambda-dashboard-quotes)
                         'face font-lock-type-face)))
    (dolist (front spec)
      (insert
       (format "  %s\n\n"
               (propertize (car front)
                           'face 'font-lock-type-face)))
      (dolist (sub (cdr front))
        (insert
         (format "    * %s\n" (car sub))))
      (insert "\n"))
    (lambda-dashboard-bind-keys spec)
    (lambda-dashboard-mode)
    (goto-char 0)
    buffer))

(define-derived-mode lambda-dashboard-mode special-mode "lambda-dashboard-mode"
  )

(defun lambda-dashboard ()
  (interactive)
  (let ((buffer (lambda-dashboard-construct-buffer lambda-dashboard-specs)))
    (switch-to-buffer buffer)))

(provide 'dashboard)
;;; dashboard.el ends here
