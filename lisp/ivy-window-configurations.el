;;; ivy-window-configurations.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-07 10:10]

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

;;  I used workgroups2 before this, but workgroups2 had a lot of
;;  functionality that I didn't need, and it appears that it is
;;  because of this feature creep that workgroups2 will grind Emacs to
;;  a halt for long running sessions. I haven't had time to look into
;;  why this is the case. Instead I just threw this together.

;;; Code:

(defvar iwc/configurations nil)

(defun iwc-set-current-wc-name (name)
  (set-frame-parameter (selected-frame) 'iwc-current-wc-name
                       name))

(defun iwc-get-current-wc-name ()
  (frame-parameter (selected-frame) 'iwc-current-wc-name))

(define-minor-mode iwc-mode
  "My ivy implementation of Emacs' built in window-configuration
  management system."
  nil nil nil
  :global t
  (cond (iwc-mode
         (setq iwc/configurations (make-hash-table :test 'equal))
         (iwc-switch-to-wc "scratch"))
        (t
         (setq iwc/configurations nil)
         (dolist (f (frame-list))
           (with-selected-frame f
             (iwc-set-current-wc-name nil))))))

(defun iwc-active-on-frame? (wc-name)
  (when-let (f (catch 'return
                 (dolist (f (frame-list))
                   (with-selected-frame f
                     (when (string= wc-name
                                    (iwc-get-current-wc-name))
                       (throw 'return f))))))
    (if (equal f (selected-frame))
        (message "Already on wc: \"%s\"" wc-name)
      (message "wc \"%s\" active on another frame" wc-name))))

(defun iwc-switch-to-wc (wc-name)
  (interactive (list (ivy-completing-read "wc? " (hash-table-keys iwc/configurations)
                                          nil nil )))
  (when (not iwc-mode)
    (error "Not in iwc-mode"))
  (unless (iwc-active-on-frame? wc-name)
    (awhen (iwc-get-current-wc-name)
      (puthash it
               (current-window-configuration)
               iwc/configurations))
    (awhen (gethash wc-name iwc/configurations)
      (set-window-configuration it))
    (iwc-set-current-wc-name wc-name)
    (puthash wc-name nil iwc/configurations)))

(defun iwc-rename (name)
  (interactive (list (read-from-minibuffer "Name? ")))
  (when (not iwc-mode)
    (error "Not in iwc-mode"))
  (remhash (iwc-get-current-wc-name) iwc/configurations)
  (puthash name nil iwc/configurations)
  (iwc-set-current-wc-name name))

;; (unless (iwc-active-on-frame? "scratch")
;;   (remhash "scratch" iwc/configurations))

(provide 'ivy-window-configurations)
;;; ivy-window-configurations.el ends here
