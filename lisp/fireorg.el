;;; fireorg.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-21 11:23]

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

(defun fireorg/make-fireorg-window ()
  (interactive)
  (exwmx-quickrun "firefox -P youtube"
                  nil
                  (quote (:class ".*" :instance "fireorg"))))

(defun my/get-org-url ()
  (let ((link-str (car (org-offer-links-in-entry (current-buffer) (point) 0))))
    (or (cadr (get-text-property 0 'htmlize-link link-str)) link-str)))

(defmacro fireorg/open-link (ff &rest body)
  (declare (indent defun)
           (debug (&rest form)))
  `(let* ((url (my/get-org-url)))
     (prog2
         (start-process-shell-command "refile-open" nil (format "firefox -P youtube \"%s\"" url))
         (progn
           ,@body)
       (exwm-background/exwm-input--fake-key-to-window (exwm--buffer->id ff) ?\C-w))))

(defvar fireorg/ignore-focus-buffer nil)

(defun exwm--ignore-firefox-clientmessage (orig raw-data _synthetic)
  (let ((obj (make-instance 'xcb:ClientMessage)))
    (xcb:unmarshal obj raw-data)
    (unless (and (= xcb:Atom:_NET_ACTIVE_WINDOW
                    (slot-value obj 'type))
                 (eq fireorg/ignore-focus-buffer
                     (-> obj
                         (slot-value 'window)
                         (exwm--id->buffer))))
      (funcall orig raw-data _synthetic))))

(defun fireorg/ignore-focus (buffer)
  (setq fireorg/ignore-focus-buffer buffer)
  (advice-add #'exwm--on-ClientMessage :around
              #'exwm--ignore-firefox-clientmessage))

(defun fireorg/unignore-focus ()
  (setq fireorg/ignore-focus-buffer nil)
  (advice-remove #'exwm--on-ClientMessage
                 #'exwm--ignore-firefox-clientmessage))

(defmacro fireorg/setup (org ff &rest body)
  (declare (indent defun)
           (debug (place place &rest form)))
  `(progn
     (fireorg/ignore-focus ,ff)
     (unwind-protect
         (progn
           ;; (delete-other-windows)
           (switch-to-buffer ,org)
           (let ((w (selected-window)))
             (pop-to-buffer ,ff)
             (select-window w))

           ,@body

           (delete-other-windows))
       (fireorg/unignore-focus)           
       (exwm-background/exwm-input--fake-key-to-window (exwm--buffer->id ,ff) ?\C-q))))

;; (defmacro defun-fireorg-command (name arglist &optional docstring &rest body)
;;   (let* ((has-docstring (eq 'string (type-of docstring)))
;;          (body (if has-docstring
;;                    body
;;                  (cons docstring body)))
;;          (docstring (or (and has-docstring
;;                              docstring)
;;                         nil)))
;;     `(defun ,name ,arglist
;;        ,docstring
;;        )))

(provide 'fireorg)
;;; fireorg.el ends here
