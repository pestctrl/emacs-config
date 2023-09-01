;;; exwmx-appconfig-predicates.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-09-01 13:53]

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
(require 'exwmx-appconfig)

(defun my/exwmx-appconfig ()
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

(advice-add #'exwmx-appconfig
            :override
            #'my/exwmx-appconfig)

;;------------------------------------------------------------------;;
(defun exwm-buffer-extract-prop (buffer prop)
  (with-current-buffer buffer
    (pcase prop
      (:class exwm-class-name)
      (:instance exwm-instance-name)
      (:title exwm-title)
      (:pretty-name exwmx-pretty-name))))

(defun exwmx-buffer-match-p (appconfig buffer)
  (cl-assert
   (eq 'exwm-mode
       (with-current-buffer buffer
         major-mode)))
  (cl-assert
   (plistp appconfig))
  (exwmx-buffer-match-alist
   (let ((configs appconfig)
         result)
     (while configs
       (push (list (pop configs) (pop configs))
             result))
     (reverse result))
   buffer))

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
              (prop-value (exwm-buffer-extract-prop buffer key)))
         (and (functionp test-function)
              (funcall test-function search-string prop-value))))
     alist)))

(defun exwmx-find-buffers (appconfig &optional alist)
  (let (result)
    (dolist (buffer (buffer-list))
      (when (and (eq 'exwm-mode (with-current-buffer buffer
                                  major-mode))
                 (if alist
                     (exwmx-buffer-match-alist appconfig buffer)
                   (exwmx-buffer-match-p appconfig buffer)))
        (push buffer result)))
    (reverse result)))

(defun exwmx-find-buffer (appconfig &optional alist)
  (let ((current (current-buffer))
        (result (exwmx-find-buffers appconfig alist)))
    ;; If two more buffers are found, switch between these buffer.
    (if (and (cadr result)
             (eq (car result) current))
        (cadr result)
      (car result))))

(defun exwmx-appconfig--search-all (search-ruler-alist)
  (let ((appconfigs (exwmx-appconfig--get-all-appconfigs))
        appconfig-matched)
    (while appconfigs
      (let ((appconfig (pop appconfigs)))
        (when (-all?
               (lambda (rule)
                 (let* ((key (nth 0 rule))
                        (search-string (nth 1 rule))
                        (test-function (or (nth 2 rule) #'equal))
                        (prop-value (plist-get appconfig key)))
                   (and (functionp test-function)
                        (funcall test-function search-string prop-value)) ))
               search-ruler-alist)
          (push appconfig appconfig-matched))))
    appconfig-matched))

(defun exwmx-appconfig-candidates (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (exwmx-appconfig--search-all
     (cons `(:class ,exwm-class-name)
           (and exwmx-pretty-name
                `((:pretty-name ,exwmx-pretty-name)))))))

(provide 'exwmx-appconfig-predicates)
;;; exwmx-appconfig-predicates.el ends here
