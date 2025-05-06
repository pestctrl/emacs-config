;;; multi-vterm-tabs.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-01-26 11:20]

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
(require 'cl-lib)
(require 'dash)
(require 'vterm)
(require 'tab-bar)

(defclass multi-vterm-tab-info ()
  ((free-numbers :type list :initform nil)
   (max-number :type number :initform -1)
   (recent-buffer :initform nil)))

(defvar mvt/info (make-hash-table))

(defun mvt/get-or-create-info (symbol)
  (or (gethash symbol mvt/info)
      (puthash symbol (make-instance 'multi-vterm-tab-info)
               mvt/info)))

;; (setq mvt/info nil)

(defun mvt/format-buffer-name (tab-name index)
  "Format vterm buffer name with INDEX."
  (format "*%s-vterm<%s>*" tab-name index))

(defvar mvt/regex
  (rx (and "*" (group (+ nonl)) "-vterm<" (group (+ digit)) ">" "*")))

(defun mvt/create-buffer (tab-name)
  (interactive
   (list (alist-get 'name (tab-bar--current-tab))))
  (let* ((mvti (gethash (intern tab-name) mvt/info))
         (index (or (pop (slot-value mvti 'free-numbers))
                    (cl-incf (slot-value mvti 'max-number))))
         vterm-name)
    (setq vterm-name (mvt/format-buffer-name tab-name index))
    (-->
     (or (get-buffer vterm-name)
         (with-current-buffer (generate-new-buffer vterm-name)
           (vterm-mode)
           (mvt/minor-mode)
           (current-buffer)))
     (setf (slot-value mvti 'recent-buffer)
           it)
     (if (called-interactively-p)
         (switch-to-buffer it)
       it))))

(defun mvt/get-all-buffers (tab-name)
  (let* ((tab-sym (intern tab-name))
         (mvti (mvt/get-or-create-info tab-sym))
         (max-num (slot-value mvti 'max-number))
         buffs)
    (dotimes (i max-num)
      (awhen (get-buffer (mvt/format-buffer-name tab-name i))
        (push it buffs)))
    (reverse buffs)))

;; (mvt/get-all-buffers (alist-get 'name (tab-bar--current-tab)))

(defun mvt/compact-terminals (tab-name)
  (interactive
   (list (alist-get 'name (tab-bar--current-tab))))
  (let* ((tab-sym (intern tab-name))
         (mvti (mvt/get-or-create-info tab-sym))
         (all-buffers (mvt/get-all-buffers tab-name))
         (iter 0))
    (dolist (buff all-buffers)
      (with-current-buffer buff
        (rename-buffer (mvt/format-buffer-name tab-name iter)))
      (incf iter))
    (setf (slot-value mvti 'free-numbers) nil
          (slot-value mvti 'max-number) (1- iter))))

(defun mvt/extract-info ()
  (when mvt/minor-mode
    (let ((buffer-name (buffer-name)))
      (when (string-match mvt/regex buffer-name)
        (cons (match-string 1 buffer-name)
              (string-to-number (match-string 2 buffer-name)))))))

(defun mvt/next ()
  (interactive)
  (let* ((info (mvt/extract-info))
         (tab-name (car info))
         (start (cdr info))
         (mvt-info (mvt/get-or-create-info (intern tab-name)))
         (max-num
          (-->
           (slot-value mvt-info 'max-number)
           (1+ it)))
         last-buffer)
    (while (progn
             (when (not (= max-num 0))
               (setq start (mod (1+ start) max-num)))
             (not (setq last-buffer
                        (get-buffer (mvt/format-buffer-name tab-name start))))))
    (setf (slot-value mvt-info 'recent-buffer)
          last-buffer)
    (switch-to-buffer last-buffer)
    (set-window-prev-buffers (selected-window)
                             (cdr (window-prev-buffers (selected-window))))))

;; (gethash 'emacs-devel mvt/info)

(defun mvt/kill-hook ()
  (when mvt/minor-mode
    (let ((info (mvt/extract-info)))
      (push (cdr info)
            (slot-value (mvt/get-or-create-info (intern (car info)))
                        'free-numbers))
      (mvt/next))))

(defvar mvt/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'mvt/next)
    (define-key map (kbd "M-c") #'mvt/create-buffer)
    map))

(define-minor-mode mvt/minor-mode ""
  :global nil
  :keymap mvt/minor-mode-map
  (when mvt/minor-mode
    (add-hook 'kill-buffer-hook #'mvt/kill-hook)))

(defun multi-vterm-tab (arg)
  "Create new vterm buffer."
  (interactive "P")
  (let* ((tab-name (alist-get 'name (tab-bar--current-tab)))
         (tab-sym (intern tab-name))
         (mvti (mvt/get-or-create-info tab-sym)))
    (let ((buffer (slot-value mvti 'recent-buffer)))
      (switch-to-buffer
       (or (and buffer
                (buffer-live-p buffer)
                (not arg)
                buffer)
           (mvt/create-buffer tab-name mvti))))))

(defun mvt/find-all-terms-in-tab (tab-name)
  (remove-if-not #'(lambda (b)
                     (with-current-buffer b
                       (let ((info (mvt/extract-info)))
                         (and info
                              (string= tab-name (car info))))))
                 (buffer-list)))

(defun mvt/rename-tab (orig new-tab-name &optional arg)
  (let ((old-tab-name (alist-get 'name (tab-bar--current-tab))))
    (funcall orig new-tab-name arg)
    (let ((old-sym (intern old-tab-name))
          (new-sym (intern new-tab-name)))
      (puthash new-sym (gethash old-sym mvt/info)
               mvt/info)
      (remhash old-sym mvt/info))
    (dolist (b (mvt/find-all-terms-in-tab old-tab-name))
      (with-current-buffer b
        (let ((info (mvt/extract-info)))
          (rename-buffer (mvt/format-buffer-name new-tab-name (cdr info))))))))

(advice-add #'tab-bar-rename-tab
            :around
            #'mvt/rename-tab)

(defun mvt/close-tab (orig)
  (let ((current-tab-name (alist-get 'name (tab-bar--current-tab))))
    (when (funcall orig)
      (dolist (b (mvt/find-all-terms-in-tab old-tab-name))
        (with-current-buffer b
          (vterm-send-C-d))))))

(advice-add #'close-tab-switch
            :around
            #'mvt/close-tab)

(provide 'multi-vterm-tabs)
;;; multi-vterm-tabs.el ends here
