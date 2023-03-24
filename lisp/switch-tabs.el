;;; switch-tabs.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-02-02 11:28]

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
(if (and (<= 30 emacs-major-version)
         (< 0 emacs-minor-version))
    (message "No longer need this patch, hopefully!")
  (require 'tab-bar-patch))

(setq tab-bar-show nil
      tab-bar-close-tab-select 'recent
      tab-bar-close-button-show nil)

(defvar switch-tabs/special-tabs '("scratch" "emacs-devel" "org" "org-spec"))

(defun my/read-tab-name ()
  (let ((current-tab (alist-get 'name (tab-bar--current-tab))))
    (->> (tab-bar--tabs-recent)
      (mapcar #'(lambda (tab)
                  (alist-get 'name tab)))
      (remove-if #'(lambda (tab-name)
                     (string= tab-name current-tab)))
      (ido-completing-read (format "Switch to tab (%s): "
                                   current-tab)))))

(defun switch-or-create-tab (tab-name)
  (interactive
   (list (my/read-tab-name)))
  (if (active-minibuffer-window)
      (message "Minibuffer still active. Close before switching tabs... ")
    (let ((tab-index (tab-bar--tab-index-by-name tab-name)))
      (if tab-index
          (progn
            (cl-incf tab-index)
            (tab-bar-select-tab tab-index)
            tab-index)
        (let* ((spec (member tab-name switch-tabs/special-tabs))
               (tab-bar-new-tab-to (if spec 'leftmost 'rightmost)))
          (tab-bar-new-tab))
        (tab-bar-rename-tab tab-name)
        (tab-bar--current-tab-index)))))

(defun last-tab ()
  (interactive)
  (->> (tab-bar--tabs-recent)
       (car)
       (alist-get 'name)
       (tab-bar--tab-index-by-name)
       (1+)
       (tab-bar-select-tab)))

(defun close-tab-switch ()
  (interactive)
  (let ((old-name (alist-get 'name (tab-bar--current-tab))))
    (when (y-or-n-p (format "Close tab \"%s\"? "
                            old-name))
      (tab-bar-close-tab))))

(defun tab-bar-report ()
  (interactive)
  (message
   (concatenate 'string "Current Tabs: "
                (mapconcat #'(lambda (tab)
                               (alist-get 'name tab))
                           (funcall tab-bar-tabs-function)
                           ", "))))

(defun my/show-tab-bar ()
  (interactive)
  (setq tab-bar-show (not tab-bar-show))
  (tab-bar-mode tab-bar-show))

(defun my/get-free-tab-name ()
  (let (result)
    (while (not result)
      (let ((name (int-to-string (+ 10000000000 (random 10000000000)))))
        (unless (tab-bar--tab-index-by-name name)
          (setq result name))))
    result))

(defun my/tab-bar-swap-tabs ()
  (interactive)
  (let ((current-tab (alist-get 'name (tab-bar--current-tab)))
        (switch-tab (my/read-tab-name))
        (temp-name (my/get-free-tab-name)))
    (tab-bar-rename-tab temp-name)
    (switch-or-create-tab switch-tab)
    (tab-bar-rename-tab current-tab)
    (switch-or-create-tab temp-name)
    (tab-bar-rename-tab switch-tab)))

(defvar my/scratch-tab-number 0)

(defun my/tab-bar-clone ()
  (interactive)
  (let ((wc (current-window-configuration)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab (format "*scratch-%d*" my/scratch-tab-number))
    (cl-incf my/scratch-tab-number)
    (set-window-configuration wc)))

(defun tab-bar-jump ()
  (interactive)
  (let ((char (read-char "[e]macs-devel [s]cratch [o]rg")))
    (switch-or-create-tab
     (pcase char
       (?o "org")
       (?s "scratch")
       (?e "emacs-devel")))))

(define-prefix-command '*tab-map*)

(define-key *root-map* (kbd "b") #'switch-or-create-tab)
(define-key *root-map* (kbd "B") #'my/show-tab-bar)
(define-key *root-map* (kbd "R") #'tab-bar-rename-tab)
(define-key *root-map* (kbd "q") #'close-tab-switch)
(define-key *root-map* (kbd "T") #'tab-bar-report)
(define-key *root-map* (kbd "t") '*tab-map*)

(define-key *tab-map* (kbd "h") (lambda () (interactive) (tab-bar-move-tab -1)))
(define-key *tab-map* (kbd "l") (lambda () (interactive) (tab-bar-move-tab 1)))

(define-key *tab-map* (kbd "c") #'my/tab-bar-clone)
(define-key *tab-map* (kbd "n") #'tab-bar-switch-to-next-tab)
(define-key *tab-map* (kbd "s") #'my/tab-bar-swap-tabs)
(define-key *tab-map* (kbd "p") #'tab-bar-switch-to-prev-tab)
(define-key *tab-map* (kbd "t") #'last-tab)
(define-key *tab-map* (kbd "j") #'tab-bar-jump)

(defvar tab-switch-mode-map nil)

(unless tab-switch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-1") #'(lambda () (interactive) (switch-or-create-tab "1")))
    (define-key map (kbd "q") #'(lambda () (interactive) (tab-switch-mode -1)))
    (setq tab-switch-mode-map map)))

(define-minor-mode tab-switch-mode ""
  :keymap tab-switch-mode-map
  :global t)

(defun init-tab-name (&optional frame)
  (interactive)
  (with-selected-frame frame
    (let* ((tab (assq 'current-tab (frame-parameter frame 'tabs)))
           (tab-explicit-name (alist-get 'explicit-name tab)))
      (unless (or tab-explicit-name
                  (cdr (assoc 'unsplittable
                              (frame-parameters nil))))
        (when my-ec/is-wsl
          (set-frame-size frame 80 30))
        (tab-bar-rename-tab "scratch")
        (tab-bar-new-tab -1)
        (tab-bar-rename-tab "emacs-devel")
        (tab-bar-new-tab)
        (tab-bar-rename-tab "org")
        (switch-or-create-tab "scratch")))))

(add-hook 'emacs-startup-hook
          #'(lambda () (init-tab-name (selected-frame))))

(add-hook 'after-make-frame-functions
          #'init-tab-name)

(provide 'switch-tabs)
;;; switch-tabs.el ends here
