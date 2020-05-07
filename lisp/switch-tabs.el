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

(defvar show-tab-bar-new-tab nil)

(defun switch-or-create-tab (tab-name)
  (interactive
   (list (let ((current-tab (alist-get 'name (tab-bar--current-tab))))
           (->> (tab-bar--tabs-recent)
                (mapcar #'(lambda (tab)
                            (alist-get 'name tab)))
                (remove-if #'(lambda (tab-name)
                               (string= tab-name current-tab)))
                (ido-completing-read (format "Switch to tab (%s): "
                                             current-tab))))))
  (let ((tab-index (tab-bar--tab-index-by-name tab-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (when (not show-tab-bar-new-tab)
        (tab-bar-mode -1))
      (tab-bar-rename-tab tab-name))))

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
      (tab-bar-close-tab)
      (when (<= 2 (length (funcall tab-bar-tabs-function)))
        (switch-or-create-tab "scratch"))
      t)))

(defun tab-bar-report ()
  (interactive)
  (message
   (concatenate 'string "Current Tabs: "
                (mapconcat #'(lambda (tab)
                               (alist-get 'name tab))
                           (funcall tab-bar-tabs-function)
                           ", "))))

(define-prefix-command '*tab-map*)

(define-key *root-map* (kbd "b") #'switch-or-create-tab)
(define-key *root-map* (kbd "R") #'tab-bar-rename-tab)
(define-key *root-map* (kbd "q") #'close-tab-switch)
(define-key *root-map* (kbd "T") #'tab-bar-report)
(define-key *root-map* (kbd "t") '*tab-map*)

(define-key *tab-map* (kbd "h") (lambda () (interactive) (tab-bar-move-tab -1)))
(define-key *tab-map* (kbd "l") (lambda () (interactive) (tab-bar-move-tab 1)))

(define-key *tab-map* (kbd "n") #'tab-bar-switch-to-next-tab)
(define-key *tab-map* (kbd "p") #'tab-bar-switch-to-prev-tab)
(define-key *tab-map* (kbd "t") #'last-tab)

(tab-bar-rename-tab "scratch1")

(provide 'switch-tabs)
;;; switch-tabs.el ends here
