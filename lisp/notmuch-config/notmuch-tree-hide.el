;;; notmuch-tree-hide.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-20 22:31]

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

(defvar notmuch-tree-show-filter-function nil)
(defvar notmuch-tree-filter-debug nil)

(defun notmuch-tree-alive-match-p (msg)
  (plist-get msg :match))

(defvar notmuch-tree-alive-function #'notmuch-tree-alive-match-p)

(defun notmuch-tree-hide-dead-trees (forest-thread)
  (let ((new-forest '()))
    (cl-loop for tree in forest-thread
             while (car tree)
             do (let ((subtrees (notmuch-tree-hide-dead-trees (cadr tree)))
                      (first (car tree)))
                  (if subtrees
                      (push (cons first (list subtrees)) new-forest)
                    (when (or (member "flagged" (plist-get first :tags))
                              (funcall notmuch-tree-alive-function first))
                      (push (list first nil) new-forest)))))
    (reverse new-forest)))

(defun notmuch-tree-show-trail-and-children (forest-thread)
  (let ((new-forest '()))
    (cl-loop for tree in forest-thread
             while (car tree)
             do (let ((first (car tree)))
                  (if (plist-get first :match)
                      (push tree new-forest)
                    (when-let (subtrees (notmuch-tree-show-trail-and-children (cadr tree)))
                      (push (cons first (list subtrees)) new-forest)))))
    (reverse new-forest)))

(defun notmuch-tree-show-trail-and-alive-children (forest-thread)
  (let ((new-forest '()))
    (cl-loop for tree in forest-thread
             while (car tree)
             do (let ((first (car tree)))
                  (if (plist-get first :match)
                      (let ((notmuch-tree-alive-function #'(lambda (msg) (member "unread" (plist-get msg :tags)))))
                        (push (list (car tree) (notmuch-tree-hide-dead-trees (cadr tree))) new-forest))
                    (when-let (subtrees (notmuch-tree-show-trail-and-alive-children (cadr tree)))
                      (push (cons first (list subtrees)) new-forest)))))
    (reverse new-forest)))

;; (notmuch-tree-show-trail-and-alive-children notmuch-tree-filter-debug)

(defun notmuch-tree-insert-hide (orig forest-thread)
  (let ((forest-thread
         (if notmuch-tree-show-filter-function
             (progn (setq notmuch-tree-filter-debug forest-thread) (funcall notmuch-tree-show-filter-function forest-thread))
           forest-thread)))
    (setq notmuch-tree-show-filter-function nil)
    (funcall orig forest-thread)))

(advice-add #'notmuch-tree-insert-forest-thread
            :around
            #'notmuch-tree-insert-hide)

;; (advice-remove #'notmuch-tree-insert-forest-thread #'notmuch-tree-insert-hide)

(provide 'notmuch-tree-hide)
;;; notmuch-tree-hide.el ends here
