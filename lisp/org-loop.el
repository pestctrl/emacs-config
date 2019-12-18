;;; org-loop.el --- Some useful macros for looping in org mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: <2019-12-05 Thu>

;; This file is not part of GNU Emacs.

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

;; I write this so I never have to remember how to iterate over a subtree
;; or org file, and clumsily rewrite the while loop for the 100th time.

;;; Code:

(require 'cl)
(require 'org)
(require 'mmt)

(require 'org-loop-legacy)

(defun ol/get-eot-marker ()
  "Return a marker to the end of a subtree.
This is useful for looping over a subtree while
the contents of the subrtee are changing."
  (if (not (org-at-heading-p))
      (point-max-marker)
    (save-excursion
     (org-end-of-subtree t nil)
     (point-marker))))

(defun ol/get-bot-marker ()
  "Return a marker at the beginning of the subtree.
\"Beginning\", in this case, means the point at which
you can insert a heading. Usually, the beginning of the line
right below the subtree."
  (save-excursion
    (if (org-at-heading-p)
        (progn (outline-next-heading) (point-marker))
      (goto-char (point-min))
      (while (not (looking-at "*"))
        (next-line))
      (point-marker))))

(defmacro ol/descendants (&rest body)
  "Wraps `BODY' in a while loop that loops over all descendants of a subtree.
Iterate using `outline-next-heading'."
  (mmt-with-gensyms (end-of-subtree)
    `(let ((,end-of-subtree (ol/get-eot-marker)))
       (while (and (outline-next-heading)
                   (< (point) ,end-of-subtree))
         ,@body))))

(defmacro ol/children (&rest body)
  "Wraps `BODY' in a while loop that loops over direct children of a subree.
This means that children of children of the
subtree do not get a chance in the limelight.

Iterate using `org-end-of-subtree'.

Also handles case where point ends up back at the
beginning of the subtree, will check to make sure
it doesn't call `org-end-of-subtree' in that situation"
  (mmt-with-gensyms (end-of-subtree start)
    `(let ((,start (progn (org-back-to-heading t) (point)))
           (,end-of-subtree (ol/get-eot-marker)))
       (save-excursion
         (while (and (if (= (point) ,start) (org-goto-first-child) (org-end-of-subtree t t))
                     (< (point) ,end-of-subtree))
           ,@body)))))

(defmacro ol/todo-children (&rest body)
  "Wraps `BODY' in `ol/children', with the added criteria that children must have a todo-keyword."
  (mmt-with-gensyms (todo-state tags)
    `(ol/children
      (let ((,todo-state (org-get-todo-state))
            (,tags (org-get-tags)))
        (when ,todo-state
          (unless (member "ARCHIVE" ,tags)
            ,@body))))))

(defmacro ols/children (&rest body)
  "Wraps `BODY' in a while loop that loops over direct children of a subree.
Experimental version that takes the result of `BODY'
and uses that to determine if the loop should iterate
to the next position.  This is to allow a more simple
method to \"continue\", like in traditional while loops."
  (mmt-with-gensyms (end-of-subtree start result)
    `(let ((,start (progn (org-back-to-heading t) (point)))
           (,end-of-subtree (ol/get-eot-marker)))
       (org-goto-first-child)
       (while (< (point) ,end-of-subtree)
         (let ((,result (progn ,@body)))
           (unless ,result
             (org-get-next-sibling)))))))

(defmacro olsb/children (&rest body)
  "Wraps `BODY' in a while loop that loops over all direct children of a FILE.
Added experimental \"s\" feature, meaning that iteration will only occur if
result of `BODY' is nil"
  `(while (not (eobp))
     (unless (progn ,@body)
       (org-get-next-sibling))))

(defmacro ol/any-children? (condition)
  (declare (indent defun))
  (mmt-with-gensyms (end-of-subtree start)
    `(save-excursion
       (let ((,start (progn (org-back-to-heading t) (point)))
             (,end-of-subtree (ol/get-eot-marker)))
         (while (and (if (= (point) ,start) (org-goto-first-child) (org-end-of-subtree t t))
                     (< (point) ,end-of-subtree)
                     (not ,condition)))
         (and (< (point) ,end-of-subtree)
              (not (= (point) ,start))
              ,condition)))))

(defmacro ol/any-todo-children? (condition)
  (declare (indent defun))
  (mmt-with-gensyms (end-of-subtree start)
    `(save-excursion
       (let ((,start (progn (org-back-to-heading t) (point)))
             (,end-of-subtree (ol/get-eot-marker)))
         (while (and (if (= (point) ,start) (org-goto-first-child) (org-end-of-subtree t t))
                     (< (point) ,end-of-subtree)
                     (or (not (org-get-todo-state))
                         (member "ARCHIVE" (org-get-tags))
                         (not ,condition))))
         (and (< (point) ,end-of-subtree)
              (not (= (point) ,start))
              ,condition)))))

(defmacro ol/any-descendents? (condition)
  (declare (indent defun))
  (mmt-with-gensyms (end-of-subtree)
    `(save-excursion
       (let ((,end-of-subtree (ol/get-eot-marker)))
         (while (and (outline-next-heading)
                     (< (point) ,end-of-subtree)
                     (not ,condition)))
         (and (< (point) ,end-of-subtree)
              ,condition)))))

(provide 'org-loop)
;;; org-loop.el ends here
