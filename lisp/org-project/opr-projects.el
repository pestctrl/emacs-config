;;; opr-projects.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-18 19:44]

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

(require 'opr-util)

;; Context sensitive
(define-todo-keyword "META" 'project :color "white" :key ?m)

(defun meta-status? (&optional greedy-active)
  (if greedy-active
      (if (or (olc/any-todo-children? 
                (eq 'active (opr/type-of-task)))
              (olc/any-todo-children?
                (eq 'active (opr/type-of-project))))
          'active
        'stuck)
    (if (or
         ;; No active tasks? Stuck if no other projects!
         (and (not (olc/any-todo-children?
                     (eq 'active (opr/type-of-task))))
              (not
               (olc/any-todo-children?
                 (eq 'project (opr/get-type)))))
         ;; Have a stuck project? Stuck! 
         (and (olc/any-todo-children?
                (eq 'stuck (opr/type-of-project)))))
        'stuck
      'active)))

(define-todo-keyword "META1" 'project :color "white" :key ?1)

(defun meta1-status? ()
  (if (olc/any-todo-children?
        (or (eq 'active (opr/type-of-task))
            (eq 'active (opr/type-of-project))))
      'active
    'stuck))

(define-todo-keyword "SEQ" 'project :color "white" :key ?S)

(defalias 'seq-status? #'meta1-status?)

(define-todo-keyword "EMPTY" 'project :color "white" :key ?e)

(defun empty-status? ()
  (if (olc/any-todo-children?
        (eq 'stuck (opr/type-of-task)))
      'stuck
    'invis))

;; Simple ones
(define-todo-keyword "ETERNAL" 'project :key ?E)

(define-todo-keyword "HOLD" 'project :color "red" :key ?h)

(finish-active-type 'project)

(defun opr/is-project ()
  (let ((state (org-get-todo-state)))
    (or (member state opr/strict-projects)
        (and (member state opr/ambiguous)
             (opr/ambiguous-project-or-task)))))

(defun opr/type-of-project (&optional greedy-active)
  (when-let ((state (org-get-todo-state)))
    (pcase state
      ("HOLD" 'hold)
      ("ETERNAL" 'active)
      (_ (when (or (member state opr/strict-projects)
                   (and (member state opr/ambiguous)
                        (eq 'project (opr/ambiguous-task-or-project))))
           (if (or (member "_invis_" (org-get-tags))
                   (when-let (d (org-entry-get (point) "DELAYED"))
                     (org-time> d (org-matcher-time "<now>"))))
               'invis
             (if (when-let (s (org-entry-get (point) "SCHEDULED"))
                   (org-time> s (org-matcher-time "<now>")))
                 'active
               (pcase state
                 ("EMPTY" (empty-status?))
                 ("SEQ" (seq-status?))
                 ("META1" (meta1-status?))
                 ("META" (meta-status? greedy-active))
                 ("ONE" (when (eq 'project (opr/ambiguous-task-or-project))
                          (meta1-status?)))
                 ("TODO" (when (eq 'project (opr/ambiguous-task-or-project))
                           (meta1-status?)))))))))))

(provide 'opr-projects)
;;; opr-projects.el ends here
