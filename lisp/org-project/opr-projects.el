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

(defvar opr/meta-active-if-one-active nil)

;; Context sensitive
(define-todo-keyword "META" 'project :key ?m)

(defun meta-status? (&optional active-if-one-active)
  (if (or opr/meta-active-if-one-active active-if-one-active)
      (if (or (olc/any-todo-children? 
                (eq 'active (opr/type-of-task)))
              (olc/any-todo-children?
                (eq 'active (opr/type-of-project))))
          'active
        'stuck)
    (if (or
         (olc/any-todo-children?
           (or
            ;; Any stuck task? Stuck!
            (eq 'stuck (opr/type-of-task))
            ;; Have a stuck project? Stuck! 
            (eq 'stuck (opr/type-of-project))))
         ;; No todo children? Stuck!
         (not (olc/any-todo-children? t))
         ;; All todo children are done? Stuck!
         (not (olc/any-todo-children?
                (not (member (cdr (opr/get-type-and-state))
                             '(done wait invis))))))
        'stuck
      'active)))

(defun seq-status? ()
  (if (olc/any-todo-children?
        (or (member (opr/type-of-task) '(active))
            (eq 'active (opr/type-of-project))))
      'active
    (if (olc/any-todo-children?
          (or (eq 'wait (opr/type-of-task))
              (eq 'invis (opr/type-of-project))))
      'invis
      'stuck)))

(define-todo-keyword "SEQ" 'project :key ?s)

(define-todo-keyword "EMPTY" 'project :key ?e)

(defun empty-status? ()
  (if (olc/any-todo-children?
        (or (eq 'stuck (opr/type-of-task))
            (eq 'stuck (opr/type-of-project))))
      'stuck
    'invis))

;; Simple ones
(define-todo-keyword "ETERNAL" 'project :key ?E)

(define-todo-keyword "HOLD" 'project :key ?h)

(finish-active-type 'project)

(defun opr/is-project ()
  (let ((state (org-get-todo-state)))
    (or (member state opr/strict-projects)
        (and (member state opr/ambiguous)
             (opr/ambiguous-task-or-project)))))

(defun opr/type-of-project (&optional greedy-active)
  (when-let ((state (org-get-todo-state)))
    (pcase state
      ("HOLD" 'hold)
      ("ETERNAL" 'active)
      (_
       (let ((tags (org-get-tags)))
         (when (or (member state opr/strict-projects)
                   (and (member state opr/ambiguous)
                        (eq 'project (opr/ambiguous-task-or-project))))
           (cond ((member "_invis_" tags)
                  'invis)
                 ((when-let (d (org-entry-get (point) "DELAYED"))
                    (org-time> d (org-matcher-time "<now>")))
                  'invis)
                 ((when-let (d (org-entry-get (point) "NOT_TODAY"))
                    (org-time> d (org-matcher-time "<yesterday>")))
                  'invis)
                 ((when-let (s (org-entry-get (point) "SCHEDULED"))
                    (org-time> s (org-matcher-time "<now>")))
                  'active)
                 (t
                  (let ((status (pcase state
                                  ("EMPTY" (empty-status?))
                                  ("SEQ" (seq-status?))
                                  ("META" (meta-status? greedy-active))
                                  ("ONE" (when (eq 'project (opr/ambiguous-task-or-project))
                                           (seq-status?)))
                                  ("TODO" (when (eq 'project (opr/ambiguous-task-or-project))
                                            (seq-status?))))))
                    (cond ((and (member "_invis_when_empty_" tags)
                                (not (eq status 'active))
                                (not (olc/any-todo-children?
                                       (eq 'stuck (cdr (opr/get-type-and-state))))))
                           'invis)
                          ((and (member "invis_when_active" (org-get-tags))
                                (eq status 'active))
                           'invis)
                          (t status)))))))))))

(provide 'opr-projects)
;;; opr-projects.el ends here
