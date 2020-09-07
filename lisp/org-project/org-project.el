;;; org-project.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-18 16:48]

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

(require 'org)
(require 'org-loop)
(require 'pcase)
;; (require 'org-delay)
(require 'org-project-legacy)

;; Import init function, opr/strict-projects, opr/ambiguous, and opr/strict-task
(require 'opr-util)

(opr/init)

;; TASKS
(require 'opr-tasks)


;; Projects
(require 'opr-projects)


;; Ambiguous

(define-todo-keyword "TODO" 'ambiguous :key ?T)

(define-todo-keyword "ONE" 'ambiguous :key ?o)

(finish-active-type 'ambiguous)

(defun opr/ambiguous-task-or-project ()
  (when-let ((state (org-get-todo-state)))
    (pcase state
      ("TODO" (if (olc/any-todo-children?
                    (or (member (opr/type-of-task) '(done stuck active))
                        (opr/type-of-project)))
                  'project
                'task))
      ("ONE" (if (olc/any-todo-children?
                   (or (member (opr/type-of-task) '(stuck active))
                       (opr/type-of-project)))
                 'project
               'task)))))

;; Util

;; Depended by olc/any-todo-children?  
(define-todo-keyword "CAT" 'util :key ?C)

(finish-active-type 'util)

;; Legacy keywords

(define-todo-keyword "CLOCK" 'legacy)

(define-todo-keyword "INACT" 'legacy)

(define-todo-keyword "BACKLOG" 'legacy :key ?b)

(finish-active-type 'legacy)

(define-todo-keyword "ABANDON" 'legacy :recordstr "@/!")

(defun opr/get-type ()
  (let* ((state (org-get-todo-state))
         (type (cond ((member state opr/strict-projects)
                      'project)
                     ((member state opr/strict-tasks)
                      'task)
                     ((member state opr/ambiguous)
                      (opr/ambiguous-task-or-project))
                     (t 'legacy))))
    (if (eq 'legacy type)
        'task
      type)))

(defun opr/get-type-and-state ()
  (let* ((state (org-get-todo-state))
         (type (cond ((member state opr/strict-projects)
                      'project)
                     ((member state opr/strict-tasks)
                      'task)
                     ((member state opr/ambiguous)
                      (opr/ambiguous-task-or-project))
                     (t 'legacy))))
    (cons (if (eq 'legacy type)
              'task
            type)
          (pcase type
            ('project (opr/type-of-project))
            ('task (opr/type-of-task))
            ('legacy 'invis)))))

(defun opr/print-type-and-state ()
  (interactive)
  (let ((ts (opr/get-type-and-state)))
    (message "Headline has todo keyword \"%s\" and is \"%s\""
             (car ts)
             (cdr ts))))

(provide 'org-project)
;;; org-project.el ends here
