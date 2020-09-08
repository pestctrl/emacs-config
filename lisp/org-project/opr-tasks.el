;;; opr-tasks.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-18 19:40]

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

(define-todo-keyword "STUFF" 'task :key ?S)

(define-todo-keyword "FUTURE" 'task :key ?f)

(define-todo-keyword "TICKLER" 'task :key ?\))

(define-todo-keyword "TASK" 'task :key ?t)

(define-todo-keyword "NEXT" 'task :key ?n)

(define-todo-keyword "WAIT" 'task :key ?w :recordstr "!")

(finish-active-type 'task)

(define-todo-keyword "DONE" 'task :key ?d :recordstr "!")

(add-to-list 'org-todo-keyword-faces
             '("DONE" . org-done))

(defun opr/is-task ()
  (let ((state (org-get-todo-state)))
    (or (member state opr/strict-tasks)
        (and (member state opr/ambiguous)
             (opr/ambiguous-project-or-task)))))

(defun opr/type-of-task ()
  (when-let ((state (org-get-todo-state)))
    (pcase state
      ("STUFF" 'invis)
      ("FUTURE" 'invis)
      ("TICKLER" 'invis)
      ("NEXT" 'active)
      ("DONE" 'done)
      ("COMPLETE" 'done)
      ("WAIT" (if (when-let (d (org-entry-get (point) "SCHEDULED"))
                    (org-time> d (org-matcher-time "<yesterday>")))
                  'wait-active
                'wait))
      (_ (when (or (member state opr/strict-tasks)
                   (and (member state opr/ambiguous)
                        (eq 'task
                            (opr/ambiguous-task-or-project))))
           (if (or (member "_invis_" (org-get-tags))
                   (when-let (d (org-entry-get (point) "DELAYED"))
                     (org-time> d (org-matcher-time "<now>"))))
               'invis
             (pcase state
               ("TASK" (if (or (org-get-deadline-time (point))
                               (org-get-scheduled-time (point)))
                           'active
                         'stuck))
               ("ONE" (when (eq 'task (opr/ambiguous-task-or-project))
                        (if (or (org-get-deadline-time (point))
                                (org-get-scheduled-time (point)))
                            'active
                          'stuck)))
               ("TODO" (when (eq 'task (opr/ambiguous-task-or-project))
                         (if (or (org-get-deadline-time (point))
                                 (org-get-scheduled-time (point)))
                             'active
                           'stuck))))))))))

(provide 'opr-tasks)
;;; opr-tasks.el ends here
