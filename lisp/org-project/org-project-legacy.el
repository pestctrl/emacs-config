;;; org-project-legacy.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-18 16:54]

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

;;; Commentary: I can't believe I'm calling this stuff legacy. It
;;; feels like I just re-wrote this stuff yesterday. In reality it's
;;; been like half a year I think, and I remember thinking I did a
;;; good job. And I still think it's a good job. The code is clean and
;;; simple, and it takes advantage of the org-loop I had written at
;;; the time. But, I believe it's time to go, old friend.

;;; Code:
;; Keywords
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
      '((sequence "STUFF(s)" "FUTURE(f)" "INACT(i)" "CLOCK(C)" "DEPEND(D)" "|")
        (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "CAT(>)" "ONE(o)" "META(m)" "META1(M)" "SEQ(S)" "EMPTY(e)" "ETERNAL(E)" "SPEC(:)" "|" "COMPLETE(c!)")
        (sequence "WAIT(w@/!)" "HOLD(h)" "TICKLER(T)" "|" "ABANDON(a@/!)")
        (sequence "TTTT" "|")))

(setq org-todo-keyword-faces 
      '(("ONE" :foreground "royal blue" :weight bold)
        ("STUFF" :foreground "goldenrod" :weight bold)
        ("NEXT" :foreground "cyan" :weight bold)
        ("WAIT" :foreground "yellow" :weight bold)
        ("HOLD" :foreground "red" :weight bold)
        ("META" :foreground "white" :weight bold)
        ("META1" :foreground "white" :weight bold)
        ("SEQ" :foreground "white" :weight bold)
        ("EMPTY" :foreground "white" :weight bold)
        ("ABANDON" :foreground "dark gray" :weight bold)
        ("CLOCK" :foreground "dark gray" :weight bold)
        ("TOP" :foreground "royal blue" :weight bold)
        ("INACT" :foreground "dark gray" :weight bold)
        ("FUTURE" :foreground "medium spring green" :weight bold)))

;; (setq org-todo-state-tags-triggers
;;         (quote (("HOLD" ("HOLD" . t))
;;                 ("WAIT" ("WAITING" . t))
;;                 (todo ("HOLD") ("WAITING")))))

;; New stuff
(defun my/is-todo-task ()
  (and (not (member "_invis_" (org-get-tags)))
       (pcase (org-get-todo-state)
         ("TODO" (my/no-children))
         ("ONE"  (my/no-todo-children))
         ("NEXT" t))))

;; Standalone tasks
(defun my/is-part-of-subtree ()
  (save-excursion
    (and (not (= 1 (org-current-level)))
         (let (has-parent-project)
           (while (and (not has-parent-project)
                       (org-up-heading-safe))
             (when (org-get-todo-state)
               (setq has-parent-project t)))
           has-parent-project))))

(defun my/is-standalone-task ()
  (and (my/is-todo-task)
       (not (my/is-part-of-subtree))))

;; Task predicates
(defun my/no-children ()
  "Check if there are NO tasks that are TODO or DONE"
  (not (ol/any-children? (org-get-todo-state))))

(defun my/has-children ()
  "Check if there are tasks that are TODO or DONE"
  (save-excursion
    (ol/any-children? (org-get-todo-state))))

(defun my/has-todo-child ()
  "Check if there are any tasks that are TODO"
  (save-excursion
    (ol/any-children? (my/is-todo-task))))

(defun my/no-todo-children ()
  "Check if there are NO tasks that are TODO"
  (save-excursion
    (not (ol/any-children? (my/is-todo-task)))))

(defun my/has-non-active-todo-child ()
  "Check if there are any tasks that are TODO"
  (ol/any-todo-children?
    (and (my/is-todo-task)
         (not (org-get-scheduled-time (point))))))

;; Project Stuff
(defconst my/project-keywords '("PROJECT" "META" "META1" "SEQ" "EMPTY" "ETERNAL" "SPEC" "HOLD"))
(defconst my/active-projects-and-tasks '("PROJECT" "META" "META1" "SEQ" "EMPTY" "ONE" "TODO"))

(defun my/is-a-project ()
  (save-excursion
    (let ((todo (org-get-todo-state)))
      (when todo
        (or (member todo my/project-keywords)
            (and (equal todo "ONE")
                 (my/has-todo-child))
            (and (member todo '("TODO"))
                 (my/has-children)))))))

;; Legacy stuff
(defun my/is-unactionable-task ()
  (or (member "NOT_TASKS" (org-get-tags (point)))
      (member (org-get-todo-state) (cons "INACT" org-done-keywords))
      (string= "FUTURE" (org-get-todo-state))))

(defun my/is-non-done-task ()
  (and (not (my/is-unactionable-task))
       (not (member (org-get-todo-state)
                    org-done-keywords))))

(defun my/has-non-done-task ()
  (ol/any-todo-children? (my/is-non-done-task)))

(defun my/is-a-task ()
  (save-excursion
    (and (not (member "NOT_TASKS" (org-get-tags (point))))
         (or (and (equal "ONE" (org-get-todo-state))
                  (not (my/has-non-done-task)))
             (and (org-get-todo-state)
                  (not (member (org-get-todo-state) '("PROJECT" "SOMEDAY" "WAIT" "HOLD")))
                  (my/no-children))))))

(defun my/has-next-task ()
  (ol/any-todo-children? (my/is-next-task)))

(defun my/is-next-task ()
  (let ((todo (org-get-todo-state)))
    (or (equal todo "NEXT")
        (and (member todo '("TODO" "ONE" "NEXT"))
             (or (org-get-scheduled-time (point))
                 (org-get-deadline-time (point)))))))

;; Project code
(defun my/active-sequential-project (file point)
  (save-excursion
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          has-next-task has-active-project)
      (outline-next-heading)
      (while (and (not (or has-next-task
                           has-active-project))
                  (< (point) subtree-end))
        (cond ((and (my/is-a-task)
                    (my/is-next-task))
               (setq has-next-task t))
              ((and (my/is-a-project)
                    (eq (my/get-project-type file (point) t)
                        'active))
               (setq has-active-project t)))
        (org-end-of-subtree t t))
      (or has-next-task
          has-active-project))))

(defun my/greedy-active-project (file point)
  (save-excursion
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          has-next-task has-active-project)
      (outline-next-heading)
      (while (and (not (and has-next-task
                            has-active-project))
                  (< (point) subtree-end))
        (while (string= "CAT" (org-get-todo-state))
          (outline-next-heading))
        (cond ((or (and (my/is-a-task)
                        (my/is-next-task))
                   (string= "WAIT" (org-get-todo-state)))
               (setq has-next-task t))
              ((and (my/is-a-project)
                    (eq (my/get-project-type file (point) nil)
                        'active))
               (setq has-active-project t)))
        (org-end-of-subtree t t))
      (or has-next-task
          has-active-project))))

(defun my/generous-active-project (file point)
  (save-excursion
    (let (has-task has-next-task has-project has-stuck-project)
      (orgc-loop/todo-children-cat custom-condition
        (if (and has-next-task has-stuck-project)
            (setq custom-condition t)
          (cond ((my/is-a-project)
                 (setq has-project t)
                 (when (eq (my/get-project-type file (point) t)
                           'stuck)
                   (setq has-stuck-project t)))
                ((my/is-non-done-task)
                 (setq has-task t)
                 (when (or (my/is-next-task)
                           (equal (org-get-todo-state) "WAIT")) ;; Ew
                   (setq has-next-task t))))))
      (or (and has-next-task 
               (not has-stuck-project))
          (and (not has-task) 
               has-project
               (not has-stuck-project))))))

;; New project show
(defun my/stuck-empty ()
  (my/has-non-active-todo-child))

(defun my/stuck-meta (ambiguous-to-stuck)
  (let ((file (buffer-file-name))
        (point (point)))
    (not (if ambiguous-to-stuck
             (my/generous-active-project file point)
           (my/greedy-active-project file point)))))

(defun my/active-seq (file point)
  (my/active-sequential-project file point))

(defun my/stuck-one ()
  (and (my/has-todo-child)
       (my/greedy-active-project (buffer-file-name) (point))))

;;(defun my/active-act)

(defun my/get-project-type (file point &optional ambiguous-to-stuck)
  (save-excursion
    (when (my/is-a-project)
      (let ((todo (org-get-todo-state)))
        (if (and (org-time> (org-entry-get (point) "SCHEDULED")
                            (org-matcher-time "<now>"))
                 (or (member todo '("META" "META1" "EMPTY" "SEQ"))
                     (member todo '("ONE" "TODO"))))
            'delayed
          (pcase todo
            ("ETERNAL" 'eternal)
            ("FUTURE" 'someday)
            ("HOLD" 'hold)
            ("SEQ"
             (if (my/active-seq file point)
                 'active 'stuck))
            ("EMPTY"
             (when (my/stuck-empty)
               'stuck))
            ("META"
             (if (my/stuck-meta ambiguous-to-stuck)
                 'stuck 'active))
            ("META1"
             (if (my/greedy-active-project (buffer-file-name) (point))
                 'active 'stuck))
            ("TODO"
             (if (my/stuck-meta ambiguous-to-stuck)
                 'stuck 'active))
            ("ONE"
             (when (my/has-todo-child)
               (if (my/greedy-active-project (buffer-file-name) (point))
                   'active 'stuck)))))))))

(defun my/show-active-projects ()
  "Only show subtrees that are stuck projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (unless (member (my/get-project-type buffer-file-name (point) nil)
                      '(active))
        subtree-end))))

(defun my/show-stuck-projects ()
  "Only show subtrees that are stuck projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t)))
          (next-heading (save-excursion (outline-next-heading))))
      ;; (setq debug-p (point)
      ;;       debuf-f (buffer-file-name))
      (if (org-get-todo-state)
          (unless (or (and (my/is-a-task)
                           (my/is-standalone-task)
                           (not (org-get-scheduled-time (point)))
                           (not (org-get-deadline-time (point))))
                      (eq (my/get-project-type buffer-file-name (point) t)
                          'stuck))
            subtree-end)
        next-heading))))

(defun my/show-delayed-projects ()
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (unless (eq (my/get-project-type buffer-file-name (point))
                  'delayed)
        subtree-end))))

(defun my/agenda-custom-skip ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current (point))
        display)
    (save-restriction
      (widen)
      (save-excursion
        (when (or (my/is-a-project)
                  (not (member "PLAN" (org-get-tags)))
                  (member (org-get-todo-state) '("FUTURE" "WAIT" "HABIT" nil)))
          next-headline)))))

(defun my/show-next-tasks-and-standalone-tasks ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (unless (and (my/is-a-task)
                 (or 
                  (my/is-next-task)
                  (my/is-standalone-task)))
      next-headline)))


(defun my/has-next-todo ()
  (save-excursion
    (let ((end-of-subtree (save-excursion (org-end-of-subtree t)))
          flag)
      (while (and (not flag)
                  (outline-next-heading)
                  (< (point) next-headline))
        (when (string= (org-get-todo-state) "NEXT")
          (setq flag (point))))
      flag)))

(defun my/show-leaf-tasks ()
  (let ((next-headline (save-excursion (org-end-of-subtree t))))
    (unless (or (string= "NEXT" (org-get-todo-state))
                (my/has-next-todo))
      next-headline)))

(defun my/parent-is-eternal ()
  (save-excursion
    (and (not (= 1 (org-current-level)))
         (progn
           (org-up-heading-safe)
           (string= (org-get-todo-state) "ETERNAL")))))

(defun my/show-top-level ()
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (or (not (my/is-part-of-subtree))
                  (my/parent-is-eternal))
        next-headline))))

(provide 'org-project-legacy)
;;; org-project-legacy.el ends here
