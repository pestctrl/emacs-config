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

(defun ol/get-eot-marker ()
  "Return a marker to the end of a subtree.
This is useful for looping over a subtree while
the contents of the subrtee are changing."
  (if (not (org-at-heading-p))
      (point-max-marker)
    (save-excursion
     (org-end-of-subtree t t)
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
    `(let ((,start (progn (org-back-to-heading) (point)))
           (,end-of-subtree (ol/get-eot-marker)))
       (while (and (if (= (point) ,start) (org-goto-first-child) (org-end-of-subtree t t))
                   (< (point) ,end-of-subtree))
         ,@body))))

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
    `(let ((,start (progn (org-back-to-heading) (point)))
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
(defun get-variables (l)
  (cond ((null l) nil)
        ((consp (car l))
         (append (extract-variables (car l)) 
                 (get-variables (cdr l))))
        (t (cons (car l)
                 (get-variables (cdr l))))))

;; (defun extract-variables (l)
;;   (if (not (consp l))
;;       l
;;     (get-variables (cdr l))))

;; (defmacro org-loop/descendants (&rest body)
;;   (let ((subtree-symbol (make-symbol "subtree-end")))
;;     `(let ((,subtree-symbol (save-excursion (org-end-of-subtree t))))
;;        (cl-loop for p = (point)
;;                 while (< p ,subtree-symbol)
;;                 do (progn ,@body)))))

;; (defun test ()
;;   (interactive)
;;   (org-loop/descendants
;;     (message (format "%s" (org-get-todo-state)))))

;; ;; Descendants
;; (defmacro org-loop/descendants (&rest body)
;;   (declare (indent defun))
;;   (let ((subtree-symbol (make-symbol "subtree-end")))
;;     `(let ((,subtree-symbol (save-excursion (org-end-of-subtree t))))
;;        (while (and (outline-next-heading)
;;                    (< (point) ,subtree-symbol))
;;          ,@body))))

;; (defmacro org-loop!/descendants (&rest body)
;;   "This version of org loop will account for if the tree has been editted while looping"
;;   (declare (indent defun))
;;   ;; (let ((subtree-symbol (make-symbol "subtree-end")))
;;   ;;   `(let ((,subtree-symbol (save-excursion (org-end-of-subtree t))))
;;   ;;      (while (and (outline-next-heading)
;;   ;;                  (< (point) ,subtree-symbol))
;;   ;;        ,@body)))
;;   )

;; (defmacro orgc-loop/descendants (condition &rest body)
;;   (declare (indent defun))
;;   (let ((subtree-symbol (make-symbol "subtree-end")))
;;     `(let ((,subtree-symbol (save-excursion (org-end-of-subtree t)))
;;            (,condition nil))
;;        (while (and (not ,condition)
;;                    (outline-next-heading)
;;                    (< (point) ,subtree-symbol))
;;          ,@body)
;;        ,condition)))

;; (defmacro orgb-loop/descendants (condition &rest body)
;;   (declare (indent defun))
;;   (let ((subtree-symbol (make-symbol "subtree-end"))
;;         (vars (extract-variables condition)))
;;     `(let ((,subtree-symbol (save-excursion (org-end-of-subtree t)))
;;            ,@vars)
;;        (while (and ,condition
;;                    (outline-next-heading)
;;                    (< (point) ,subtree-symbol))
;;          ,@body)
;;        ,condition)))

;; (defmacro orgc-loop/todo-descendants (condition &rest body)
;;   (declare (indent defun))
;;   (let ((todo-state (make-symbol "todo-state"))
;;         (tags (make-symbol "tags")))
;;     `(orgc-loop/descendants ,condition
;;        (let ((,todo-state (org-get-todo-state))
;;              (,tags (org-get-tags (point))))
;;          (when ,todo-state
;;            (if (member "ARCHIVE" ,tags)
;;                (org-end-of-subtree t)
;;              ,@body))))))

;; (defmacro org-loop/todo-children (&rest body)
;;   (declare (indent defun))
;;   (let ((todo-state (make-symbol "todo-state"))
;;         (tags (make-symbol "tags")))
;;     `(org-loop/children 
;;        (let ((,todo-state (org-get-todo-state))
;;              (,tags (org-get-tags (point))))
;;          (when ,todo-state
;;            (if (member "ARCHIVE" ,tags)
;;                (org-end-of-subtree t)
;;              ,@body))))))

;; (defmacro org-loop/children (&rest body)
;;   (declare (indent defun))
;;   (let ((level-symbol (make-symbol "level")))
;;     `(progn
;;        (let ((,level-symbol (org-current-level)))
;;          (outline-next-heading)
;;          (when (< ,level-symbol (org-current-level))
;;            (while (progn 
;;                     ,@body
;;                     (outline-get-next-sibling))))))))

;; (defmacro orgc-loop/children (condition &rest body)
;;   (declare (indent defun))
;;   (let ((level-symbol (make-symbol "level")))
;;     `(let ((,condition nil)
;;            (,level-symbol (org-current-level)))
;;        (outline-next-heading)
;;        (when (< ,level-symbol (org-current-level))
;;          (while (progn
;;                   ,@body
;;                   (and (not ,condition)
;;                        (org-get-next-sibling))))
;;          ,condition))))

;; (defmacro orgc-loop/todo-children (condition &rest body)
;;   (declare (indent defun))
;;   (let ((todo-state (make-symbol "todo-state"))
;;         (tags (make-symbol "tags")))
;;     `(orgc-loop/children ,condition
;;        (let ((,todo-state (org-get-todo-state))
;;              (,tags (org-get-tags (point))))
;;          (when ,todo-state
;;            (if (member "ARCHIVE" ,tags)
;;                (org-end-of-subtree t)
;;              ,@body))))))

;; (defmacro orgc-loop/children-cat (condition &rest body)
;;   (declare (indent defun))
;;   (let ((level-symbol (make-symbol "level")))
;;     `(let ((,condition nil)
;;            (,level-symbol (org-current-level)))
;;        (outline-next-heading)
;;        (when (< ,level-symbol (org-current-level))
;;          (while (progn
;;                   (while (string= (org-get-todo-state) "CAT")
;;                     (outline-next-heading))
;;                   ,@body
;;                   (and (not ,condition)
;;                        (or (org-get-next-sibling)
;;                            (and (not (eobp))
;;                                 (< ,level-symbol (org-current-level)))))))
;;          ,condition))))

;; (defmacro orgc-loop/todo-children-cat (condition &rest body)
;;   (declare (indent defun))
;;   (let ((todo-state (make-symbol "todo-state"))
;;         (tags (make-symbol "tags")))
;;     `(orgc-loop/children-cat ,condition
;;        (let ((,todo-state (org-get-todo-state))
;;              (,tags (org-get-tags (point))))
;;          (when ,todo-state
;;            (if (member "ARCHIVE" ,tags)
;;                (org-end-of-subtree t)
;;              ,@body))))))

;; ;; (defmacro orgb-loop/todo-children (condition &rest body)
;; ;;   (declare (indent defun))
;; ;;   (let ((todo-state (make-symbol "todo-state"))
;; ;;         (tags (make-symbol "tags")))
;; ;;     `(orgb-loop/children ,condition
;; ;;        (let ((,todo-state (org-get-todo-state))
;; ;;              (,tags (org-get-tags (point))))
;; ;;          (when ,todo-state
;; ;;            (if (member "ARCHIVE" ,tags)
;; ;;                (org-end-of-subtree t)
;; ;;              ,@body))))))

;; (defmacro org-loop/todo-children (condition &rest body)
;;   (declare (indent defun))
;;   (let ((todo-state (make-symbol "todo-state"))
;;         (tags (make-symbol "tags")))
;;     `(org-loop/children 
;;        (let ((,todo-state (org-get-todo-state))
;;              (,tags (org-get-tags (point))))
;;          (when (and ,todo-state
;;                     (not (member "ARCHIVE") ,tags))
;;            ,@body)))))


;; (defmacro traverse-org-headlines (headline &rest body)
;;   (declare (indent defun))
;;   (let ((buffer-symbol (make-symbol "buffer")))
;;     `(let (,buffer-symbol)
;;        (org-check-agenda-file ,(cadr headline))
;;        (setq ,buffer-symbol (if (file-exists-p ,(cadr headline))
;;                                 (org-get-agenda-file-buffer ,(cadr headline))
;;                               (error "No such file %s" ,(cadr headline))))
;;        (with-current-buffer ,buffer-symbol
;;          (while (and (not (eobp))
;;                      (outline-next-heading))
;;            ,@body)))))

;; (defmacro traverse-org-files (files &rest body)
;;   (declare (indent defun))
;;   (let ((file-symbol (make-symbol "file")))
;;     `(dolist (,file-symbol ,(cadr files))
;;        (traverse-org-headlines (,(car files) ,file-symbol)
;;          ,@body))))


(provide 'org-loop)
;;; org-loop.el ends here
