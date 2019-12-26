;;; exwm-workspace-counsel.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2019-12-26 10:55]

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
;;; Workspace naming
(setq exwm-workspace-number 2)

(defvar my/workspace-list '((scratch0 . 0)
             (scratch1 . 1))
  "My custom workspace list")

(defun delete-frame-cleanup-workspace-monitor (orig &rest args)
  (unless exwm--floating-frame
    (let ((wnumber exwm-workspace-current-index))
      (apply orig args)
      (setq my/workspace-list
            (remove-if (lambda (cell) (= wnumber (cdr cell)))
                       my/workspace-list))
      (assq-delete-all wnumber my/monitor-list)
      (unless (= (1+ wnumber) (exwm-workspace--count))
        (setq my/workspace-list
              (mapcar (lambda (cell)
                        (if (< (cdr cell) wnumber)
                            cell
                          (cons (car cell) (1- (cdr cell)))))
                      my/workspace-list))
        (setq my/monitor-list
              (mapcar (lambda (cell)
                        (if (< (car cell) wnumber)
                            cell
                          (cons (1- (car cell)) (cdr cell))))
                      my/monitor-list))))))

(defun my/get-current-workspace ()
  (car (remove-if-not (lambda (cell)
            (= (cdr cell)
               exwm-workspace-current-index))
          my/workspace-list)))

(defun workspace-switch-record-last (&rest args)
  (setq my/last-workspace (car (my/get-current-workspace))))

(defun my/switch-workspace (name)
  (interactive (list (completing-read "Workspace name? "
                  (mapcar #'car my/workspace-list))))
  (let* ((key (intern name))
     (workspace-number (cdr (assoc key my/workspace-list))))
    (if workspace-number
    (exwm-workspace-switch workspace-number)
  (my/make-new-workspace name))))

(defun my/get-empty-workspace ()
  (exwm-workspace--count))

(defun my/make-new-workspace (name &optional no-switch)
  (let ((workspace-number (my/get-empty-workspace)))
    (add-to-list 'my/workspace-list
         `(,(intern name) . ,workspace-number))
    (add-to-list 'my/monitor-list
         `(,workspace-number . ,(car (my/get-screens))))
    (my/update-workspace-monitors)
    (exwm-workspace-switch-create workspace-number)
    (when no-switch
      (my/switch-to-last-workspace))
    workspace-number))

(defun my/rename-workspace (name)
  (interactive (list (intern (read-from-minibuffer "What name? "))))
  (cl-flet ((current-workspace-p (cell) (= (cdr cell) exwm-workspace-current-index)))
    (let* ((results (remove-if-not #'current-workspace-p
                   my/workspace-list))
       (res-count (length results)))
  (when (not (= 0 res-count))
    (setq my/workspace-list
      (remove-if #'current-workspace-p
             my/workspace-list)))
  (add-to-list 'my/workspace-list
           `(,name . ,exwm-workspace-current-index)))))

(defun my/move-window-to-workspace ()
  (interactive)
  (let* ((key (completing-read "Move to which workspace? "
                               (mapcar #'car my/workspace-list)))
         (workspace-number (or (cdr (assoc (intern key) my/workspace-list))
                               (my/make-new-workspace key t))))
    (exwm-workspace-move-window workspace-number)))

(defvar my/last-workspace nil)

(defun my/switch-to-last-workspace ()
  (interactive)
  (when-let (cell (assoc my/last-workspace my/workspace-list))
    (exwm-workspace-switch (cdr cell))
    (message (format "Switched to workspace: \"%S\"" (car cell)))))

(defun my/current-workspace ()
  (interactive)
  (let* ((results (remove-if-not (lambda (cell) (= (cdr cell) exwm-workspace-current-index)) my/workspace-list))
     (res-count (length results)))
    (if (= res-count 0)
    (message "No name for current workspace")
  (message (format "Current Workspace: #%d \"%s\" on %s"
           exwm-workspace-current-index
           (car (car results)))))))

(defun my/workspace-report ()
  (interactive)
  (loop for i in my/workspace-list
    do (message "Workspace #%d: \"%s\" on \"%s\""
            (cdr i) (car i)
            (cdr (assoc (cdr i) my/monitor-list)))))

;; Handle deleting if frame was destroyed

(define-key *root-map* (kbd "b") #'my/switch-workspace)
(define-key *root-map* (kbd "R") #'my/rename-workspace)
(define-key *root-map* (kbd "M") #'my/move-window-to-workspace)
(define-key *root-map* (kbd "P") #'my/current-workspace)

(add-to-list 'exwm-input-global-keys
             `(,(kbd "<s-tab>") . my/switch-to-last-workspace))
;; (define-key *window-map* (kbd "s") #'my/switch-workspace)

;;; Workspace positioning
(defvar my/monitor-list '((0 . "eDP1") 
                          (1 . "eDP1")))

(defun my/setup-workspace-monitors ()
  (let* ((monitors (my/get-screens))
     (primary (car monitors))
     (secondary (or (cadr monitors) primary)))
    (setq my/monitor-list
      `((0 . ,primary)
        (1 . ,secondary)))
    (my/update-workspace-monitors t)))

(defun flatten-list (list)
  (let ((f (car list)))
    (cond ((null f) '())
      ((listp f)
       (append (flatten-list f)
           (flatten-list (cdr list))))
      (t
       (cons f (flatten-list (cdr list)))))))

(defun my/update-workspace-monitors (&optional no-refresh)
  (let ((primary (car (my/get-screens))))
    (->> (loop for j in (cl-sort (copy-seq my/workspace-list) #'< :key 'cdr)
               collect (list (cdr j) (or (cdr (assoc (cdr j) my/monitor-list)) primary)))
         (flatten-list)
         (setq exwm-randr-workspace-monitor-plist)))
  (unless no-refresh
    (exwm-randr-refresh)))

(defun my/move-to-monitor (monitor)
  (interactive (list (let ((monitors (my/get-screens)))
           (if (= (length monitors) 2)
               (if (string= (car monitors)
                    (cdr (assoc exwm-workspace-current-index my/monitor-list)))
               (cadr monitors)
                 (car monitors))
             (completing-read "Which Monitor? "
                      monitors)))))
  (setf (alist-get exwm-workspace-current-index my/monitor-list)
    monitor)
  (my/update-workspace-monitors))

;; Switch monitor
(defvar monitor-active-list '())

(defun workspace-switch-record-monitor (&rest args)
  (setf (alist-get (intern (frame-parameter exwm-workspace--current 'exwm-randr-monitor))
           monitor-active-list)
    exwm-workspace--current))

(defun my/switch-monitor ()
  (interactive)
  (let ((monitors (my/get-screens))
    (current (frame-parameter exwm-workspace--current 'exwm-randr-monitor)))
    (setf (alist-get (intern current) monitor-active-list)
      exwm-workspace--current)
    (when (= 2 (length monitors))
  (let ((other-monitor (if (string= current (car monitors))
               (cadr monitors)
                 (car monitors))))
    (when-let ((workspace (alist-get (intern other-monitor) monitor-active-list)))
      (exwm-workspace-switch workspace))))))

(define-key *root-map* (kbd "%") #'my/move-to-monitor)
(add-to-list 'exwm-input-global-keys
         `(,(kbd "<s-iso-lefttab>") . my/switch-monitor)) 


(defun my/init-workspace-module ()
  (my/setup-workspace-monitors)
  (advice-add #'delete-frame
      :around
      #'delete-frame-cleanup-workspace-monitor)
  (advice-add #'exwm-workspace-switch
      :before
      #'workspace-switch-record-last))


(add-hook 'exwm-init-hook 'my/init-workspace-module)

(defun my/flip-screens ()
  (interactive)
  (let* ((screens (my/get-screens))
         (primary (car screens))
         (secondary (cadr screens)))
    (setq my/monitor-list
          (mapcar (lambda (a)
                    (cons (car a)
                          (if (string= (cdr a)
                                       primary)
                              secondary
                            primary)))
                  my/monitor-list)))
  (my/update-workspace-monitors))

(provide 'exwm-workspace-counsel)
;;; exwm-workspace-counsel.el ends here
