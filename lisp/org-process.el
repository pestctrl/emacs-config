;;; org-process.el --- Some useful functions cutting down big piles of stuff in org mode -*- lexical-binding: t -*-

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

;; These functions help me slash a huge subtree that is ungodly awful to handle.
;; It helps me quickly sort through a huge subtree that has received no attention
;; for a while.  Quick way to jump-start the cleaning process.

;;; Code:

(require 'cl)
(require 'org)
(require 'org-loop)

;; Utilities that will help with the rest of the code.

(defun get-random-uuid ()
  (replace-regexp-in-string "\n$" ""
                            (shell-command-to-string "uuidgen")))

(defun op/refile-to-location (file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun op/refile-to-point (file point)
  (org-refile nil nil (list nil file nil point)))

(defun op/lenient-y-or-n-p (prompt)
  "Prompt a `y-or-n-p' with prompt `PROMPT', but allow for actions to be taken in X-windows."
  (save-excursion
    (let ((query-replace-map (copy-keymap query-replace-map)))
      (define-key query-replace-map [t] 'skip)
      (y-or-n-p prompt))))


;; Actual user functions

(defun process-youtube-headline ()
  (interactive)
  (op/org-add-tag "watch")
  (save-window-excursion
    (save-excursion
      (if (op/lenient-y-or-n-p "Educational?")
          (progn (org-todo)
                 (op/org-add-tag
                  (if (op/lenient-y-or-n-p "short?")
                      "short"
                    "long"))
                 (op/org-add-tag "grow")
                 (op/refile-to-location (my/agenda-file "sandbox.org")
                                        "Educational Youtube Videos"))
        (org-todo "TODO")
        (op/org-add-tag "rest")
        (op/refile-to-location (my/agenda-file "eternal.org")
                               "Entertaining YouTube Videos")))))

(defun op/insert-category-heading (point heading-name)
  (let* ((uuid (get-random-uuid))
         (new-heading (concat heading-name
                              ": "
                              uuid))
         (condition (= point (point))))
    (prog1  
        (save-excursion
          (goto-char point)
          (org-insert-heading)
          (insert new-heading)
          (my/org-add-tag "sorting")
          (beginning-of-line)
          (forward-char)
          (point-marker))
      (when condition
        (outline-next-heading)))))

;; (defun op/refile-quick-sort ()
;;   (interactive)
;;   (find-file (my/agenda-file "refile.org"))
;;   (goto-char (point-min))
;;   (outline-next-heading)
;;   (let* ((youtube-marker (op/insert-category-heading "Youtube Videos"))
;;          (reddit-marker (op/insert-category-heading "Reddit Things")))
;;     (while
;;         (progn
;;           (org-copy-subtree)
;;           (let ((tree-string (current-kill 0)))
;;             (cond ((or (string-match-p "Watch \"" tree-string)
;;                        (string-match-p "youtube\.com/" tree-string)
;;                        (string-match-p "youtu\.be/" tree-string))
;;                    (op/refile-to-point (buffer-file-name) youtube-marker))
;;                   ((or (string-match-p "reddit\.com" tree-string)
;;                        (string-match-p "redd\.it" tree-string))
;;                    (op/refile-to-point (buffer-file-name) reddit-marker))
;;                   (t (outline-next-heading))))))))

(defun op/refile-quick-sort ()
  (interactive)
  (find-file (my/agenda-file "refile.org"))
  (goto-char (point-min))
  (let ((beg (ol/get-bot-marker)))
    (outline-next-heading)
    (let* ((other-marker (op/insert-category-heading beg "Other"))
           (youtube-marker (op/insert-category-heading beg "Youtube Videos"))
           (reddit-marker (op/insert-category-heading beg "Reddit Things")))
      (olsb/children
        (org-copy-subtree)
        (let ((tree-string (current-kill 0)))
          (cond ((and (member "sorting" (org-get-tags-at (point)))
                      (let ((heading-text (org-get-heading)))
                        (not (remove-if-not (lambda (regex)
                                              (string-match-p regex heading-text))
                                            '("Reddit Things: " "Youtube Videos: " "Other: ")))))
                 nil)
                ((or (string-match-p "Watch \"" tree-string)
                     (string-match-p "youtube\.com/" tree-string)
                     (string-match-p "youtu\.be/" tree-string))
                 (op/refile-to-point (buffer-file-name) youtube-marker))
                ((or (string-match-p "reddit\.com" tree-string)
                     (string-match-p "redd\.it" tree-string))
                 (op/refile-to-point (buffer-file-name) reddit-marker))
                (t
                 (op/refile-to-point (buffer-file-name) other-marker)))))
      (dolist (regex '("Reddit Things: " "Youtube Videos: " "Other: "))
        (goto-char (point-min))
        (when (search-forward (concat "** " regex))
          (my/org-delete-promote))))))

(defun op/org-sort-subtree ()
  (interactive)
  (let ((db '((1-undecided . nil)))
        (beg (ol/get-bot-marker))
        (path (org-get-outline-path t)))
    (org-set-tags (delete "sorting" (org-get-tags nil t)))
    (ols/children
     (if (member "sorting" (org-get-tags (point) t))
         (let* ((heading (org-get-heading t t t t))
                (index-of (string-match-p ":" heading))
                (category (intern (substring heading 0 index-of))))
           (forward-char)
           (setf (alist-get category db)
                 (point-marker))
           nil)
       (let* ((category-name (completing-read "Category? " (sort (mapcar #'car db) #'ivy-string<)))
              (category (intern category-name))
              (entry (assoc category db)))
         (unless entry ;; New category! 
           (let ((refile-location (op/insert-category-heading beg category-name)))
             (setf (alist-get category db)
                   refile-location)))
         (when-let (location (alist-get category db))
           (op/refile-to-point (buffer-file-name) location)))))))


(use-exwm
  :config
  (require 'fireorg)

  (defun op/youtube-loop ()
    (interactive)
    (let ((ff (get-buffer "fireorg")))
      (if (null ff)
          (progn
            (fireorg/make-fireorg-window)
            (run-with-timer 4 nil 'op/youtube-loop))
        (let ((org (find-file-noselect (my/agenda-file "refile.org"))))
          (fireorg/setup org ff
            ;; Goto youtube node
            (beginning-of-buffer)
            (search-forward "Youtube Videos: ")
            (org-back-to-heading)
            ;; Loop through the children
            (ol/children
              (fireorg/open-link ff
                (when (op/lenient-y-or-n-p "Refile this one?")
                  (process-youtube-headline)
                  (org-get-last-sibling)))))))))

  (defun op/get-rid-of-entertaining-youtube-videos ()
    (interactive)
    (let ((ff (get-buffer "fireorg")))
      (if (null ff)
          (progn
            (fireorg/make-fireorg-window)
            (run-with-timer 4 nil 'op/get-rid-of-entertaining-youtube-videos))
        (let ((org (find-file-noselect (my/agenda-file "refile.org"))))
          (fireorg/setup org ff
            (beginning-of-buffer)
            (search-forward "Youtube Videos: ")
            (org-back-to-heading)

            (ol/children
              (fireorg/open-link ff
                (when (not (op/lenient-y-or-n-p "Educational?"))
                  (save-excursion
                    (save-window-excursion
                      (op/org-add-tag "watch")
                      (org-todo "TODO")
                      (op/org-add-tag "rest")
                      (op/refile-to-location (my/agenda-file "eternal.org")
                                             "Entertaining YouTube Videos")))
                  (org-get-last-sibling)))))))))

  (defun fireorg/org-sort-subtree (org)
    (interactive (list (current-buffer)))
    (let ((ff (get-buffer "fireorg")))
      (if (null ff)
          (progn
            (fireorg/make-fireorg-window)
            (run-with-timer 4 nil 'fireorg/org-sort-subtree org))
        (fireorg/setup org ff
          (let ((db '((1-undecided . nil)))
                (beg (ol/get-bot-marker))
                (path (org-get-outline-path t)))
            (org-set-tags (delete "sorting" (org-get-tags nil t)))
            (ols/children
              (if (member "sorting" (org-get-tags))
                  (let* ((heading (org-get-heading t t t t))
                         (index-of (string-match-p ":" heading))
                         (category (intern (substring heading 0 index-of))))
                    (forward-char)
                    (setf (alist-get category db)
                          (point-marker))
                    nil)
                (fireorg/open-link ff
                  (let* ((category-name (completing-read "Category? " (sort (mapcar #'car db) #'ivy-string<)))
                         (category (intern category-name))
                         (entry (assoc category db)))
                    (unless entry ;; New category! 
                      (let ((refile-location (op/insert-category-heading beg category-name)))
                        (setf (alist-get category db)
                              refile-location)))
                    (prog1 (when-let (location (alist-get category db))
                             (op/refile-to-point (buffer-file-name) location))
                      (recenter))))))))))))

;; (defun op/open-loop ()
;;   (interactive)
;;   (let* ((ff-buffer (or (get-buffer "fireorg")
;;                         (ivy-read "Firefox Buffer: " #'internal-complete-buffer
;;                                   :keymap ivy-switch-buffer-map
;;                                   :preselect (buffer-name (other-buffer (current-buffer)))
;;                                   :matcher #'ivy--switch-buffer-matcher)))
;;          (id (with-current-buffer ff-buffer exwm--id)))
;;     (while t
;;       (let ((url (->> (org-offer-links-in-entry (current-buffer) (point) 0)
;;                       (car)
;;                       (get-text-property 0 'htmlize-link)
;;                       (cadr))))
;;         (start-process-shell-command "refile-open" nil (format "firefox -P youtube \"%s\"" url))
;;         (switch-window)
;;         (op/lenient-y-or-n-p "Next")
;;         (switch-window))
;;       (org-end-of-subtree t t)
;;       (exwm-background/exwm-input--fake-key-to-window id ?\C-w))))

(provide 'org-process)
;; org-process.el ends here
