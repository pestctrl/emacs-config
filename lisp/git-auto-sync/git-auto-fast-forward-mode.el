;;; git-auto-fast-forward-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-08-31 20:02]

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
(defvar gaff/watch-directories nil)

(defun ga/magit-not-in-progress (dir)
  (with-current-buffer (dired-noselect dir)
    (and (not (magit-rebase-in-progress-p))
         (not (magit-revert-in-progress-p))
         (not (magit-am-in-progress-p))
         (not (magit-merge-in-progress-p))
         (not (magit-cherry-pick-in-progress-p))
         (not (magit-sequencer-in-progress-p))
         (not (magit-bisect-in-progress-p)))))

(defun ga/on-auto-branch (dir)
  (with-current-buffer (dired-noselect dir)
    (member (magit-get-current-branch)
            '("desktop" "gaming-laptop" "puppet" "mobile"))))

(defun ga/should-be-automatic (dir)
  (and (if (ga/magit-not-in-progress dir)
           t
         (message "Oops, magit is in progress")
         nil)
       (if (ga/on-auto-branch dir)
           t
         (message "Oops, repo %s is on branch %s, which is not an automatic branch"
                  dir (magit-get-current-branch))
         nil)))

(defun gaff/get-open-buffers-in (folder)
  (remove-if-not (lambda (b) (string-prefix-p folder (buffer-file-name b)))
                 (buffer-list)))

(defun gaff/no-pending-gac-commits (folder)
  (->> gac--debounce-timers
       (hash-table-values)
       (mapcar (lambda (timer) (car (timer--args timer))))
       (remove-if-not (lambda (buffer) (string-prefix-p folder (buffer-file-name buffer))))
       (length)
       (zerop)))

(defun gaff/no-modified-buffers (folder)
  (->> (gaff/get-open-buffers-in folder)
       (remove-if-not (lambda (b) (buffer-modified-p b)))
       (length)
       (zerop)))

(defun gaff/no-git-changes (folder)
  (let ((default-directory folder))
    (string-empty-p (shell-command-to-string "git status -suno"))))

(defun gaff/fetch-fast-forward (repo branches)
  ;; TODO: work around this hack
  (with-current-buffer (dired-noselect repo)
    (shell-command "git fetch --all")
    (dolist (b branches)
      (let ((output-buffer (format "*merge-%s*" b)))
        (when (not (zerop (shell-command (format "git merge --ff-only %s" b) output-buffer)))
          (pop-to-buffer output-buffer)
          (user-error "Uh oh, one of the merges resulted in an error!"))))
    (magit-push-current-to-pushremote nil)))

(defun gaff/trigger ()
  (interactive)
  (dolist (info gaff/watch-directories)
    (let ((dir (car info))
          (branches (cdr info)))
      (when (and (ga/should-be-automatic dir)
                 (gaff/no-pending-gac-commits dir)
                 (gaff/no-modified-buffers dir)
                 (gaff/no-git-changes dir))
        (let ((buffers (gaff/get-open-buffers-in dir)))
          (unwind-protect
              (progn
                (dolist (b buffers)
                  (with-current-buffer b
                    (read-only-mode 1)))
                (gaff/fetch-fast-forward dir branches)
                (org-id-update-id-locations (directory-files-recursively "~/plaintext/org/" "^[^#].*.org$")))
            (dolist (b buffers)
              (with-current-buffer b
                (read-only-mode -1)))))))))

(defvar gaff/timer nil)

(define-minor-mode git-auto-fast-forward-mode ""
  nil nil nil
  (if git-auto-fast-forward-mode
      (setq gaff/timer (run-at-time nil 300 #'gaff/trigger))
    (cancel-timer gaff/timer)))

(provide 'git-auto-fast-forward-mode)
;;; git-auto-fast-forward-mode.el ends here
