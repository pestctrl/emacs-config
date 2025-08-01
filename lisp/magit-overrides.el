;;; magit-overrides.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-11-18 17:38]

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
(require 'magit)

;; Push all branches
(defun my/magit-push-all ()
  "Push all branches."
  (interactive)
  (magit-run-git-async "push" "-v"
                       (magit-read-remote "Remote")
                       "--all"))

(transient-append-suffix 'magit-push "m"
  '("a" "all remotes" my/magit-push-all))

(defun my/magit-worktree-checkout (path branch)
  "Checkout BRANCH in a new worktree at PATH."
  (interactive
   (let ((branch (magit-read-branch-or-commit "Checkout")))
     (list (funcall magit-worktree-read-directory-name-function
                    (format "Checkout %s in new worktree: " branch))
           branch)))
  (magit-run-git-async "worktree" "add" (magit--expand-worktree path) branch))

(defun my/magit-worktree-branch (path branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH."
  (interactive
   `(,(funcall magit-worktree-read-directory-name-function
               "Create worktree: ")
     ,@(magit-branch-read-args "Create and checkout branch")
     ,current-prefix-arg))
  (magit-run-git-async "worktree" "add" (if force "-B" "-b")
                       branch (magit--expand-worktree path) start-point))

(defun magit-show-ancestor-merges (revs &optional args files)
  (interactive (cons (magit-read-starting-point "Ancestry path for: ")
                     (magit-diff-arguments)))
  (let ((flags "--merges --oneline --reverse --first-parent"))
    (async-shell-command
     (shell-and
      (format "git --no-pager log --oneline %s~1..%s"
              revs revs)
      (format "git --no-pager log %s %s..origin/main | cut -c -70 | head -n 10"
              flags revs)))))

(transient-append-suffix 'magit-worktree "b"
  '("B" "[async] worktree" my/magit-worktree-checkout))

(transient-append-suffix 'magit-worktree "c"
  '("C" "[async] branch and worktree" my/magit-worktree-branch))

(transient-append-suffix 'magit-log "s"
  '("A" "Ancestry path" magit-show-ancestor-merges))

(transient-append-suffix 'magit-fetch "-t"
  '("-f" "Force fetch" "--force"))

;; This is fixed in 5478d4e of magit/transient (committed )
;; (require 'transient-bug)
;; (with-eval-after-load 'vertico-multiform
;;   (add-to-list 'vertico-multiform-commands
;;                '(magit:--author flat (vertico-cycle . t)))
;;   (add-to-list 'vertico-multiform-commands
;;                '(magit:-- flat (vertico-cycle . t))))

;; Update all submodules
(defun magit-submodule-update-recursive ()
  (interactive)
  (magit-run-git-async "submodule" "update" "--init" "--recursive"))

(transient-append-suffix 'magit-submodule "u"
  '("U" "Update all (recursively)" magit-submodule-update-recursive))

(defvar my/magit-authors (make-hash-table :test 'equal))

(defun my/magit-invalidate-authors ()
  (interactive)
  (remhash (magit-gitdir) my/magit-authors))

(defun my/magit-get-authors ()
  (or (gethash (magit-gitdir) my/magit-authors)
      (puthash (magit-gitdir)
               (mapcar (lambda (line)
                         (save-excursion
                           (and (string-match "\\`[\s\t]+[0-9]+\t" line)
                                (list (substring line (match-end 0))))))
                       (magit-git-lines "shortlog" "-n" "-s" "-e" "HEAD"))
               my/magit-authors)))

(defun my/magit-transient-read-person (prompt initial-input history)
  (magit-completing-read
   prompt
   (my/magit-get-authors)
   nil nil initial-input history))

(advice-add #'magit-transient-read-person
            :override
            #'my/magit-transient-read-person)

(defun my/transient--show ()
  (transient--timer-cancel)
  (setq transient--showp t)
  (let ((transient--shadowed-buffer (current-buffer))
        (focus nil))
    (setq transient--buffer (get-buffer-create transient--buffer-name))
    (with-current-buffer transient--buffer
      (when transient-enable-popup-navigation
        (setq focus (or (button-get (point) 'command)
                        (and (not (bobp))
                             (button-get (1- (point)) 'command))
                        (transient--heading-at-point))))
      (erase-buffer)
      (run-hooks 'transient-setup-buffer-hook)
      (when transient-force-fixed-pitch
        (transient--force-fixed-pitch))
      (setq window-size-fixed 'width)
      (when (bound-and-true-p tab-line-format)
        (setq tab-line-format nil))
      (setq header-line-format nil)
      (setq mode-line-format
            (if (or (natnump transient-mode-line-format)
                    (eq transient-mode-line-format 'line))
                nil
              transient-mode-line-format))
      (setq mode-line-buffer-identification
            (symbol-name (oref transient--prefix command)))
      (if transient-enable-popup-navigation
          (setq-local cursor-in-non-selected-windows 'box)
        (setq cursor-type nil))
      (setq display-line-numbers nil)
      (setq show-trailing-whitespace nil)
      (transient--insert-groups)
      (when (or transient--helpp transient--editp)
        (transient--insert-help))
      (when-let ((line (transient--separator-line)))
        (insert line)))
    (unless (window-live-p transient--window)
      (setq transient--window
            (display-buffer transient--buffer
                            transient-display-buffer-action)))
    (when (window-live-p transient--window)
      (with-selected-window transient--window
        ;; Save the previous value of the 'no-other-window window parameter.
        (set-window-parameter nil 'prev--no-other-window
                              (window-parameter nil 'no-other-window))
        (goto-char (point-min))
        (when transient-enable-popup-navigation
          (transient--goto-button focus))
        (transient--fit-window-to-buffer transient--window)))))

(advice-add #'transient--show
            :override
            #'my/transient--show)

;; What's so great about the above advice? Why can't the below work?!
;;
;; (defun my/transient--show-post ()
;;   (let ((transient--shadowed-buffer (current-buffer))
;;         (focus nil))
;;     (setq transient--buffer (get-buffer-create transient--buffer-name))
;;     (with-current-buffer transient--buffer
;;       (when transient-enable-popup-navigation
;;         (setq focus (or (button-get (point) 'command)
;;                         (and (not (bobp))
;;                              (button-get (1- (point)) 'command))
;;                         (transient--heading-at-point)))))
;;     (when (window-live-p transient--window)
;;       (with-selected-window transient--window
;;         (set-window-parameter nil 'prev--no-other-window
;;                               (window-parameter nil 'no-other-window))
;;         (set-window-parameter nil 'no-other-window t)
;;         (goto-char (point-min))
;;         (when transient-enable-popup-navigation
;;           (transient--goto-button focus))
;;         (transient--fit-window-to-buffer transient--window)))))

;; (advice-add #'transient--show
;;             :after
;;             #'my/transient--show-post)

(defun my/transient--delete-win-restore-window-param (&rest ignore)
  (when (window-live-p transient--window)
    (with-selected-window transient--window
      ;; Restore the value
      (set-window-parameter nil 'no-other-window
                            (window-parameter nil 'prev--no-other-window)))))

(advice-add #'transient--delete-window
            :before
            #'my/transient--delete-win-restore-window-param)

(provide 'magit-overrides)
;;; magit-overrides.el ends here
