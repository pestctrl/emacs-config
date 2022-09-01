;;; my-org-misc.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-04-29 21:30]

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
(require 'org-hide-property-drawers)

(global-set-key (kbd "C-x C-o") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-src-window-setup 'current-window)

(setq org-list-allow-alphabetical t)
(setq org-todo-repeat-to-state t)

(setq org-src-ask-before-returning-to-edit-buffer nil)

(with-eval-after-load 'outline
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all))

(setq org-ctrl-k-protect-subtree t)

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)

(add-to-list 'org-structure-template-alist
             '("sv" . "src :results value"))
(add-to-list 'org-structure-template-alist
             '("so" . "src :results output"))
(add-to-list 'org-structure-template-alist
             '("n" . "notes"))

(set-face-attribute 'org-agenda-date-today nil :inherit 'org-agenda-date :foreground "cyan" :slant 'italic :weight 'bold :height 1.1)
(set-face-attribute 'org-agenda-structure  nil :foreground "LightSkyBlue" :box '(:line-width 1 :color "grey75" :style released-button))
(set-face-attribute 'org-ellipsis          nil :foreground "turquoise" :underline nil)
(when (and (not (eq system-type 'windows-nt))
           (find-font (font-spec :name "Font Awesome 5 Free")))
  (setq org-ellipsis " "))

(setq org-log-done 'time)


(setq org-cycle-separator-lines 0)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-link-abbrev-alist
      '(("youtube" . "https://youtube.com/watch?v=")))

(setq org-use-speed-commands t)

;;; org-refile configuration
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(if my/puppet-p
    (setq org-refile-targets `((nil :maxlevel . 9)
                               (my/all-agenda-files :maxlevel . 9)
                               (my/aux-refile-files :maxlevel . 3)
                               ;; (my/non-agenda-files :maxlevel . 3)
                               ;; ("~/MEGA/org/entries/panic.org" :maxlevel . 9)
                               ))
  (setq org-refile-targets `((nil :maxlevel . 9)
                             (my/all-agenda-files :maxlevel . 9)
                             (my/aux-refile-files :maxlevel . 3)
                             (my/non-agenda-files :maxlevel . 3)
                             ("~/MEGA/org/entries/panic.org" :maxlevel . 9)
                             )))

(setq org-refile-use-cache t)

(setq org-refile-target-verify-function
      (lambda ()
        (let ((tags (org-get-tags-at)))
          (and (not (member "ARCHIVE" tags))
               (not (equal "DONE" (org-get-todo-state)))))))

(setq org-agenda-show-future-repeats nil)

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Uh oh
(setq org-clock-continuously t)

(defun my/org-un-project ()
  (interactive)
  (let ((level (org-current-level)))
    (org-map-entries 'org-do-promote (format "LEVEL>%d" level) 'tree)
    (org-cycle t)))

(defun my/org-delete-promote ()
  (interactive)
  (my/org-un-project)
  (org-cut-subtree))

(defun my/org-reset-clock ()
  (interactive)
  (setq org-clock-out-time nil))

(with-eval-after-load 'ox-html
  (setq org-html-table-default-attributes
        '(:border "2" :cellspacing "0" :cellpadding "6")))

(use-package git-auto-commit-mode
  :config
  (require 'git-auto-fast-forward-mode)
  (require 'ssh-key-management)
  (use-package keychain-environment)
  (setq gac-automatically-add-new-files-p nil)
  (setq-default gac-debounce-interval 300)

  (defmacro defun-cached (name args &rest body)
    (let ((var-name (intern (concat (symbol-name name) "--cached"))))
      `(progn
         (defvar ,var-name t)

         (defun ,name ,args
           (when ,var-name
             (setq ,var-name nil)
             (run-with-timer 15 nil #'(lambda () (setq ,var-name t)))
             ,@body)))))

  (defun gac-use-magit-push (buffer)
    (let ((default-directory (file-name-directory (buffer-file-name buffer))))
      (magit-push-current-to-pushremote nil)))

  (advice-add #'gac-push :override #'gac-use-magit-push)

  ;; (defvar gac-auto-merge-branch-list nil)
  ;; (make-variable-buffer-local 'gac-auto-merge-branch-list)

  (defun-cached gac-run-gaff ()
    (gaff/trigger))

  (defun-cached gac-run-ssh-add ()
    (rb/ssh-add))

  (add-hook 'git-auto-commit-mode-hook
            #'gac-run-gaff)

  (add-hook 'git-auto-commit-mode-hook
            #'gac-run-ssh-add)

  (add-hook 'git-auto-commit-mode-hook
            #'gac-after-save-func t t)

  (add-hook 'git-auto-commit-mode-hook
            #'keychain-refresh-environment)

  (add-to-list 'safe-local-variable-values
               '(gac-automatically-push-p . t))

  (add-to-list 'safe-local-variable-values
               '(gac-automatically-push-p . nil))

  (defun gac-commit-message (filename)
    (format "%s autocommit: %s\n\n%s"
            (system-name)
            (format-time-string "%Y/%m/%d %H:%M:%S")
            filename))

  (setq gac-default-message
        #'gac-commit-message)

  (defun my/gac--debounced-save ()
    (let* ((actual-buffer (current-buffer)))
      (when-let (current-timer (gethash actual-buffer gac--debounce-timers))
        (remhash actual-buffer gac--debounce-timers)
        (cancel-timer current-timer))
      (puthash actual-buffer
               (run-at-time gac-debounce-interval nil
                            #'gac--after-save
                            actual-buffer)
               gac--debounce-timers)))

  (advice-add #'gac--debounced-save
              :override
              #'my/gac--debounced-save)

  (defun gac-debounce-again-if-magit-in-progress (buf)
    (with-current-buffer buf
      (if (ga/should-be-automatic)
          t
        (gac--debounced-save)
        nil)))

  (advice-add #'gac--after-save
              :before-while
              #'gac-debounce-again-if-magit-in-progress))

(when (eq window-system 'x)
  (define-key org-mode-map
    (kbd "C-S-b")
    #'(lambda () (interactive) (org-back-to-heading))))

(define-key org-mode-map (kbd "M-g o") (lambda () (interactive) (org-back-to-heading)))
(define-key org-mode-map (kbd "C-M-a") (lambda () (interactive) (org-back-to-heading)))

;; My replacement for follow mode
(defun org-agenda-jump-to-heading-show ()
  (interactive)
  (let ((agenda-buffer (buffer-name))
	    (agenda-window (selected-window))
        (indirect-window
	     (and org-last-indirect-buffer
		      (get-buffer-window org-last-indirect-buffer))))
    (save-window-excursion (org-agenda-do-tree-to-indirect-buffer t))
    (pop-to-buffer org-last-indirect-buffer)
    (quick-bury-mode)))

(require 'quick-bury)

(with-eval-after-load "org-agenda"
  (define-key org-agenda-mode-map (kbd "j")
    #'org-agenda-jump-to-heading-show))

(provide 'my-org-misc)
;;; my-org-misc.el ends here
