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

(global-set-key (kbd "C-x C-o") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

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
(setq org-refile-targets `((nil :maxlevel . 9)
                           (my/all-agenda-files :maxlevel . 9)
                           ("~/MEGA/org/entries/panic.org" :maxlevel . 9)))

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
  (use-package keychain-environment)
  (setq gac-automatically-add-new-files-p nil)
  (setq-default gac-debounce-interval 300)

  (defun gac-use-magit-push (buffer)
    (let ((default-directory (file-name-directory (buffer-file-name buffer))))
      (magit-push-current-to-pushremote nil)))

  (defun gac-use-magit-fetch ()
    (let ((default-directory (file-name-directory (buffer-file-name (current-buffer)))))
      (magit-fetch-all nil)))

  (advice-add #'gac-push :override #'gac-use-magit-push)

  (defvar rb/ssh-default-key (format "~/.ssh/devices/%s/id_rsa" (system-name))
    "My default SSH key.")

  (defun rb/ssh-add (&optional arg)
    "Add the default ssh-key if it's not present.

With a universal argument, prompt to specify which key."
    (interactive "P")
    (when (or arg
              (not (rb/ssh-agent-has-keys-p)))
      (rb/ssh-add-in-emacs
       (if (not arg)
           rb/ssh-default-key
         (read-file-name
          "Add key: \n" "~/.ssh" nil 't nil
          (lambda (x)
            (not (or (string-suffix-p ".pub" x)
                     (string= "known_hosts" x)))))))))

  (defun rb/ssh-agent-has-keys-p ()
    "Return t if the ssh-agent has a key."
    (when (not
           (string-match-p
            "No identities"
            (shell-command-to-string "ssh-add -l")))
      t))

  (defun rb/ssh-add-in-emacs (key-file)
    "Run ssh-add to add a key to the running SSH agent."
    (let ((process-connection-type t)
          process)
      (unwind-protect
          (progn
            (setq process
                  (start-process
                   "ssh-add" nil "ssh-add"
                   (expand-file-name key-file)))
            (set-process-filter
             process 'rb/ssh-add-process-filter)
            (while (accept-process-output process)))
        (if (eq (process-status process) 'run)
            (kill-process process)))))

  (defun rb/ssh-add-process-filter (process string)
    (save-match-data
      (if (string-match ":\\s *\\'" string)
          (process-send-string process
                               (concat
                                (read-passwd string)
                                "\n"))
        (message "ssh-add: %s" string))))

  (advice-add #'keychain-refresh-environment
              :after
              (lambda (&rest args) (message "Remember to ssh-add!")))

  (add-hook 'git-auto-commit-mode-hook
            #'gac-use-magit-fetch)

  (add-hook 'git-auto-commit-mode-hook
            #'rb/ssh-add)

  (add-hook 'git-auto-commit-mode-hook
            #'gac-after-save-func t t)

  (add-hook 'git-auto-commit-mode-hook
            #'keychain-refresh-environment)

  (add-to-list 'safe-local-variable-values
               '(gac-automatically-push-p . t))

  (add-to-list 'safe-local-variable-values
               '(gac-automatically-push-p . nil))

  (defun gac-commit-message (filename)
    (format "Desktop autocommit: %s\n\n%s"
            (format-time-string "%Y/%m/%d %H:%M:%S")
            filename))

  (setq gac-default-message
        #'gac-commit-message))

(when (eq window-system 'x)
  (define-key org-mode-map
    (kbd "C-S-b")
    #'(lambda () (interactive) (org-back-to-heading))))

(provide 'my-org-misc)
;;; my-org-misc.el ends here
