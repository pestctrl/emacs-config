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
(setq org-agenda-window-setup 'other-window)

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
                               ;; (my/aux-refile-files :maxlevel . 3)
                               ;; (my/non-agenda-files :maxlevel . 3)
                               ;; ("~/MEGA/org/entries/panic.org" :maxlevel . 9)
                               ))
  (setq org-refile-targets `((nil :maxlevel . 9)
                             (my/all-agenda-files :maxlevel . 9)
                             (my/aux-refile-files :maxlevel . 3)
                             ;; TODO Figure out why this is causing a problem
                             ;; (my/non-agenda-files :maxlevel . 3)
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

(let ((fetcher (if (or noninteractive my-ec/at-ti) "github" "github-ssh")))
  (quelpa `(org-ql :repo "pestctrl/org-ql" :branch "my-changes" :fetcher ,fetcher)))
(require 'org-ql-find)

(defun org-ql-clock-in ()
  (interactive)
  (let ((marker (org-ql-completing-read (org-agenda-files)
                  :query-prefix nil
                  :query-filter nil
                  :prompt "Clock into entry: ")))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (org-clock-in))))

(define-prefix-command '*org-clock-map*)

(define-key *org-clock-map* (kbd "i") #'org-ql-clock-in)
(define-key *org-clock-map* (kbd "c") #'org-resolve-clocks)
(define-key *root-map* (kbd "C") '*org-clock-map*)

(define-key *root-map* (kbd "j") 'org-clock-goto)
(define-key *root-map* (kbd "o") 'org-agenda)

(use-package embark)

(defun org-embark-clock-in (target)
  (with-current-buffer (marker-buffer target)
    (goto-char target)
    (org-clock-in)))

(defun org-embark-goto (target)
  (switch-to-buffer (marker-buffer target))
  (goto-char target))

;; (embark-define-keymap embark-org-map
;;   nil
;;   ("c" org-embark-clock-in)
;;   ("RET" org-embark-goto))

;; (add-to-list 'embark-keymap-alist '(org . embark-org-map))

(defun org-ql-embark ()
  (interactive)
  (let* ((marker (org-ql-completing-read (org-agenda-files)
                   :query-prefix nil
                   :query-filter nil
                   :prompt "Find entry: "))
         (action (read-char "[RET]Goto, [c]lock")))
    (pcase action
      (13 (progn
            (switch-to-buffer (marker-buffer marker))
            (goto-char marker)))
      (99 (with-current-buffer (marker-buffer marker)
            (goto-char marker)
            (org-clock-in))))))

(define-key *root-map* (kbd "F") #'org-ql-embark)

(require 'ob)
(require 'ob-shell)
(require 'ob-dot)

(setq org-confirm-babel-evaluate nil)

(provide 'my-org-misc)
;;; my-org-misc.el ends here
