;;; my-org-roam.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-08-10 11:58]

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

(require 'org-ql)
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (setq straight-vc-git-default-protocol 'ssh)

(define-prefix-command '*org-roam-map*)

(define-key pestctrl-minor-mode-map
            (kbd "C-c n")
            '*org-roam-map*)

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (my/org-file "org-roam"))
  (org-roam-use-completion-everywhere t)
  :bind (:map *org-roam-map*
              ("h" . org-roam-buffer-toggle)
              ("f" . my/org-roam-find-file)
              ("F" . my/org-roam-find-daily)
              ("p" . my/org-roam-find-project)
              ("T" . org-roam-dailies-goto-today)
              ("t" . org-roam-dailies-capture-today)
              ("i" . org-roam-node-insert)
              ("w" . org-roam-refile)
              ("j" . my/org-roam-logger-capture-current)
              ("c" . org-roam-capture)
              :map org-mode-map
              ("C-M-i" . completion-at-point))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  (setq org-roam-dailies-directory "daily/")

  (defvar-keymap org-capture-self-chat-keymap
    "C-c C-k" #'(lambda ()
                  (interactive)
                  (call-interactively #'org-edit-src-abort)
                  (call-interactively #'org-cut-special)
                  (call-interactively #'delete-window)))

  (defun self-chat-capture-template-display-src ()
    (with-current-buffer (org-capture-get :buffer)
      (goto-char (org-capture-get :insertion-point))
      (let ((src-buffer (save-window-excursion
                          (org-edit-special)
                          (setq-local minor-mode-overriding-map-alist
                                      `((org-src-mode . ,org-capture-self-chat-keymap)))
                          (current-buffer)
                          )))
        (org-display-buffer-split src-buffer nil))))

  (setq org-roam-dailies-capture-templates
        '(("j" "Journal" entry "* %<%H:%M> %?"
           :unnarrowed t
           :target (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                                  ("Journal")))
          ("J" "Journal with source" entry "* %<%H:%M> %?\n:PROPERTIES:\n:LOCATION: %a\n:END:"
           :unnarrowed t
           :target
           (file+head+olp "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                          ("Journal")))
          ("c" "self-chat" entry "* %<%H:%M> \n#+begin_src self-chat\n%?\n#+end_src"
           :unnarrowed t
           :target
           (file+head+olp "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n#+filetags: :dailies:%<%Y:%B:>\n"
                          ("Journal"))
           :after-finalize self-chat-capture-template-display-src
           :immediate-finish)
          ;; ("m" "Most Important Thing" entry "* TODO %? :mit:"
          ;;  :target (file+head+olp "%<%Y-%m-%d>.org"
          ;;                         "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>\n"
          ;;                         ("Most Important Thing(s)")))
          ))

  (require 'my-org-roam-logger)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("j" "Journal" entry "* %<%H:%M> %?" :target
           (file+datetree "%<%Y%m%d%H%M%S>-${slug}.org" 'day)
           :unnarrowed t)
          ("t" "tech tips" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: techtips\n")
           :unnarrowed t)))

  (require 'org-roam-util)

  (defun my/org-roam-find-file ()
    (interactive)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil nil
     (lambda (node)
       (let ((tags (org-roam-node-tags node)))
         (not (member "project" tags))))))


  (defun my/org-roam-find-project ()
    (interactive)
    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil nil
     (lambda (node)
       (let ((tags (org-roam-node-tags node)))
         (and (eq (org-roam-node-level node) 0)
              (member "project" tags)
              (not (member "done" tags)))))
     nil
     :templates
     '(("p" "project" plain ""
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}: %^{Description}\n#+category: ${title}\n#+filetags: project")
        :unnarrowed t))))

  (use-package consult-org-roam
    :demand t
    :commands (my/org-roam-find-daily)
    :config
    (require 'org-roam-util)

    (defun consult-org-roam-file-find (arg)
      "Find org-roam node with preview, if ARG open in other window."
      (interactive "P")
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (symbol-function 'consult-org-roam-node-read)))
        (let ((other-window (if arg t nil)))
          (org-roam-node-find other-window nil #'consult-org-roam--node-file-p))))

    (defun my/org-roam-find-daily ()
      (interactive)
      (cl-letf (((symbol-function 'org-roam-node-read)
                 (symbol-function 'consult-org-roam-node-read)))
        (org-roam-node-find nil nil
                            (my/org-roam-filter-by-tag "dailies")
                            (lambda (x y)
                              (string-lessp (org-roam-node-file (cdr y))
                                            (org-roam-node-file (cdr x)))))))))

;; (require 'org-roam-protocol)

(provide 'my-org-roam)
;;; my-org-roam.el ends here
