(setq package-list '(org use-package))
;; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

;; activate all the packages (in particular autoloads)
(package-initialize)

(when (not package-archive-contents)
      (package-refresh-contents))
;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'org)
(require 'use-package)

(setq use-package-always-ensure t)

(defvar my/enable-exwm t)

;; Add my modules

(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'libs)

(when my/enable-exwm
  (use-package exwm)
  (setq exwm-input-global-keys nil))

(load-file "~/.emacs.d/lisp/keymap.el")

(when my/enable-exwm
  (org-babel-load-file
   (expand-file-name "config-exwm.org"
                     user-emacs-directory))

(org-babel-load-file
 (expand-file-name "config-base.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "config-org.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "my-redefs.org"
                   user-emacs-directory))

(setq my/finished t)
;; Testing pull from windows
