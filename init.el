(setq package-list '(org use-package exwm))
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

;; Add my modules
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory  "~/.emacs.d/submodule/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'libs)

(ec/load-or-ask-pred my/is-wsl "Are you running Emacs in WSL?")
(ec/load-or-ask-pred my/enable-exwm "Do you want to load EXMW?")
(ec/load-or-ask-pred my/at-ti "Are you at TI for work?")
(defvar is-windows (or my/is-wsl
                       (eq system-type
                           'windows-nt)))

(setq my/enable-exwm (and my/enable-exwm (eq 'x window-system)))

;; On TI VNC machine
(when my/at-ti (setq epg-pinentry-mode nil))

(require 'keymap)

;; It is imperative that this be loaded for a nice emacs
;; experience. Only SUPER stable stuff goes in this file, and should
;; rarely be modified
(org-babel-load-file
 (expand-file-name "config-min.org"
                   user-emacs-directory))

;; Load additional exwm stuff that changes constantly
(use-package exwm
  :config 
  (org-babel-load-file
   (expand-file-name "config-exwm.org"
                     user-emacs-directory)))

(org-babel-load-file
 (expand-file-name "config-ext.org"
                   user-emacs-directory))

;; Load the minimal org config stuff
(require 'my-org-misc)

;; These are less important when it comes to debugging emacs itself
(org-babel-load-file
 (expand-file-name "config-org.org"
                   user-emacs-directory))

(require 'my-org)

(org-babel-load-file
 (expand-file-name "my-redefs.org"
                   user-emacs-directory))

(when my/enable-exwm
  (require 'exwm)
  (setq exwm-input--global-prefix-keys
        (delq 24 exwm-input--global-prefix-keys)))

(setq my/finished t)
;; Testing pull from windows

(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
