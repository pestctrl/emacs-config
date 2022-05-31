(add-to-list 'default-frame-alist '(width  . 200))
(add-to-list 'default-frame-alist '(height . 60))

(setq package-list '(org use-package))
;; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

;; activate all the packages (in particular autoloads)
(package-initialize)

(when (<= emacs-major-version 26)
  (setq gnutls-algorithm-priority
	    "NORMAL:-VERS-TLS1.3"))

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
(progn
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

  (let ((default-directory  "~/.emacs.d/lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

  (let ((default-directory  "~/.emacs.d/submodule/"))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'libs)
(require 'emacs-custom-load-or-ask)

(ec/load-or-ask-pred 'my-ec/is-wsl "Are you running Emacs in WSL?")
(ec/load-or-ask-pred 'my-ec/enable-exwm "Do you want to load EXMW?")
(ec/load-or-ask-pred 'my-ec/at-ti "Are you at TI for work?")
(defvar is-windows (or my-ec/is-wsl
                       (eq system-type
                           'windows-nt)))

(ec/load-or-ask-pred 'my-ec/add-info-dir "Do you want an auxiliary info dir? ")

(when my-ec/add-info-dir
  (ec/load-or-ask-dir 'my-ec/info-dir "Info Directory? ")
  (add-to-list 'Info-directory-list my-ec/info-dir))

(setq my-ec/enable-exwm (and my-ec/enable-exwm (eq 'x window-system)))

(require 'keymap)

(when (boundp 'face--new-frame-defaults)
  (define-obsolete-variable-alias
    'face-new-frame-defaults 'face--new-frame-defaults
    "28.1"))

(ec/load-or-ask-pred 'my-ec/load-full-config "Do you want to load full config for emacs?")
(ec/load-or-ask-pred 'my-ec/load-org-config "Do you want to load org config?")

;; It is imperative that this be loaded for a nice emacs
;; experience. Only SUPER stable stuff goes in this file, and should
;; rarely be modified
(org-babel-load-file
 (expand-file-name "config-min.org"
                   user-emacs-directory))

(when my-ec/load-full-config

  ;; Load additional exwm stuff that changes constantly
  (use-exwm
   :config
   (org-babel-load-file
    (expand-file-name "config-exwm.org"
                      user-emacs-directory)))

  (org-babel-load-file
   (expand-file-name "config-ext.org"
                     user-emacs-directory))

  ;; Load work stuff when at work.
  (when my-ec/at-ti
    (require 'work-config))

  (when (and (not my-ec/at-ti) my-ec/load-org-config)
    (org-babel-load-file
     (expand-file-name "config-org.org"
                       user-emacs-directory)))

  (org-babel-load-file
   (expand-file-name "my-redefs.org"
                     user-emacs-directory))

  (when my-ec/enable-exwm
    (require 'exwm)))

(setq my/finished t)
;; Testing pull from windows

(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
