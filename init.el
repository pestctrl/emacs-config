(setq package-list '(org use-package quelpa quelpa-use-package))
;; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

;; activate all the packages (in particular autoloads)
(package-initialize)

(when (<= emacs-major-version 26)
  (setq gnutls-algorithm-priority
	    "NORMAL:-VERS-TLS1.3"))

(setq gc-cons-threshold (* 100 1024 1024))

;; Add my modules
(progn
  (let ((lisp-dir (expand-file-name "lisp/"
                                    user-emacs-directory)))
    (add-to-list 'load-path lisp-dir)

    (let ((default-directory lisp-dir))
      (normal-top-level-add-subdirs-to-load-path))

    (let ((default-directory
           (expand-file-name "submodule/"
                             user-emacs-directory)))
      (normal-top-level-add-subdirs-to-load-path))))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq use-package-always-ensure t)

(require 'libs)

(require 'emacs-custom-load-or-ask)
(require 'my-predicates)

(when my-ec/at-ti
  (require 'work-proxy-config)
  (ti-proxy-mode 1))

(require 'org)
(require 'use-package)
(require 'quelpa)
(require 'quelpa-use-package)

(when (null window-system)
  (require 'term-compat))

(when (native-comp-available-p)
  (require 'comp)
  (condition-case nil
      (require 'comp-run)
    (error nil))
  (let ((symbol
	 (if (boundp 'native-comp-jit-compilation-deny-list)
	     'native-comp-jit-compilation-deny-list
	   'native-comp-deferred-compilation-deny-list)))
    (add-to-list symbol ".*realgud.*"))
  ;; (setenv "PATH" (concat "/db/sds/packages2/emacs-master/bin:" (getenv "PATH")))
  )

(require 'my-keymap)

(when (boundp 'face--new-frame-defaults)
  (define-obsolete-variable-alias
    'face-new-frame-defaults 'face--new-frame-defaults
    "28.1"))

(when my/puppet-p
  (setq warning-minimum-level :emergency))

(when (not my-ec/is-wsl)
  (add-to-list 'default-frame-alist '(width  . 200))
  (add-to-list 'default-frame-alist '(height . 60)))

;; It is imperative that this be loaded for a nice emacs
;; experience. Only SUPER stable stuff goes in this file, and should
;; rarely be modified
(org-babel-load-file
 (expand-file-name "config-min.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "config-look-and-feel.org"
                   user-emacs-directory))

(setq my-switch-found (member "-min" command-line-args))
(setq command-line-args (delete "-min" command-line-args))

;; Load additional exwm stuff that changes constantly
(when (and (not my-switch-found)
           my-ec/load-full-config)
  (org-babel-load-file
   (expand-file-name "config-emacs.org"
                     user-emacs-directory))

  (org-babel-load-file
   (expand-file-name "config-programming.org"
                     user-emacs-directory))

  (when (file-exists-p "~/.config/emacs-mail.el")
    (require 'emacs-mail))

  (org-babel-load-file
   (expand-file-name "config-ext.org"
                     user-emacs-directory))

  ;; Load work stuff when at work.
  (when my-ec/at-ti
    (require 'work-config))

  (when (and (not my-ec/at-ti) my-ec/load-org-config)
    (org-babel-load-file
     (expand-file-name "config-org.org"
                       user-emacs-directory))

    (org-babel-load-file
     (expand-file-name "helpers.org"
                       user-emacs-directory)))

  (org-babel-load-file
   (expand-file-name "my-redefs.org"
                     user-emacs-directory))

  (use-exwm
   :config
   (org-babel-load-file
    (expand-file-name "config-exwm.org"
                      user-emacs-directory)))

  (when my-ec/enable-exwm
    (require 'exwm))

  ;; Ideally as a last step, load all things in submodule directory
  (org-babel-load-file
   (expand-file-name "config-submodules.org"
                     user-emacs-directory)))

;; Set gc-cons-threshold lower, to decrease GC times.
(setq gc-cons-threshold (* 20 1024 1024))

(setq my/finished t)
;; Testing pull from windows

(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
