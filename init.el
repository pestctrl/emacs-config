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

(let ((default-directory  "~/.emacs.d/submodule/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'libs)

(when (and (eq 'x window-system)
           my/enable-exwm)
  (use-package exwm)
  (setq exwm-input-global-keys nil))

(load-file "~/.emacs.d/lisp/keymap.el")

(when (and (eq 'x window-system)
           my/enable-exwm)
  (org-babel-load-file
   (expand-file-name "config-exwm.org"
                     user-emacs-directory)))

(org-babel-load-file
 (expand-file-name "config-base.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "config-org.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "my-redefs.org"
                   user-emacs-directory))

(defun test-monitor ()
  (interactive)
  (shell-command "xrandr --output DP1 --mode 1920x1080 --above eDP1"))

(defun 2k ()
  (interactive)
  (shell-command "xrandr --output DP1 --mode 2560x1440 --above eDP1"))

(defun top ()
  (interactive)
  (shell-command "xrandr --output DP1 --above eDP1"))

(defun bottom ()
  (interactive)
  (shell-command "xrandr --output DP1 --below eDP1"))

(defun left ()
  (interactive)
  (shell-command "xrandr --output DP1 --left-of eDP1"))

(defun right ()
  (interactive)
  (shell-command "xrandr --output DP1 --right-of eDP1"))


(setq my/finished t)
;; Testing pull from windows
;; This is a test comment
