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

;; Uncomment if want to debug
;; (progn
;;   (toggle-debug-on-error))

;; Add my modules

(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'libs)

(setq use-package-always-ensure t)

(define-prefix-command '*root-map*)
(global-set-key (kbd "C-t") '*root-map*)

(define-key *root-map* (kbd "C-n") 'switch-window)
(define-key *root-map* (kbd "i") 'org-mru-clock-in)
(define-key *root-map* (kbd "C-i") 'leaving-computer)
(define-key *root-map* (kbd "C") 'org-resolve-clocks)
(define-key *root-map* (kbd "j") 'org-clock-goto)
(define-key *root-map* (kbd "o") 'switch-window)
(define-key *root-map* (kbd "n") 'toggle-notifications)
(define-key *root-map* (kbd "RET") 'dired-jump)

;; (org-babel-load-file
;;  (expand-file-name "config-exwm.org"
;;                    user-emacs-directory))

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
