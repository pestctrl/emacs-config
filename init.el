(setq package-list '(org-plus-contrib use-package))
; list the repositories containing them
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

; activate all the packages (in particular autoloads)
(package-initialize)

(when (not package-archive-contents)
      (package-refresh-contents))
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'org)
(require 'use-package)

(setq use-package-always-ensure t)

(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))(let ((--dolist-tail-- org-agenda-files) file) (while --dolist-tail-- (setq file (car --dolist-tail--)) (let (buffer) (org-check-agenda-file file) (setq buffer (if (file-exists-p file) (org-get-agenda-file-buffer file) (error No such file %s file))) (save-current-buffer (set-buffer buffer) (while (and (not (eobp)) (outline-next-heading)) (+ 1 2)))) (setq --dolist-tail-- (cdr --dolist-tail--))))

(org-babel-load-file
 (expand-file-name "my-redefs.org"
                   user-emacs-directory))
