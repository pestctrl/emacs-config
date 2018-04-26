(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(debug-on-error nil)
 '(global-company-mode t)
 '(iswitchb-mode t)
 '(line-number-mode nil)
 '(org-agenda-files
   (quote
    ("~/MEGA/org/agenda/people.org" "/home/benson/MEGA/org/agenda/work.org" "/home/benson/MEGA/org/agenda/refile.org" "/home/benson/MEGA/org/agenda/school.org" "~/MEGA/org/entries/journal.gpg" "~/MEGA/org/agenda/tech.org" "/home/benson/MEGA/org/agenda/personal.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(package-selected-packages
   (quote
    (use-package glsl-mode oauth2 ng2-mode transpose-frame org clj-refactor clojure-snippets common-lisp-snippets company-auctex go-snippets haskell-snippets java-snippets ensime yasnippet-bundle yasnippet helm-exwm csharp-mode smex dashboard matlab-mode beacon company-flx dired-du diredful free-keys elfeed-goodies exwm-surf el-autoyas flycheck-clojure flycheck-haskell flycheck-irony flycheck-pycheckers company-erlang company-ghc company-ghci company-go company-lua company-arduino company-c-headers company-cmake company-distel company-lsp company-irony ac-c-headers ac-html ac-html-angular ac-slime ac-cider ace-window exwm angular-mode neotree smart-mode-line-powerline-theme smart-mode-line airline-themes counsel-projectile helm-projectile projectile ace-jump-mode ace-jump-buffer ace-jump-helm-line resize-window volume babel babel-repl lua-mode pocket-reader el-pocket magit-svn magit dirtree mu4e-alert habitica scala-mode auto-complete w3m wanderlust calfw calfw-gcal org-gcal nlinum nlinum-relative color-theme-modern linum-relative helm i3wm org-bullets auctex windresize slime powerline-evil persistent-soft pdf-tools multi-term ergoemacs-mode epresent engine-mode cyberpunk-theme cider)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date-today ((t (:inherit org-agenda-date :foreground "cyan" :slant italic :weight bold :height 1.1 :width ultra-condensed)))))
