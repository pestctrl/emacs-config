(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "c616e584f7268aa3b63d08045a912b50863a34e7ea83e35fcab8537b75741956" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(debug-on-error t)
 '(global-company-mode t)
 '(iswitchb-mode t)
 '(line-number-mode nil)
 '(org-agenda-files
   (quote
    ("~/MEGA/org/agenda/learn.org" "~/MEGA/org/agenda/work.org" "~/MEGA/org/agenda/tech.org" "~/MEGA/org/agenda/school.org" "~/MEGA/org/agenda/refile.org" "~/MEGA/org/agenda/personal.org" "~/MEGA/org/agenda/people.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(org-super-agenda-mode t)
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (ocodo-svg-modelines skewer-mode inf-ruby solarized-theme org-super-agenda restclient shell-pop org-toodledo dired-narrow dired+ quelpa-use-package geiser treemacs-projectile treemacs org-wunderlist youtube-dl irony dired-aux elfeed-org elfeed calfw-ical mu4e org-protocol ox-reveal ob-clojure ob-core org-caldav calfw-org org-habit epa-file switch-window ibuf-ext evil helm-config exwm-randr exwm-config exwm-systemtray use-package glsl-mode oauth2 ng2-mode transpose-frame clojure-snippets common-lisp-snippets go-snippets haskell-snippets java-snippets ensime yasnippet-bundle yasnippet helm-exwm smex matlab-mode beacon company-flx dired-du diredful free-keys elfeed-goodies exwm-surf el-autoyas flycheck-clojure flycheck-haskell flycheck-irony flycheck-pycheckers company-erlang company-ghc company-ghci company-go company-lua company-arduino company-c-headers company-cmake company-distel company-irony ac-c-headers ac-html ac-html-angular ac-slime ac-cider ace-window exwm angular-mode neotree smart-mode-line-powerline-theme smart-mode-line helm-projectile projectile ace-jump-mode ace-jump-buffer ace-jump-helm-line resize-window volume babel babel-repl lua-mode pocket-reader el-pocket magit-svn magit dirtree mu4e-alert habitica scala-mode auto-complete w3m calfw calfw-gcal org-gcal nlinum nlinum-relative color-theme-modern linum-relative helm i3wm org-bullets windresize slime powerline-evil persistent-soft pdf-tools multi-term ergoemacs-mode epresent engine-mode)))
 '(safe-local-variable-values (quote ((org-log-done))))
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
 '(org-agenda-date-today ((t (:inherit org-agenda-date :foreground "cyan" :slant italic :weight bold :height 1.1 :width ultra-condensed))))
 '(org-agenda-structure ((t (:foreground "LightSkyBlue" :box (:line-width 1 :color "grey75" :style released-button))))))
