(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "DodgerBlue2" "magenta3" "cyan3" "gray90"])
 '(custom-safe-themes
   (quote
    ("8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" default)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(org-agenda-files
   (quote
    ("~/MEGA/org/entries/reviews.gpg" "~/MEGA/org/2019-05-agenda/what.org" "~/MEGA/org/2019-05-agenda/lists.org" "~/MEGA/org/2019-05-agenda/datetree.org" "~/MEGA/org/entries/journal.gpg" "~/MEGA/org/2019-05-agenda/refile.org" "~/MEGA/org/2019-05-agenda/sandbox.org" "~/MEGA/org/2019-05-agenda/reference.org" "~/MEGA/org/2019-05-agenda/dev.org" "~/MEGA/org/2019-05-agenda/prod.org")))
 '(package-selected-packages
   (quote
    (quelpa-use-package quelpa orca pophint poshint f3 helm-fuzzy-find e2wm exwm exwm-firefox-core ivy-posframe dockerfile-mode docker realgud gruvbox-theme eterm-256color iy-go-to-char ivy ivy-hydra exwm-edit w3 w3m dired-collapse dired-subtree dired-filter google-this strace-mode ten-hundred-mode jupyter hackernews academic-phrases racket-mode flymd telephone-line cnfonts names ctable c-c-combo doom-themes helpful helm-org-rifle all-the-icons speed-type proof-general webpaste erc-hl-nick java-snippets mvn pass erc-hl-nicks erc-colorize bbdb- bbdb-ext erc gnorb bbdb window-purpose tide company-flow tern-auto-complete tern dotnet dotnet-mode elisp-def auto-highlight-symbol ccls xcscope ggtags indium cargo gradle-mode kdeconnect hy-mode hacker-typer csv-mode switch-buffer-functions org-bookmark-heading system-packages org-projectile treemacs-projectile expand-region org-wunderlist lua-mode treemacs-icons-dired memoize skeletor 0xc lsp-rust smart-mode-line-powerline-theme hyperbole eclim alert gscholar-bibtex org-noter pdf-tools md4rd arch-packer interleave sx desktop-environment org-journal-list indent-guide org-board org-brain org-edna org-clock-csv org-clock-convenience org-mru-clock lsp-ui dap-mode lsp-python lsp-java lsp-ruby lsp-mode smart-jump scrollkeeper elfeed-org elfeed rg ag dumb-jump rainbow-delimiters org-super-agenda htmlize exwm-x keyfreq org-jira eyebrowse omnisharp omnisharp-emacs typescript-mode csharp-mode restclient rjsx-mode go-mode go treemacs org-timeline plantuml-mode pkgbuild-mode airline-themes powerline magit-svn ein emacs-ipython-notebook processing-mode web-mode ledger-mode elpy skewer-mode js2-mode dired-narrow yaml-mode clj-refactor paredit geiser magit ensime yasnippet irony company dired-du ox-reveal oauth2 org-caldav calfw-org calfw-gcal calfw-ical calfw org-bullets ace-jump-mode switch-window counsel smex helm counsel-projectile helm-projectile projectile slime pulseaudio-control evil undo-tree multi-term swiper color-theme-modern org org-plus-contrib use-package)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(wtf-custom-alist
   (quote
    (("AES" . "Advanced Encryption Standard")
     ("PDF" . "Portable Document Format")
     ("RSA" . "Rivest-Shamir-Adleman")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "gray8" :foreground "#70FF00"))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :foreground "cyan" :slant italic :weight bold :height 1.1))))
 '(org-agenda-structure ((t (:foreground "LightSkyBlue" :box (:line-width 1 :color "grey75" :style released-button)))))
 '(org-ellipsis ((t (:foreground "turquoise"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#9E1CB2"))))
 '(rainbow-delimiters-depth-10-face ((t (:foreground "#1194f6"))))
 '(rainbow-delimiters-depth-11-face ((t (:foreground "#47B04B"))))
 '(rainbow-delimiters-depth-12-face ((t (:foreground "#FFED18"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#47B04B"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#1194f6"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#C90067"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#FFED18"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#1194f6"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#C90067"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FE7380"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#9E1CB2"))))
 '(term-bold ((t (:weight bold))))
 '(term-color-blue ((t (:background "dodger blue" :foreground "dodger blue")))))
