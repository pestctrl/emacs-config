;;; Libraries that other parts of the config use

(use-package mmt)
(use-package f)
(use-package s)
(use-package dash)
(use-package mmt)
(use-package memoize)
(use-package hydra)

(require 'cl)

;;; These are utilities that I have written.

(require 'org-loop)
(require 'org-process)
(require 'org-project)
(require 'org-delay)
(require 'clojure-swap)

;; Buggy overall, not needed cause of workgroups2.el
;; (require 'exwm-workspace-counsel)

;; New stuff, may not stick around
(require 'org-ql-custom-stuck-projects)

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

(provide 'libs)
