;;; Libraries that other parts of the config use

(use-package mmt)
(use-package f)
(use-package s)
(use-package memoize)
(use-package hydra)

;; I would like common lisp and the library of alexandria
(use-package dash)
(use-package anaphora)
(require 'cl-lib)
(require 'subr-x)

;; This is helpful, for exwm-related loading
(require 'use-exwm)

;; Register my own rx forms
(require 'my-rx-forms)

(defmacro ef (path)
  `(expand-file-name ,path user-emacs-directory))

(provide 'libs)
