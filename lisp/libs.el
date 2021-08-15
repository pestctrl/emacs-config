;;; Libraries that other parts of the config use

(use-package mmt)
(use-package f)
(use-package s)
(use-package dash)
(use-package memoize)
(use-package hydra)

;; I would like common lisp and the library of alexandria
(require 'cl)
(require 'subr-x)

;; This is helpful, for exwm-related loading
(require 'use-exwm)

(provide 'libs)
