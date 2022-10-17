;;; work-config.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-07-30 14:07]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'my-org-misc)
(require 'work-org-stuff)
(require 'work-commentor)
(require 'work-asm-config)
(require 'ti-lib)
(require 'ti-build-tool)
(require 'ti-debug-compile)

;; (setq url-proxy-services '(("http" . "http://webproxy.ext.ti.com:80")
;;                            ("https" . "http://webproxy.ext.ti.com:80")))

(global-display-fill-column-indicator-mode t)

(setq-default fill-column 79)

;; Prompt for passwords via the minibuffer
(setq epg-pinentry-mode nil)

(add-to-list 'display-buffer-alist
             '("\\*xref\\*" display-buffer-in-side-window
               (side . top) (slot . 0) (window-height . fit-window-to-buffer)
               (preserve-size . (nil . t)) ,parameters))

(add-to-list 'auto-mode-alist '("\\.mir$" . llvm-mode))

(when (executable-find "rg")
  (use-package deadgrep
    :config
    (setq deadgrep-project-root-function
          #'(lambda ()
              (if current-prefix-arg
                  default-directory
                (deadgrep--project-root))))))

(defmacro load-file? (fname)
  `(when (file-exists-p ,fname)
     (load-file ,fname)))

(load-file? (expand-file-name
             "lisp/work-config/secrets/tools-manipulation.el"
             user-emacs-directory))
(load-file? (expand-file-name
             "lisp/work-config/secrets/update_environment.el"
             user-emacs-directory))

(load-file? "/scratch/benson/tools2/llvm_cgt/llvm-project/llvm/utils/emacs/llvm-mode.el")
(load-file? "/scratch/benson/tools2/llvm_cgt/llvm-project/llvm/utils/emacs/tablegen-mode.el")
(load-file? "/scratch/benson/tools2/llvm_cgt/llvm-project/llvm/utils/emacs/emacs.el")

(add-to-list 'auto-mode-alist
             '("\\.dsls$" . json-mode))

(add-to-list 'auto-mode-alist '("\\.test" . tcl-mode))

(add-to-list 'auto-mode-alist '("\\.gel" . c-mode))

(pop c-mode-common-hook)
(add-hook 'c-mode-common-hook
	  (function
	   (lambda nil
	     (if (and buffer-file-name (string-match "llvm" buffer-file-name))
		 (progn
		   (c-set-style "llvm.org"))))))

(define-key *root-map* (kbd "u")
  (lambda ()
    (interactive)
    (insert comment-start)
    (just-one-space)
    (insert "BENSON_UPDATE_THIS: ")
    (save-excursion
      (insert comment-end)
      (indent-for-comment))))

(provide 'work-config)
;;; work-config.el ends here
