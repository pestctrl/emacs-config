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
(require 'ti-keymap)
(require 'ti-lib)
(require 'ti-tools-backup)
(require 'argo-fastsim-dump-mode)
(require 'machine-scheduler-debug-mode)
(require 'frame-restore)
(require 'ti-config)

(add-to-list 'auto-mode-alist '("\\.map$" . ti-linker-map-mode))
(add-to-list 'auto-mode-alist '("\\.cdis$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.fsdump$" . argo-fastsim-dump-mode))

(use-package ein
  :config
  (setq request-curl-options '("--noproxy \"*\""))
  (setq ein:jupyter-server-command "~/.local/bin/jupyter"))

(use-package deadgrep)

(use-package cmake-mode
  :config
  (defun my/cmake-jump-to-definiton (sym)
    (interactive
     (list (symbol-name (symbol-at-point))))
    (rgrep (concat "function(" sym)
           "*.cmake CMakeLists.txt"
           (projectile-acquire-root)))

  (define-key cmake-mode-map (kbd "M-.") #'my/cmake-jump-to-definiton))

(global-display-fill-column-indicator-mode t)

(setq-default fill-column 79)

;; Prompt for passwords via the minibuffer
(setq epg-pinentry-mode nil)

(add-to-list 'display-buffer-alist
             '("\\*xref\\*" display-buffer-in-side-window
               (side . top) (slot . 0) (window-height . fit-window-to-buffer)
               (preserve-size . (nil . t)) ,parameters))

(add-to-list 'auto-mode-alist '("\\.mir$" . llvm-mode))

(when-let (exe (executable-find "rg"))
  (use-package deadgrep
    :config
    (setq deadgrep-executable exe)
    (setq deadgrep-project-root-function
          #'(lambda ()
              (if current-prefix-arg
                  default-directory
                (deadgrep--project-root))))))

(defmacro load-file? (fname)
  `(when (file-exists-p ,fname)
     (load-file ,fname)))

;; (pop llvm-font-lock-keywords)

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

(require 'llvm-jump-to-tablegen)
(require 'my-tablegen-mode)

(define-key tablegen-mode-map (kbd "M-.") #'ll/jump-to-tablegen)

;; (when (string-match-p ".*NATIVE_COMP.*" system-configuration-features)
;;   (defun replace-path-for-async-native-comp (orig &rest args)
;;     (let ((path (getenv "PATH")))
;;       (setenv "PATH" (concat "/db/sds/packages2/emacs-master/bin:" path))
;;       (apply orig args)))

;;   (defun replace-path-for-native-comp (orig &rest args)
;;     (let ((path (getenv "PATH")))
;;       (setenv "PATH" (concat "/db/sds/packages2/emacs-master/bin:" path))
;;       (apply orig args)
;;       (setenv "PATH" path)))

;;   (advice-add #'native-compile
;;               :around
;;               #'replace-path-for-native-comp)

;;   (advice-add #'native-compile-async
;;               :around
;;               #'replace-path-for-async-native-comp))

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "~/bin/plantuml"
        plantuml-default-exec-mode 'jar))

(use-package realgud
  :config
  (setq realgud-window-split-orientation 'horizontal))

(use-package realgud-lldb)

(provide 'work-config)
;;; work-config.el ends here
