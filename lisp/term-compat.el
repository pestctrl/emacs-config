;;; term-compat.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-05-30 16:59]

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

(require 'term/xterm)

(let ((ascii-start 97)
      (C-M-start ?\C-\M-a))
  (dotimes (n 26)
    (define-key xterm-function-map
                (format "\e[27;5;%d~" (+ ascii-start n))
                (vector (1+ n)))
    (define-key xterm-function-map
                (format "\e[27;7;%d~" (+ ascii-start n))
                (vector (+ n C-M-start)))))

(define-key xterm-function-map
            "\e[27;5;8~"
            [C-backspace])

(define-key xterm-function-map
            "\e[27;5;32~"
            [?\C-\s] ;; Or (kbd "C-SPC") ;; Or [?\C- ]
            )

(define-key xterm-function-map
            "\e[27;2;32~"
            [?\S-\s])

;; (xterm--init-modify-other-keys)
(defun my/xterm--init-modify-other-keys ()
  "Terminal initialization for xterm's modifyOtherKeys support."
  (send-string-to-terminal "\e[>4;2m")
  (push "\e[>4m" (terminal-parameter nil 'tty-mode-reset-strings))
  (push "\e[>4;2m" (terminal-parameter nil 'tty-mode-set-strings)))

(advice-add #'xterm--init-modify-other-keys
            :override
            #'my/xterm--init-modify-other-keys)

(xterm-mouse-mode 1)
(terminal-init-xterm)

;; (global-set-key (kbd "M-[ emacs-C-SPC") #'set-mark-command)
;; (global-set-key (kbd "M-[ emacs-M-SPC") #'cycle-spacing)
;; (global-set-key (kbd "M-[ emacs-C-/") #'undo)
(global-set-key (kbd "M-[ emacs-C-<backspace>") #'backward-kill-word)

(with-eval-after-load 'org
  ;; (define-key org-mode-map (kbd "C-c M-[ emacs-C-,") #'org-insert-structure-template)
  ;; (define-key org-mode-map (kbd "M-[ emacs-C-<return>") #'org-insert-heading-respect-content)
  (define-key org-mode-map (kbd "M-[ emacs-M-S-<return>") #'org-insert-todo-heading)
  ;; (define-key org-mode-map (kbd "M-[ emacs-C-S-<return>") #'org-insert-todo-heading-respect-content)
  )

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-[ emacs-C-<backspace>")
              #'(lambda () (interactive) (vterm-send-key "w" nil nil t))))

(provide 'term-compat)
;;; term-compat.el ends here
