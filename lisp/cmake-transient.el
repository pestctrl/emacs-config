;;; cmake-transient.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-11-17 13:32]

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
(require 'transient)

(defun test-function ()
  (interactive)
  (message "Test function")
  (message "%s" (transient-args 'cmake-transient)))

(defun cmake-read-generator (prompt &optional initial-input history)
  (format "\"%s\""
          (completing-read
           prompt
           '("Ninja"
             "Unix Makefiles")
           nil nil initial-input history)))

;; (with-eval-after-load 'vertico-multiform
;;   (add-to-list 'vertico-multiform-commands
;;                '(cmake:-G flat (vertico-cycle . t))))

(transient-define-argument cmake:-G ()
  :description "Generator"
  :class 'transient-option
  :key "-G"
  :argument "-G "
  :reader #'cmake-read-generator)

(transient-define-prefix cmake-transient (repo-dir)
  "Test Transient Title"
  ["Arguments"
   (cmake:-G)]
  ["Actions"
   ("a" "Action a" test-function)
   ("s" "Action s" test-function)
   ("d" "Action d" test-function)]
  (interactive (list (projectile-project-root)))
  (transient-setup 'cmake-transient nil nil :scope repo-dir))

(put 'magit-log-mode 'magit-log-default-arguments
     '("--graph" "-n256" "--decorate"))

(global-set-key (kbd "C-c C") #'cmake-transient)

(provide 'cmake-transient)
;;; cmake-transient.el ends here
