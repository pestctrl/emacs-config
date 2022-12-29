;;; my-comp-minor-mode.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:17]

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
(require 'anaphora)

(define-minor-mode compilation-minor-mode
  "Toggle Compilation minor mode.

When Compilation minor mode is enabled, all the error-parsing
commands of Compilation major mode are available.  See
`compilation-mode'."
  :lighter " Compilation" :keymap compilation-mode-map
  (if compilation-minor-mode
      (compilation-setup t)
    (compilation--unsetup)))

(defun my/enable-comp-keys-if-separate-mode (orig &rest args)
  (aprog1 (apply orig args)
    (when (cadr args)
      (with-current-buffer it
        (compilation-minor-mode)))))

(advice-add #'compilation-start
            :around
            #'my/enable-comp-keys-if-separate-mode)

(provide 'my-comp-minor-mode)
;;; my-comp-minor-mode.el ends here
