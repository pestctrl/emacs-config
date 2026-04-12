;;; comp-mode-locus-override.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2026-04-12 10:41]

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

(defvar compile-locus-display-window nil)
(make-variable-buffer-local 'compile-locus-display-window)

(defun my/compile-locus-some-window (buffer)
  `(lambda (buffer alist)
     (if (and compile-locus-display-window
              (window-live-p compile-locus-display-window))
         compile-locus-display-window
       (setq
        compile-locus-display-window
        (let ((win (window-in-direction 'left (get-buffer-window ,buffer))))
          (split-window-horizontally nil win))))))

(defun my/compilation-goto-locus (orig &rest args)
  (save-selected-window
    (let ((display-buffer-overriding-action
           `(display-buffer-use-some-window
             (inhibit-same-window . t)
             (some-window . ,(my/compile-locus-some-window (current-buffer))))))
      (apply orig args))))

(advice-add #'compilation-goto-locus
            :around
            #'my/compilation-goto-locus)

(provide 'comp-mode-locus-override)
;;; comp-mode-locus-override.el ends here
