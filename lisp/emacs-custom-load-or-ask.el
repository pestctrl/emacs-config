;;; emacs-custom-load-or-ask.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-14 16:33]

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
(require 'mmt)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defvar ec/my-variables-list '())

(defmacro ec/load-or-ask-pred (sym prompt)
  `(when (not (boundp ',sym))
     (customize-save-variable ',sym (y-or-n-p ,prompt))))

(defmacro ec/load-or-ask-key (key key-key prompt)
  (mmt-with-gensyms (keygen)
    `(when (not (boundp ',key))
       (let ((,keygen (read-key ,prompt)))
         (customize-save-variable ',key-key ,keygen)
         (customize-save-variable ',key (char-to-string ,keygen))))))

(defun ec/re-run-all-prompts ())

(provide 'emacs-custom-load-or-ask)
;;; emacs-custom-load-or-ask.el ends here
