;;; light-default-theme.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-07 14:00]

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

(deftheme light-default)

(custom-theme-set-faces
 'light-default

 '(org-todo ((t :foreground "Red1" :bold t)))
 
 `(rainbow-delimiters-depth-1-face ((t :foreground "#000000")))
 `(rainbow-delimiters-depth-2-face ((t :foreground "#a8007f")))
 `(rainbow-delimiters-depth-3-face ((t :foreground "#005f88")))
 `(rainbow-delimiters-depth-4-face ((t :foreground "#904200")))
 `(rainbow-delimiters-depth-5-face ((t :foreground "#7f10d0")))
 `(rainbow-delimiters-depth-6-face ((t :foreground "#006800")))
 `(rainbow-delimiters-depth-7-face ((t :foreground "#b60000")))
 `(rainbow-delimiters-depth-8-face ((t :foreground "#1f1fce")))

 '(opr/STUFF-todo-face ((t :foreground "goldenrod")))
 '(opr/FUTURE-todo-face ((t :foreground "medium spring green")))
 '(opr/NEXT-todo-face ((t :foreground "cyan")))
 '(opr/WAIT-todo-face ((t :foreground "orange")))

 '(opr/ONE-todo-face ((t :foreground "royal blue")))

 '(opr/CLOCK-todo-face ((t :foreground "dark gray")))
 '(opr/INACT-todo-face ((t :foreground "dark gray")))
 '(opr/BACKLOG-todo-face ((t :foreground "dark gray")))
 '(opr/BLOCKED-todo-face ((t :foreground "dark gray")))
 '(opr/ABANDON-todo-face ((t :foreground "dark gray")))

 '(opr/META-todo-face ((t :foreground "black" :background "medium spring green")))
 '(opr/SEQ-todo-face ((t :foreground "black" :background "medium spring green")))
 '(opr/EMPTY-todo-face ((t :foreground "black")))
 '(opr/HOLD-todo-face ((t :foreground "red")))
 )

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'light-default)
;;; light-default-theme.el ends here
