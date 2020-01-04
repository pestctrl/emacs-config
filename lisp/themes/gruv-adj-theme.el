;;; gruv-adj-theme.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-02 15:32]

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

(deftheme gruv-adj)

(custom-theme-set-faces
 'gruv-adj

 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(org-scheduled-previously ((t (:foreground "chocolate1"))))
 '(org-upcoming-deadline ((t (:foreground "chocolate1"))))
 '(org-warning ((t (:foreground "chocolate1"))))
 '(org-scheduled-today ((t (:foreground "PaleGreen"))))
 '(org-todo ((t (:foreground "Pink" :weight bold)))))

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gruv-adj)
;;; gruv-adj-theme.el ends here
