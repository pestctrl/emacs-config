;;; same-defaults-theme.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-07 15:36]

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

(deftheme same-defaults)

(defvar my/fixed-pitch-height 100)
(defvar my/variable-pitch-height 130)

(custom-theme-set-faces
 'same-defaults
 `(default ((t (:family "Roboto Mono" :height ,my/fixed-pitch-height
                        ))))
 `(fixed-pitch ((t (:family "Roboto Mono" :height ,my/fixed-pitch-height
                            ))))
 `(variable-pitch ((t (:family "Linux Libertine" :height ,my/variable-pitch-height
                               ))))
 `(mode-line ((t (:family "Roboto Mono" :height ,my/fixed-pitch-height))))
 `(mode-line-inactive ((t (:family "Roboto Mono" :height ,my/fixed-pitch-height))))
 ;; '(org-todo ((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t :inherit fixed-pitch))
 ;;             (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t :inherit fixed-pitch))
 ;;             (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t :inherit fixed-pitch))
 ;;             (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t :inherit fixed-pitch))
 ;;             (t (:inverse-video t :bold t :inherit fixed-pitch))))
 )

;; (when (member "Roboto Mono" (font-family-list))
;;   (custom-theme-set-faces
;;    'same-defaults
;;    `(default ((t (:family "RobotoMono" :height ,my/fixed-pitch-height))))
;;    `(fixed-pitch ((t (:family "Roboto Mono" :height ,my/fixed-pitch-height))))))

;; (when (member "Linux Libertine" (font-family-list))
;;   (custom-theme-set-faces
;;    'same-defaults
;;    `(variable-pitch ((t (:family "Linux Libertine" :height ,my/variable-pitch-height :width normal))))))

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'same-defaults)
;;; same-defaults-theme.el ends here
