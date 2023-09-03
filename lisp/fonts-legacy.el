;;; fonts-legacy.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-09-03 14:44]

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

;; (defmacro my/english-fontspec (name size &rest extra-attr)
;;   `(font-spec :name ,name :size ,size ,@extra-attr))

;; (defmacro my/roboto-mono (&rest extra-attr)
;;   `(my/english-fontspec "Roboto Mono" 13 ,@extra-attr))

;; (set-face-attribute 'default nil :font (my/roboto-mono))
;; (set-face-font 'bold (my/roboto-mono :weight 'bold))
;; (set-face-font 'italic (my/roboto-mono :slant 'italic))
;; (set-face-font 'bold-italic (my/roboto-mono :weight 'bold :slant 'italic))

;; (set-fontset-font "fontset-default" 'cyrillic (font-spec :size 15 :name "PT Sans Expert"))
;; (set-fontset-font "fontset-default" 'unicode (font-spec :size 10 :name "FontAwesome"))

;; (set-fontset-font "fontset-default" 'unicode (font-spec :name "SourceCodePro"))
;; (set-fontset-font "fontset-default"
;;                   (cons
;;                    (decode-char 'ucs #x2500)
;;                    (decode-char 'ucs #x257F))
;;                   (font-spec :name "SourceCodePro"))

;; HanWangKaiMediumChuIn
;; (set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "HanWangMingMediumChuIn"))
;; (set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "HanWangKaiMediumChuIn-20"))

;; extra/adobe-source-code-pro-fonts

;; adobe-source-han-sans-otc-fonts
;; (set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "Source Han Sans TW"))

;; (set-frame-font "fontset-default")

;; You can see a difference by switching between default and
;; fontset-standard.
;;
;; (set-face-font 'default "fontset-standard")

;; (custom-set-faces
;;  '(default ((t (:family "ETBookOT" :foundry "QUQA"
;;                :slant normal :weight normal :height 120
;;                :width normal :spacing 90)))))

;;(set-face-attribute 'variable-pitch nil :font '(:family "ETBookOT"))

(provide 'fonts-legacy)
;;; fonts-legacy.el ends here
