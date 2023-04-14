;;; fonts.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-04-14 06:17]

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

(create-fontset-from-fontset-spec
 "-*-Roboto Mono-regular-normal-normal-*-13-*-*-*-m-0-fontset-mine")

(set-face-font 'default "fontset-default")

(set-fontset-font "fontset-default" 'ascii (font-spec :size 13 :name "RobotoMono"))
(set-fontset-font "fontset-default" 'latin (font-spec :size 13 :name "RobotoMono"))

;; HanWangKaiMediumChuIn
;; (set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "HanWangMingMediumChuIn"))
;; (set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "HanWangKaiMediumChuIn-20"))

(set-fontset-font "fontset-default" 'unicode (font-spec :size 10 :name "FontAwesome"))
(set-fontset-font "fontset-default" 'cyrillic (font-spec :size 15 :name "PT Sans Expert"))

;; extra/adobe-source-code-pro-fonts
(set-fontset-font "fontset-default" 'unicode (font-spec :name "SourceCodePro"))
(set-fontset-font "fontset-default"
                  (cons
                   (decode-char 'ucs #x2500)
                   (decode-char 'ucs #x257F))
                  (font-spec :name "SourceCodePro"))

;; adobe-source-han-sans-otc-fonts
(set-fontset-font "fontset-default" 'han (font-spec :size 16 :name "SourceHanSans"))

;; (custom-set-faces
;;  '(default ((t (:family "ETBookOT" :foundry "QUQA"
;;                :slant normal :weight normal :height 120
;;                :width normal :spacing 90)))))

;;(set-face-attribute 'variable-pitch nil :font '(:family "ETBookOT"))

(provide 'fonts)
;;; fonts.el ends here
