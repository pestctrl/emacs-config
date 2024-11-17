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
(require 'hoagie-adjust)
(require 'emacs-custom-load-or-ask)

;; If I reeaaaally wanted to do this property, I would use
;; #'new-fontset, but that requires LOTS of background knowledge. Just
;; look at the #'setup-default-fontset function.
;;
;; For now, I will just use the default fontset.

;; (create-fontset-from-fontset-spec
;;  "-*-*-medium-r-normal-*-15-*-*-*-*-*-fontset-default")

(ec/load-or-ask-num 'my-font-size "What would you like your font size to be (10-16)? ")

(set-fontset-font "fontset-default" 'ascii (font-spec :size my-font-size :name "Roboto Mono"))

;; 三寶飯
;; "HanWangMingMediumChuIn"
(let ((cf (font-spec :name "SourceHanSansTW"
                     :size 18)))
  (dolist (charset '(kana han cjk-misc bopomofo gb18030))
    (set-fontset-font "fontset-default" charset cf)))

(if my-ec/at-ti
    (progn
      (set-fontset-font "fontset-default"
                        (cons
                         (decode-char 'ucs #x2500)
                         (decode-char 'ucs #x257F))
                        (font-spec :name "Latin Modern Math" :size 12))
      (set-fontset-font "fontset-default"
                        (cons
                         (decode-char 'ucs #x2997)
                         (decode-char 'ucs #x2997))
                        (font-spec :size 8 :avgwidth 8 :name "STIXGeneral" :weight 'normal)))
  (set-fontset-font "fontset-default"
                    (cons
                     (decode-char 'ucs #x2500)
                     (decode-char 'ucs #x25CF))
                    (font-spec :name "DejaVu Sans Mono" :size 14))

  (set-fontset-font "fontset-default"
                    (cons
                     (decode-char 'ucs #x2191)
                     (decode-char 'ucs #x2191))
                    (font-spec :name "DejaVu Sans Mono" :size 12))

  (set-fontset-font "fontset-default"
                    (cons
                     (decode-char 'ucs #x26A1)
                     (decode-char 'ucs #x26A1))
                    (font-spec :name "Symbols Nerd Font Mono" :size 16)))

(when my-ec/at-ti
  (set-fontset-font "fontset-default" 'ascii (font-spec :size 11 :name "RobotoMono"))
  (set-fontset-font "fontset-default" 'latin (font-spec :size 11 :name "RobotoMono"))
  (set-face-attribute 'mode-line nil :font "RobotoMono-8")
  (set-face-attribute 'mode-line-inactive nil :font "RobotoMono-8")
  (setq doom-modeline-height 20))

(set-face-font 'default "fontset-default")

;; (setq doom-modeline-height 20) ; optional
;; (if (facep 'mode-line-active)
;;     (set-face-attribute 'mode-line-active nil :family "Roboto Mono" :height 100) ; For 29+
;;   (set-face-attribute 'mode-line nil :family "Roboto Mono" :height 100))
;; (set-face-attribute 'mode-line-inactive nil :family "Roboto Mono" :height 100)

(provide 'fonts)
;;; fonts.el ends here
