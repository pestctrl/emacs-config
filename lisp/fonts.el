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


(defvar frame-font-size-cache
  (make-hash-table))

;; Font size adjustment
(defun hoagie-adjust-font-size (&optional frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
  If I let Windows handle DPI everything looks blurry."
  (interactive)
  ;; Using display names is unreliable...switched to checking the resolution
  (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
         (monitor-name (cdr (assoc 'name attrs)))
         (width-mm (second (assoc 'mm-size attrs)))
         (width-px (fourth (assoc 'geometry attrs)))
         (height-px (fifth (assoc 'geometry attrs)))
         (size 10)) ;; default for first screen at work
    (when (eq height-px 2880)
      (let ((f (selected-frame)))
        (set-face-attribute 'default f :height 130)
        (set-face-attribute 'mode-line f :height 130)
        (set-face-attribute 'mode-line-inactive f :height 130)
        (setq doom-modeline-height 30))
      (exwm-randr-refresh))
    ;; (unless (and (gethash frame frame-font-size-cache)
    ;;              (= size (gethash frame frame-font-size-cache)))
    ;;   (puthash frame size frame-font-size-cache))
    ))
;; (remove-hook 'window-size-change-functions #'hoagie-adjust-font-size)

(when my/at-ti
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

(provide 'fonts)
;;; fonts.el ends here
