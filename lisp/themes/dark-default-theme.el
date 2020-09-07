;;; dark-default.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-02 15:06]

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

(deftheme dark-default)

(let ((rainbow-purple  "#9E1CB2")
      (rainbow-green   "#47B04B")
      (rainbow-blue    "#1194f6")
      (rainbow-red     "#C90067")
      (rainbow-yellow  "#FFED18")
      (rainbow-orange  "#E7B500")
      (rainbow-7       "#00AA5D")
      (rainbow-8       "#FE7380"))
  (custom-theme-set-faces
   'dark-default
 
   '(default ((t (:foreground "#70FF00" :background "gray8"))))
   '(mode-line-inactive ((t :foreground "gray60" :background "#404045" :inverse-video nil)))
   '(mode-line     ((t :foreground "gray60" :background "black" :inverse-video nil)))
   '(eldoc-highlight-function-argument ((t :foreground "dodger blue")))

   `(rainbow-delimiters-depth-1-face ((t :foreground ,rainbow-purple)))
   `(rainbow-delimiters-depth-2-face ((t :foreground ,rainbow-green)))
   `(rainbow-delimiters-depth-3-face ((t :foreground ,rainbow-blue)))
   `(rainbow-delimiters-depth-4-face ((t :foreground ,rainbow-red)))
   `(rainbow-delimiters-depth-5-face ((t :foreground ,rainbow-yellow)))
   `(rainbow-delimiters-depth-6-face ((t :foreground ,rainbow-blue)))
   `(rainbow-delimiters-depth-7-face ((t :foreground ,rainbow-red)))
   `(rainbow-delimiters-depth-8-face ((t :foreground ,rainbow-8)))
   `(rainbow-delimiters-depth-9-face ((t :foreground ,rainbow-purple)))

  )

;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dark-default)
;;; dark-default-theme.el ends here
