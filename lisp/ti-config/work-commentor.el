;;; work-commentor.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-15 16:13]

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
(global-set-key (kbd "C-c h") #'my/banner-comment)

(defun work-banner-switch-char ()
  (interactive)
  (setq work-banner-comment-char
        (if (= banner-comment-char ?-)
            ?*
          ?-)))

(defvar banner/styles nil)

(defvar banner/current-style nil)

(defclass banner-comment-style ()
  ((align :initarg :align :type symbol :initform 'left)
   (fill :initarg :fill :type character :initform (string-to-char " "))
   (empty-fill :initarg :empty-fill)
   (padding :initarg :padding :type string :initform "")
   (start :initarg :start :initform nil)
   (end :initarg :end :initform nil)))

(cl-defun banner/new-style (sym &key (align 'left) (fill (string-to-char " ")) (padding "") start end empty)
  (add-to-list 'banner/styles
               (cons sym
                     (make-instance
                      'banner-comment-style
                      :align align :fill fill :padding padding
                      :start start :end end :empty-fill (or empty fill)))))

(banner/new-style 'llvm-start :padding "===" :fill ?- :end 'comment-start)
(banner/new-style 'ti-box
                  :fill (string-to-char " ")
                  :empty ?- :end 'comment-start)

(defun banner/change-alignment (align)
  (interactive (list (intern (completing-read "Alignment? " '(left right center)))))
  (setf (slot-value banner/current-style 'align)
        align))

;; (setq banner/current-style (cdr (car banner/styles)))            ;;

(defun banner/extract-comment-string (expr)
  (cond ((null expr) (banner/extract-comment-string comment-start))
        ((eq 'symbol (type-of expr))
         (banner/extract-comment-string (eval expr)))
        ((eq 'string (type-of expr))
         (string-trim expr))))

(defun my/banner-comment (arg style)
  (interactive
   (list
    current-prefix-arg
    (or banner/current-style
        (cdr (car banner/styles)))))
  (with-slots (align fill empty-fill padding start end) style
    (let* ((comm-start (banner/extract-comment-string start))
           (comm-end (banner/extract-comment-string end))
           (regexp
            (rx-to-string
             `(and
               line-start
               (group (* " "))
               (group
                (optional
                 ,comm-start
                 ,padding
                 (or (+ ,(string empty-fill)) (+ ,(string fill)))
                 (* " ")))
               (group
                (*? (not ,fill)))
               (group
                (optional
                 (* " ")
                 (or (+ ,(string empty-fill)) (+ ,(string fill)))
                 ,(string-reverse padding)
                 ,comm-end))
               line-end))))
      (save-excursion
        (goto-char (point-at-bol))
        (when (or (looking-at regexp)
                  ;; (looking-at (rx line-start
                  ;;                 (group (* " "))
                  ;;                 (group)
                  ;;                 (group (+ nonl))
                  ;;                 (group (* " "))
                  ;;                 line-end))
                  ;; (looking-at (rx line-start
                  ;;                 (group (* " "))
                  ;;                 (group) (group) (group)
                  ;;                 line-end))
                  )
          (let* ((empty-fill (or (and arg fill)
                                 empty-fill))
                 (len (length (match-string 3)))
                 (space-pad (and (not (= len 0))
                                 (not (eq (string-to-char " ") fill))))
                 (fill-left (- fill-column len
                               (length (match-string 1))
                               (if space-pad 2 0)
                               (* 2 (length padding))
                               (length comm-start)
                               (length comm-end)))
                 left right)
            (pcase align
              ('center
               (setq left (/ fill-left 2)
                     right (- fill-left left)))
              ('left
               (setq left 1
                     right (1- fill-left)))
              ('right
               (setq right 1
                     left (1- fill-left))))
            (replace-match (concat comm-start padding
                                   (make-string left (or (and (= len 0) empty-fill)
                                                         fill))
                                   (when space-pad " "))
                           nil nil nil 2)

            (when (and padding comm-end)
              (replace-match (concat (when space-pad " ")
                                     (make-string right (or (and (= len 0) empty-fill)
                                                            fill))
                                     (string-reverse padding) comm-end)
                             nil nil nil 4))))))))
;;------------------------------------------------------------------;;
;; Hello                                                            ;;

(provide 'work-commentor)
;;; work-commentor.el ends here
