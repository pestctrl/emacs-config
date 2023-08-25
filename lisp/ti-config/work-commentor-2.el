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
(defvar banner/styles nil)
(defvar banner/current-style nil)

(defclass banner-comment-style ()
  ((align :initarg :align :type symbol :initform 'left)
   (fill :initarg :fill :type character :initform (string-to-char " "))
   (empty-fill :initarg :empty-fill)
   (padding :initarg :padding :type string :initform "")
   (start :initarg :start :initform nil)
   (end :initarg :end :initform nil)
   (right-ornament :initarg :rorn :initform "")))

(cl-defun banner/new-style (sym &key (align 'left) (fill (string-to-char " ")) (padding "") start end empty rorn)
  (add-to-list 'banner/styles
               (cons sym
                     (make-instance
                      'banner-comment-style
                      :align align :fill fill :padding padding
                      :start start :end end :empty-fill (or empty fill) :rorn rorn))))

(banner/new-style 'llvm-start :padding "===" :fill ?-
                  :end '(string-reverse (string-trim comment-start))
                  :rorn "-*- C++ -*-")
(banner/new-style 'ti-box
                  :fill (string-to-char " ")
                  :empty ?- :end '(string-reverse comment-start))

(defun banner/change-alignment (align)
  (interactive (list (intern (completing-read "Alignment? " '(left right center)))))
  (setf (slot-value banner/current-style 'align)
        align))

;; (setq banner/current-style (cdr (car banner/styles)))

(defun banner/extract-comment-string (expr other)
  (when (or (not (null expr))
            (not (null other)))
    (cond ((null expr) (banner/extract-comment-string other nil))
          ((member (type-of expr) '(symbol cons))
           (banner/extract-comment-string (eval expr) other))
          ((eq 'string (type-of expr))
           (string-trim expr)))))

(defun my/banner-select-style (style)
  (interactive
   (list (intern (completing-read "Style? " (mapcar #'car banner/styles)))))
  (setq banner/current-style (alist-get style banner/styles)))

(defun my/construct-comment-string (string indent &optional style)
  (with-slots (align fill empty-fill padding start end) (or style banner/current-style)
    (let* ((comm-start (concat (banner/extract-comment-string start comment-start)
                               padding))
           (comm-end (concat (string-reverse padding)
                             (banner/extract-comment-string end comment-end)))
           (fill-column (- fill-column indent)))
      (concat
       (make-string indent ? )
       (if (string-empty-p string)
           (-->
            comm-start
            (string-pad it (- fill-column (length comm-end) indent) empty-fill)
            (concat it comm-end))
         (let* ((len (length (concat comm-start " " string " " comm-end))))
           (when (> len fill-column)
             (user-error "string too long"))
           (let* ((fill-left (- fill-column len))
                  left right)
             (pcase align
               ('center
                (setq left (/ fill-left 2)
                      right (- fill-left left)))
               ('left
                (setq left 0
                      right fill-left))
               ('right
                (setq right 0
                      left fill-left)))
             (concat
              comm-start
              " "
              (make-string left fill)
              string
              (make-string right fill)
              " "
              comm-end))))))))

(defun my/looking-at-comment (style)
  (interactive
   (list banner/current-style))
  (with-slots (align fill empty-fill padding start end) style
    (let* ((comm-start (banner/extract-comment-string start (string-trim comment-start)))
           (comm-end (banner/extract-comment-string end (string-trim comment-end)))
           (regexp
            (rx-to-string
             `(and
               line-start
               (group (* " "))
               (group
                ,comm-start
                ,padding)
               (group
                (* nonl))
               (group
                ,(string-reverse padding)
                ,comm-end)
               ))))
      (save-excursion
        (goto-char (point-at-bol))
        (aprog1 (and (looking-at regexp) (save-match-data (length (match-string 1))))
          (when (called-interactively-p 'interactive)
            (if it
                (message "Looking at comment! space: '%s', beg: '%s', mid: '%s', end: '%s'"
                         (match-string 1) (match-string 2) (match-string 3) (match-string 4))
              (message "Not looking at comment!"))))))))

(defun banner/parse-comment (style)
  (interactive (list banner/current-style))
  (with-slots (align fill empty-fill padding start end) style
    (save-match-data
      (save-excursion
        (goto-char (point-at-bol))
        (let ((res (my/looking-at-comment style)))
          (if (not res)
              (let ((string (buffer-substring (line-beginning-position) (line-end-position))))
                (string-match "^\\( *\\)\\(.*\\)$" string)
                (cons (length (match-string 1 string)) (match-string 2 string)))
            (cons res
                  (let ((mid (match-string 3)))
                    (cond
                     ((string-match-p (rx-to-string `(and line-start (+ ,empty-fill) line-end))
                                      mid)
                      "")
                     ((string-match (rx-to-string `(and line-start
                                                        (* ,empty-fill)
                                                        (group (+ (not (any "\n" ,empty-fill))))
                                                        (+ ,empty-fill)
                                                        line-end))
                                    mid)
                      (match-string 1 mid))
                     ((eq align 'center) (string-trim mid))
                     ((eq align 'left) (string-trim-right (substring mid 1 (length mid))))
                     ((eq align 'right) (string-trim-left (substring mid 0 (1- (length mid))))))))))))))

(defun my/banner-comment (style)
  (interactive
   (list banner/current-style))
  (let* ((res (banner/parse-comment style))
         (new-line (my/construct-comment-string (cdr res) (car res) style)))
    (let ((point (point)))
      (goto-char (line-beginning-position))
      (when (not (looking-at-p "^$"))
        (kill-region (line-beginning-position) (line-end-position)))
      (insert new-line)
      (goto-char point))))

(defun banner/create-llvm-banner (desc)
  (interactive
   (list (read-string "Description? ")))
  (let ((start
         (concat "//==--- "
                 (file-name-nondirectory (buffer-file-name))
                 " - " desc " "))
        (end
         (concat "-*- C++ -*-===//")))
    (insert (string-pad start (- fill-column (length end)) ?-)
            end
            "\n")))

(provide 'work-commentor)
;;; work-commentor.el ends here
