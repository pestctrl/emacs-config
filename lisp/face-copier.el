;;; face-copier.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-05 18:56]

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

(defalias 'def-face-copier
  'def-face-copier1)

(defmacro def-face-copier1 (name var body-pred &rest included)
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (name (intern (concat name "--face-set"))))
    `(defun ,name (frame &optional parameters)
       ;; The `reverse' is so that `default' goes first.
       (dolist (face (append ',included 
                             (remove-if-not 
                              (lambda ,var
                                ,body-pred)
                              (nreverse (face-list)))))
         (condition-case ()
             (progn
               ;; Initialize faces from face spec and custom theme.
               (face-spec-recalc face frame)
               ;; Apply attributes specified by face-new-frame-defaults
               (internal-merge-in-global-face face frame))
           ;; Don't let invalid specs prevent frame creation.
           (error nil)))

       ;; Apply attributes specified by frame parameters.
       (let ((face-params '((foreground-color default :foreground)
                            (background-color default :background)
                            (font default :font)
                            (border-color border :background)
                            (cursor-color cursor :background)
                            (scroll-bar-foreground scroll-bar :foreground)
                            (scroll-bar-background scroll-bar :background)
                            (mouse-color mouse :background))))
         (dolist (param face-params)
           (let* ((param-name (nth 0 param))
                  (value (cdr (assq param-name parameters))))
             (if value
                 (set-face-attribute (nth 1 param) frame
                                     (nth 2 param) value))))))))

(defmacro def-face-copier2 (name var body-pred &rest included)
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (name (intern (concat name "--face-recalc"))))
    (let ((sym (car var)))
      `(defun ,name (,sym frame)
         (when (or (member ,sym ',included)
                   ,body-pred)
           (while (get ,sym 'face-alias)
             (setq ,sym (get ,sym 'face-alias)))
           (face-spec-reset-face ,sym frame)
           ;; If FACE is customized or themed, set the custom spec from
           ;; `theme-face' records.
           (let ((theme-faces (get ,sym 'theme-face))
                 (no-match-found 0)
                 face-attrs theme-face-applied)
             (if theme-faces
                 (dolist (elt (reverse theme-faces))
                   (setq face-attrs (face-spec-choose (cadr elt) frame no-match-found))
                   (unless (eq face-attrs no-match-found)
                     (face-spec-set-2 ,sym frame face-attrs)
                     (setq theme-face-applied t))))
             ;; If there was a spec applicable to FRAME, that overrides the
             ;; defface spec entirely (rather than inheriting from it).  If
             ;; there was no spec applicable to FRAME, apply the defface spec
             ;; as well as any applicable X resources.
             (unless theme-face-applied
               (setq face-attrs (face-spec-choose (face-default-spec ,sym) frame))
               (face-spec-set-2 ,sym frame face-attrs)
               (make-face-x-resource-internal ,sym frame))
             (setq face-attrs (face-spec-choose (get ,sym 'face-override-spec) frame))
             (face-spec-set-2 ,sym frame face-attrs)))))))

(defmacro def-face-copier3 (name var body-pred &rest included)
  `(progn
     (def-face-copier1 ,name ,var ,body-pred &rest ,included)
     (def-face-copier2 ,name ,var ,body-pred &rest ,included)))

(defmacro override1-face (name &rest body)
  (declare 
           (indent 1))
  (let* ((name (symbol-name name))
         (face-set (intern (concat name "--face-set"))))
    `(cl-letf (((symbol-function 'face-set-after-frame-default)
                (symbol-function ',face-set)))
       ,@body)))

(defmacro override2-face (name &rest body)
  (declare 
           (indent 1))
  (let* ((name (symbol-name name))
         (face-recalc (intern (concat name "--face-recalc"))))
    `(cl-letf (((symbol-function 'face-spec-recalc)
                (symbol-function ',face-recalc)))
       ,@body)))

(defmacro override-face (name &rest body)
  (declare 
           (indent 1))
  `(override1-face ,name
     (override2-face ,name
       ,@body)))

(provide 'face-copier)
;;; face-copier.el ends here
