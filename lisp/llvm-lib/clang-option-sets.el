;;; clang-option-sets.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-05-05 15:13]

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

(defclass clang-option-config ()
  ((target-str          :initarg :target-str   :type string :initform "")
   (binary-path         :initarg :binary       :type string :initform "")
   (target-options      :initarg :target       :type string :initform "")
   (lang-options        :initarg :lang         :type string :initform "")
   (other-options       :initarg :other        :type string :initform "")
   (optimization-level  :initarg :optimization :type string :initform "")
   (include-dirs        :initarg :include-dirs :type list   :initform nil)
   (system-include-dirs :initarg :isystem      :type list   :initform nil)))

(defmacro register-prebaked-optionset (hashmap target-str key &rest options)
  (declare (indent 3))
  `(puthash ',key
            (make-instance 'clang-option-config
                           :target-str ,target-str
                           ,@options)
            ,hashmap))

(defun cos/clang-options-merge (primary secondary)
  (make-instance
   'clang-option-config
   :target-str     (slot-value primary 'target-str)
   :binary         (or (slot-value primary 'binary-path)
                       (car (mapcar #'(lambda (x) (slot-value x 'binary-path))) ))
   :target         (slot-value primary 'target-options)
   :lang           (slot-value primary 'lang-options)
   :optimization   (slot-value primary 'optimization-level)
   :other          (-->
                    (cons primary secondary)
                    (mapconcat
                     (lambda (x)
                       (when (slot-boundp x 'other-options)
                         (slot-value x 'other-options)))
                     it
                     " "))
   :include-dirs   (-->
                    (cons primary secondary)
                    (apply #'append
                           (mapcar
                            (lambda (x)
                              (when (slot-boundp x 'include-dirs)
                                (slot-value x 'include-dirs)))
                            it)))))

(defun cos/clang-options->string (opts)
  (with-slots
      (binary-path
       target-options lang-options
       other-options optimization-level
       include-dirs)
      opts
    (-->
     (list
      (or binary-path "")
      target-options
      lang-options
      optimization-level
      other-options
      (mapconcat (lambda (x)
                   (format "-I\"%s\"" x))
                 include-dirs
                 " "))
     (remove-if #'string-empty-p it)
     (string-join it " "))))

(provide 'clang-option-sets)
;;; clang-option-sets.el ends here
