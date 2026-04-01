;;; compiler-option-sets.el ---  -*- lexical-binding: t -*-

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

(defclass compiler-option-config ()
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
            (make-instance 'compiler-option-config
                           :target-str ,target-str
                           ,@options)
            ,hashmap))

(defun cos/edit-compiler-options (optionset current-name)
  (dolist (slot (cddr (eieio-class-slots 'compiler-option-config)))
    (let* ((slot-sym (eieio-slot-descriptor-name slot))
           (slot-val (and (slot-boundp optionsset slot-sym)
                          (slot-value optionsset slot-sym))))
      (when slot-val
        (pcase (cl--slot-descriptor-type slot)
          ('list
           (when (or prefix
                     (not (zerop (length slot-val))))
             (setf (slot-value optionsset slot-sym)
                   (read
                    (read-string (format "Edit '%s' for optionset '%s': "
                                         (symbol-name slot-sym)
                                         current-name)
                                 (prin1-to-string slot-val))))))
          ('string
           (when (or prefix
                     (not (string= slot-val "")))
             (setf (slot-value optionsset slot-sym)
                   (read-string (format "Edit '%s' for optionset '%s': "
                                        (symbol-name slot-sym)
                                        current-name)
                                slot-val)))))))))

(provide 'compiler-option-sets)
;;; compiler-option-sets.el ends here
