;;; clang-command.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-17 11:27]

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
(require 'clang-option-sets)

(defvar clang-subtargets
  (make-hash-table :test #'equal))

(defvar clang-options-extensions
  (make-hash-table :test #'equal))

(defvar cc/all-target-options
  (make-hash-table))

(defvar cc/current-target-optionset
  (make-hash-table))

(defun cc/push-new-target-option (target name option-set)
  (puthash target
           (cons (cons name option-set)
                 (gethash target cc/all-target-options))
           cc/all-target-options))

(defun cc/add-and-set-target-option (target name option-set)
  (puthash target name cc/current-target-optionset)
  (cc/push-new-target-option target name option-set))

(defvar cc/file-specific-options
  (make-hash-table :test #'equal))

(defun my/completing-read-formatter (formatter prompt list)
  (let ((alist
         (mapcar #'(lambda (x)
                     (cons (funcall formatter x)
                           x))
                 list)))
    (alist-get (completing-read prompt (mapcar #'car alist))
               alist
               nil nil #'equal)))

(defun cc/make-clang-option-set (target)
  (let ((subtargets (hash-table-values clang-subtargets))
        primary extensions)
    (setq primary
          (my/completing-read-formatter
           #'cos/clang-options->string
           "Primary Subtarget? "
           (remove-if-not (lambda (x)
                            (string= target
                                     (slot-value x 'target-str)))
                          subtargets)))
    (cons primary extensions)))

(defun cc/get-named-target-clang-optionset (target option-name)
  (when-let ((target-options
              (gethash target cc/all-target-options)))
    (and target-options
         (alist-get option-name target-options nil nil #'equal))))

(defun cc/reinitialize-clang-options (target)
  (interactive (list (intern (lls/conf-get 'target))))
  (let ((option-name (or (gethash target cc/current-target-optionset)
                         (puthash target "default" cc/current-target-optionset))))
    (aprog1 (cc/make-clang-option-set target)
      (cc/push-new-target-option target option-name it))))

;;; THE function
(defun cc/get-clang-options-for-target (target &optional option-name)
  (let ((option-name (or option-name
                         (gethash target cc/current-target-optionset)
                         (puthash target "default" cc/current-target-optionset))))
    (or (gethash (buffer-file-name) cc/file-specific-options)
        (cc/get-named-target-clang-optionset target option-name)
        (aprog1 (cc/make-clang-option-set target)
          (cc/push-new-target-option target option-name it)))))

;; (cc/get-clang-options-for-target "c29")

(defun cc/new-clang-option-set (target name &optional optionset)
  (interactive
   (list (intern (lls/conf-get 'target))
         (read-string "Name for new optionset? ")))
  (let ((optionset (or optionset (cc/make-clang-option-set target))))
    (cc/add-and-set-target-option target name optionset)))

(defun cc/copy-clang-option-set (target name &optional optionset)
  (interactive
   (list (intern (lls/conf-get 'target))
         (read-string "Name for new optionset? ")))
  (let ((optionset (cc/get-clang-options-for-target target)))
    (cc/add-and-set-target-option target name optionset)))

(defun cc/file-specific-option-set (target &optional optionset)
  (interactive
   (list (intern (lls/conf-get 'target))))
  ;; TODO: might need to garbage collect these
  (or (gethash (buffer-file-name)
               cc/file-specific-options)
      (let ((optionset (cc/get-clang-options-for-target target)))
        (puthash (buffer-file-name)
                 optionset
                 cc/file-specific-options))))

;; (cc/new-clang-option-set "c29" "toyota")

(defun cc/switch-clang-option-set (target name)
  (interactive
   (let ((target (lls/conf-get 'target)))
     (list target
           (--> (intern target)
                (gethash it cc/all-target-options)
                (mapcar #'car it)
                (completing-read "Optionset? " it)))))
  (remhash (buffer-file-name)
           cc/file-specific-options)
  (cc/get-clang-options-for-target target name)
  (puthash (intern target) name cc/current-target-optionset))

(defun cc/edit-clang-options (prefix)
  (interactive "P")
  (let* ((target (intern (lls/conf-get 'target)))
         (options-config
          (->>
           target
           (cc/get-clang-options-for-target)
           (car)))
         (current-name
          (or (and (gethash (buffer-file-name) cc/file-specific-options )
                   "file-specific")
              (gethash target cc/current-target-optionset))))
    (dolist (slot (cddr (eieio-class-slots 'clang-option-config)))
      (let* ((slot-sym (eieio-slot-descriptor-name slot))
             (slot-val (and (slot-boundp options-config slot-sym)
                            (slot-value options-config slot-sym))))
        (when slot-val
          (pcase (cl--slot-descriptor-type slot)
            ('list
             (when (or prefix
                       (not (zerop (length slot-val))))
               (setf (slot-value options-config slot-sym)
                     (read
                      (read-string (format "Edit '%s' for optionset '%s': "
                                           (symbol-name slot-sym)
                                           current-name)
                                   (prin1-to-string slot-val))))))
            ('string
             (when (or prefix
                       (not (string= slot-val "")))
               (setf (slot-value options-config slot-sym)
                     (read-string (format "Edit '%s' for optionset '%s': "
                                          (symbol-name slot-sym)
                                          current-name)
                                  slot-val))))))))))

(defvar cc/detect-extensions-function nil)

(cl-defun cc/get-clang-options (&key filename compiler action)
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (target-str (lls/conf-get 'target))
         (target (intern target-str))
         (options-config
          (cc/get-clang-options-for-target target))
         (detected-extensions
          (awhen cc/detect-extensions-function
            (funcall it compiler action filename target-str))))
    (cos/clang-options->string
     (cos/clang-options-merge
      (car options-config)
      (append detected-extensions (cdr options-config))))))

(provide 'clang-command)
;;; clang-command.el ends here
