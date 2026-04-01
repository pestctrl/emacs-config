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
(require 'compiler-option-sets)

(defclass clang-option-config (compiler-option-config)
  nil)

(defvar clang-subtargets
  (make-hash-table :test #'equal))

(defvar clang-options-extensions
  (make-hash-table :test #'equal))

(defvar clang/all-target-options
  (make-hash-table))

(defvar clang/current-target-optionset
  (make-hash-table))

(defun clang/push-new-target-option (target name option-set)
  (puthash target
           (cons (cons name option-set)
                 (gethash target clang/all-target-options))
           clang/all-target-options))

(defun clang/add-and-set-target-option (target name option-set)
  (puthash target name clang/current-target-optionset)
  (clang/push-new-target-option target name option-set))

(defvar clang/file-specific-options
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

(defun clang/make-clang-option-set (target)
  (let ((subtargets (hash-table-values clang-subtargets))
        primary extensions)
    (setq primary
          (my/completing-read-formatter
           #'clang/clang-options->string
           "Primary Subtarget? "
           (remove-if-not (lambda (x)
                            (string= target
                                     (slot-value x 'target-str)))
                          subtargets)))
    (cons primary extensions)))

(defun clang/get-named-target-clang-optionset (target option-name)
  (when-let ((target-options
              (gethash target clang/all-target-options)))
    (and target-options
         (alist-get option-name target-options nil nil #'equal))))

(defun clang/reinitialize-clang-options (target)
  (interactive (list (intern (lls/conf-get 'target))))
  (let ((option-name (or (gethash target clang/current-target-optionset)
                         (puthash target "default" clang/current-target-optionset))))
    (aprog1 (clang/make-clang-option-set target)
      (clang/push-new-target-option target option-name it))))

;;; THE function
(defun clang/get-clang-options-for-target (target &optional option-name)
  (let ((option-name (or option-name
                         (gethash target clang/current-target-optionset)
                         (puthash target "default" clang/current-target-optionset))))
    (or (gethash (buffer-file-name) clang/file-specific-options)
        (clang/get-named-target-clang-optionset target option-name)
        (aprog1 (clang/make-clang-option-set target)
          (clang/push-new-target-option target option-name it)))))

;; (clang/get-clang-options-for-target "c29")

;; (defun clang/new-clang-option-set (target name &optional optionset)
;;   (interactive
;;    (list (intern (lls/conf-get 'target))
;;          (read-string "Name for new optionset? ")))
;;   (let ((optionset (or optionset (clang/make-clang-option-set target))))
;;     (clang/add-and-set-target-option target name optionset)))

;; (defun clang/copy-clang-option-set (target name &optional optionset)
;;   (interactive
;;    (list (intern (lls/conf-get 'target))
;;          (read-string "Name for new optionset? ")))
;;   (let ((optionset (clang/get-clang-options-for-target target)))
;;     (clang/add-and-set-target-option target name optionset)))

;; (defun clang/file-specific-option-set (target &optional optionset)
;;   (interactive
;;    (list (intern (lls/conf-get 'target))))
;;   ;; TODO: might need to garbage collect these
;;   (or (gethash (buffer-file-name)
;;                clang/file-specific-options)
;;       (let ((optionset (clang/get-clang-options-for-target target)))
;;         (puthash (buffer-file-name)
;;                  optionset
;;                  clang/file-specific-options))))

;; (clang/new-clang-option-set "c29" "toyota")

(defun clang/switch-clang-option-set (target name)
  (interactive
   (let ((target lls/conf-get 'target))
     (list target
           (--> (intern target)
                (gethash it clang/all-target-options)
                (mapcar #'car it)
                (completing-read "Optionset? " it)))))
  (remhash (buffer-file-name)
           clang/file-specific-options)
  (clang/get-clang-options-for-target target name)
  (puthash (intern target) name clang/current-target-optionset))

(defun clang/edit-clang-options (prefix)
  (interactive "P")
  (let* ((target (intern (lls/conf-get 'target)))
         (options-config
          (->>
           target
           (clang/get-clang-options-for-target)
           (car)))
         (current-name
          (or (and (gethash (buffer-file-name) clang/file-specific-options )
                   "file-specific")
              (gethash target clang/current-target-optionset))))
    (cos/edit-compiler-options options-config current-name)))

(defun clang/clang-options-merge (primary secondary)
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

(defun clang/clang-options->string (opts)
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

(defvar clang/detect-extensions-function nil)

(cl-defun clang/get-clang-options (&key filename compiler action)
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (target-str (lls/conf-get 'target))
         (target (intern target-str))
         (options-config
          (clang/get-clang-options-for-target target))
         (detected-extensions
          (awhen clang/detect-extensions-function
            (funcall it compiler action filename target-str))))
    (clang/clang-options->string
     (clang/clang-options-merge
      (car options-config)
      (append detected-extensions (cdr options-config))))))

(provide 'clang-command)
;;; clang-command.el ends here
