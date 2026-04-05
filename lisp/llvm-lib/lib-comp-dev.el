;;; lib-comp-dev.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 19:31]

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

(require 'magit)
(require 'eieio)
(require 'my-clang-options)
(require 'my-nprocs)
(require 'use-package)
(use-package realgud-lldb)

;; =========================== Rebuild ==========================

(defvar comp-dev/default-parallelism
  (nprocs))

(defun comp-dev/build-target (build-dir targets &optional verbose)
  (let ((cmake-make-program
         (if (string= "Makefile" (car (directory-files build-dir nil
                                                       (rx line-start (or "build.ninja" "Makefile") line-end))))
             "make"
           "ninja")))
    (format "set -o pipefail && CLICOLOR_FORCE=1 %s -C %s -j %d %s %s 2>&1 | tee ninja.log"
            cmake-make-program
            build-dir comp-dev/default-parallelism
            (if verbose "-v" "")
            (string-join targets " "))))

;; =============================== Init ==============================

(defclass comp-dev-config ()
  ((root-dir :initarg :root-dir :type string)
   (target :initarg :target :type string)
   ;; (tramp-connection :initarg :tramp :type list :initform nil)

   (aux-props :initarg :aux-props :type list :initform nil)))

(cl-defgeneric comp-dev/get-bin-dirs (config))
(cl-defgeneric comp-dev/get-build-dirs (config))
(cl-defgeneric comp-dev/get-file-types (config))
(cl-defgeneric comp-dev/get-c-action-table (config))
(cl-defmethod comp-dev/get-c-action-table (config)
  nil)
(cl-defgeneric comp-dev/process-file (config start-type end-type compiler file output flags))
(cl-defgeneric comp-dev/tool-name (config tool))

;; =============================== Tabs ===============================

(defvar comp-dev/configs (make-hash-table :test #'equal))

(defun comp-dev/get-config (&optional tab-name)
  (let ((tab-name (or tab-name (alist-get 'name (tab-bar--current-tab)))))
    (gethash tab-name comp-dev/configs)))

(defun comp-dev/get-active-configs ()
  (hash-table-values comp-dev/configs))

(defvar comp-dev/init-functions nil)

(defun comp-dev/set-config (conf &optional tab-name)
  (puthash (or tab-name (alist-get 'name (tab-bar--current-tab)))
           conf
           comp-dev/configs))

(defun comp-dev/conf-set (key val)
  (comp-dev/ensure-initialized)
  (setf (slot-value (comp-dev/get-config) key)
        val))

;; ====== External ======
(defun comp-dev/initialize ()
  (interactive)
  (comp-dev/set-config
   (or
    (let ((active-conf (comp-dev/get-active-configs)))
      (and (not (zerop (length active-conf)))
           (y-or-n-p "Would you like to reuse a configuration? ")
           (let ((tab-name
                  (completing-read "Which tab's configuration would you like to reuse? "
                                   (->>
                                    (tab-bar-tabs)
                                    (mapcar #'(lambda (x) (alist-get 'name x)))
                                    (remove-if-not #'(lambda (x) (comp-dev/get-config x)))))))
             (comp-dev/get-config tab-name))))
    (funcall (intern
              (completing-read "Which initialization function would you like to use? "
                               comp-dev/init-functions)))))
  (message "comp-dev initialize!"))

(defun comp-dev/conf-get (sym)
  (comp-dev/ensure-initialized)
  (slot-value (comp-dev/get-config) sym))

(defun comp-dev/conf-get-safe (sym)
  (if-let ((conf (comp-dev/get-config)))
      (slot-value conf sym)
    nil))

(defun comp-dev/conf-aux-get (sym)
  (comp-dev/ensure-initialized)
  (-->
   (comp-dev/conf-get 'aux-props)
   (alist-get sym it)))

(defun comp-dev/initialized? ()
  (and (comp-dev/get-config)
       (typep (comp-dev/get-config) 'comp-dev-config)))

(defun comp-dev/ensure-initialized ()
  (when (not (comp-dev/initialized?))
    (if (not (functionp lls/target-init-fun))
        (error "Please register an init function for llvm")
      (comp-dev/initialize))))

;; (defun lls/tramp-connection ()
;;   (comp-dev/conf-get 'tramp-connection))

;; (defun lls/trampify (path)
;;   (if-let ((vec (lls/tramp-connection)))
;;       (tramp-make-tramp-file-name vec path)
;;     path))

;; (defun lls/un-trampify (path)
;;   (if-let ((vec (lls/tramp-connection)))
;;       (with-parsed-tramp-file-name path nil
;;         localname)
;;     path))

;; =============================== Misc ==============================

(defun my/completing-read (prompt collection &optional initial-input)
  (let ((len (length collection)))
    (cond ((< len 1)
           (user-error "Uhhh, no %ss? " prompt))
          ((= len 1) (car collection))
          (t (completing-read (format "Which %s? " prompt)
                              collection nil nil initial-input)))))

(defun comp-dev/get-tools (tool-regexp &optional directories)
  (cl-mapcan #'(lambda (dir)
                 (when (file-exists-p dir)
                   (when (string-match-p "/sim/sds11.*" dir)
                     (message "Checking %s..." dir))
                   (directory-files dir t tool-regexp)))
             (or directories
                 (comp-dev/get-bin-dirs (comp-dev/get-config)))))

(defun comp-dev/prompt-tool (tool-regexp &optional directories)
  (let (;;(vertico-sort-function nil)
        )
    (my/completing-read tool-regexp
                        (comp-dev/get-tools tool-regexp
                                      (or (and (eq 'string (type-of directories))
                                               (list directories)))))))

(provide 'lib-comp-dev)
;;; lib-comp-dev.el ends here
