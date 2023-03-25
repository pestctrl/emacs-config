;;; llvm-shared.el ---  -*- lexical-binding: t -*-

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

;; =========================== LLVM Rebuild ==========================

(defvar llvm-core-count 8)

(defun lls/ninja-build-tools (build-dir tools-list &optional verbose)
  (format "ninja -C %s -j %d %s %s"
          build-dir llvm-core-count
          (if verbose "-v" "")
          (string-join tools-list " ")))

;; =============================== Init ==============================

(defclass llvm-config ()
  ((root-dir :initarg :root-dir :type string)
   (build-dirs :initarg :build-dirs :type list)
   (target :initarg :target :type string)
   (bin-dirs :initarg :bin-dirs :type list :initform nil)
   (compile-command-fun :initarg :cc :type function :initform (lambda ()))
   (dis-command-fun :initarg :dc :type function :initform (lambda ()))
   (llc-command-fun :initarg :llc :type function :initform (lambda ()))
   ;; Cached compilation command options
   (target-clang-opts :initarg :clang-opts :initform nil)
   ;; Target + CPU -> compilation command options
   (target-clang-opts-fun :initarg :clang-opts-fun :type function :initform (lambda ()))))

(defvar lls/llvm-config nil)

(defvar lls/target-init-fun nil)

(defun lls/conf-get (sym)
  (slot-value lls/llvm-config sym))

(defun lls/initialize ()
  (interactive)
  (setq lls/llvm-config
        (funcall lls/target-init-fun))
  (setf (slot-value lls/llvm-config
                    'build-dirs)
        (let ((r (rx (or "RelWithAsserts" "Release"))))
          (sort (lls/conf-get 'build-dirs)
                #'(lambda (x y)
                    (cond ((string-match-p r y) nil)
                          ((string-match-p r x) t)
                          (t (string< x y)))))))
  (message "llvm-lib initialize!"))

(defun lls/ensure-initialized ()
  (when (or (not lls/llvm-config)
            (not (llvm-config-p lls/llvm-config)))
    (if (not (functionp lls/target-init-fun))
        (error "Please register an init function for llvm")
      (lls/initialize))))

(defun lls/get-llvm-root-dir ()
  (lls/ensure-initialized)
  (lls/conf-get 'root-dir))

(defun lls/get-llvm-build-dirs ()
  (lls/ensure-initialized)
  (lls/conf-get 'build-dirs))

(defun lls/get-llvm-bin-dir ()
  (car (lls/get-llvm-bin-dirs)))

(defun lls/get-llvm-bin-dirs ()
  (lls/ensure-initialized)
  (append (mapcar #'(lambda (x) (expand-file-name "bin" x))
                  (lls/get-llvm-build-dirs))
          (lls/conf-get 'bin-dirs)))

(defun lls/get-llvm-build-dir ()
  (car (lls/get-llvm-build-dirs)))

(defun lls/add-llvm-build-dir (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (lls/ensure-initialized)
  (setf (slot-value lls/llvm-config 'build-dirs)
        (cons dir
              (lls/conf-get 'build-dirs))))

(defun lls/get-clang-options ()
  (or (lls/conf-get 'target-clang-opts)
      (funcall (lls/conf-get 'target-clang-opts-fun))))

;; =============================== Misc ==============================

(defun my/completing-read (prompt collection)
  (let ((len (length collection)))
    (cond ((< len 1)
           (user-error "Uhhh, no %ss? " prompt))
          ((= len 1) (car collection))
          (t (completing-read (format "Which %s? " prompt)
                              collection)))))

(defun lls/prompt-tool (tool-regexp &optional directories)
  (my/completing-read tool-regexp
                      (lls/get-tool tool-regexp
                                    (or (and (eq 'string (type-of directories))
                                             (list directories))
                                        directories))))

(defun lls/get-tool (tool-regexp &optional directories)
  (cl-mapcan #'(lambda (dir)
                 (when (file-exists-p dir)
                   (directory-files dir t tool-regexp)))
             (or directories
                 (lls/get-llvm-bin-dirs))))

(defun lls/get-clang-command-fun (&rest args)
  (apply (lls/conf-get 'compile-command-fun)
         args))

(defun lls/get-llc-command-fun (&rest args)
  (apply (lls/conf-get 'llc-command-fun) args))

(defun lls/get-dis-command-fun (&rest args)
  (apply (lls/conf-get 'dis-command-fun) args))

;; ========================= LLVM Build Dirs =========================
(defun lls/default-clang-opts ()
  (let ((target (lls/conf-get 'target)))
    (concat "--target="
            (pcase target
              ("X86" "x86_64")
              ("ARM" "arm")))))

(cl-defun lls/default-comp-fun (compiler file action &key output rest)
  (string-join
   (list compiler
         (lls/get-clang-options)
         (string-join rest " ")
         file
         (pcase action
           ('compile "-c")
           ('assemble "-S")
           ('preprocess "-E")
           ('llvm-ir "-S -emit-llvm")
           ('executable ""))
         (format "-o %s"
                 (or output
                     (and (eq action 'executable) "a.out")
                     "-")))
   " "))

(defun lls/default-llc-comm (file _action)
  (concat "llc -o - "
          file " "))

(defun lls/default-dis-comm (file _action)
  (concat "llvm-objdump --disassemble "
          file " "))

(setq lls/target-init-fun
      ;; TODO: load llvm-mode
      (lambda ()
        (make-instance
         'llvm-config
         :root-dir (lls/guess-root-dir-fun)
         :build-dirs (lls/guess-build-dirs-fun)
         :target (completing-read "Which target? " '("X86" "ARM"))
         :bin-dirs '("/usr/bin/")
         :cc #'lls/default-comp-fun
         :dc #'lls/default-dis-comm
         :llc #'lls/default-llc-comm
         :clang-opts-fun #'lls/default-clang-opts)))

(defun lls/guess-root-dir-fun ()
  ;; TODO: constant
  "~/workspace/llvm-project")

(defun lls/guess-build-dirs-fun ()
  (when-let ((toplevel ;;(magit-toplevel (buffer-file-name (current-buffer)))
              (lls/guess-root-dir-fun)))
    (and (string-match-p "llvm-project" toplevel)
         (let ((build-dir (expand-file-name "build" toplevel)))
           (when (file-exists-p build-dir)
             (--> build-dir
                  (directory-files it t)
                  (remove-if-not #'(lambda (dir)
                                     (file-exists-p
                                      (expand-file-name "build.ninja" dir)))
                                 it)
                  (sort it #'(lambda (x y)
                               (cond ((string-match-p "^Release$" (file-name-nondirectory y)) nil)
                                     ((string-match-p "^Release$" (file-name-nondirectory x)) t)
                                     (t (string< x y)))))))))))

(provide 'llvm-shared)
;;; llvm-shared.el ends here
