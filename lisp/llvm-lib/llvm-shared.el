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
(require 'my-llvm-mode)

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
   (target-clang-opts-fun :initarg :clang-opts-fun :type function :initform (lambda ()))
   (aux-props :initarg :aux-props :type list :initform nil)))

;; (defvar lls/llvm-config nil)

(defvar lls/llvm-configs (make-hash-table :test #'equal))

(defvar lls/target-init-fun nil)

(defun lls/get-active-configs ()
  (hash-table-values lls/llvm-configs))

(defun lls/get-llvm-config (&optional tab-name)
  (gethash (or tab-name (alist-get 'name (tab-bar--current-tab)))
           lls/llvm-configs))

(defun lls/set-llvm-config (conf &optional tab-name)
  (puthash (or tab-name (alist-get 'name (tab-bar--current-tab)))
           conf
           lls/llvm-configs))

(defun lls/conf-get (sym)
  (lls/ensure-initialized)
  (slot-value (lls/get-llvm-config) sym))

(defun lls/conf-get-safe (sym)
  (if-let ((conf (lls/get-llvm-config)))
      (slot-value conf sym)
    nil))

(defun lls/conf-aux-get (sym)
  (lls/ensure-initialized)
  (-->
   (lls/conf-get 'aux-props)
   (alist-get sym it)))

(defun lls/conf-set (key val)
  (lls/ensure-initialized)
  (setf (slot-value (lls/get-llvm-config) key)
        val))

(defun lls/initialize ()
  (interactive)
  (lls/set-llvm-config
   (or
    (let ((active-conf (lls/get-active-configs)))
      (and (not (zerop (length active-conf)))
           (y-or-n-p "Would you like to reuse a configuration? ")
           (let ((tab-name
                  (completing-read "Which tab's configuration would you like to reuse? "
                                   (->>
                                    (tab-bar-tabs)
                                    (mapcar #'(lambda (x) (alist-get 'name x)))
                                    (remove-if-not #'(lambda (x) (lls/get-llvm-config x)))))))
             (lls/get-llvm-config tab-name))))
    (aprog1 (funcall lls/target-init-fun)
      (setf (slot-value it 'build-dirs)
            (let ((r (rx (or "RelWithAsserts" "Release"))))
              (sort (slot-value it 'build-dirs)
                    #'(lambda (x y)
                        (cond ((string-match-p r y) nil)
                              ((string-match-p r x) t)
                              (t (string< x y))))))))))
  (load-llvm-mode (lls/conf-get 'root-dir))
  (message "llvm-lib initialize!"))

(defun lls/ensure-initialized ()
  (when (or (not (lls/get-llvm-config))
            (not (llvm-config-p (lls/get-llvm-config))))
    (if (not (functionp lls/target-init-fun))
        (error "Please register an init function for llvm")
      (lls/initialize))))

(defun lls/get-cached-value (key fun)
  (or (lls/conf-get key)
      (lls/conf-set key (funcall fun))))

(require 'projectile)

(defun projectile-dont-switch-when-conf-available (x)
  (if-let ((dir (lls/conf-get-safe 'root-dir))
           (tools-dir
            (progn
              (string-match (rx line-start
                                "/scratch"
                                (group "/benson/tools" (+ (not "/")) "/"))
                            dir)
              (match-string 1 dir))))
      (remove-if #'(lambda (path)
                     (and (string-match-p (rx "/benson/tools")
                                          path)
                          (not (string-match-p tools-dir path))))
                 x)
    x))

(advice-add 'projectile-relevant-known-projects
            :filter-return
            #'projectile-dont-switch-when-conf-available)


;;===---------------------------------------------------------------------===;;

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
  (lls/conf-set 'build-dirs
                (cons dir
                      (lls/conf-get 'build-dirs))))

(defun lls/get-clang-options ()
  (lls/get-cached-value 'target-clang-opts (lls/conf-get 'target-clang-opts-fun)))

(defun lls/swap-clang-options ()
  (interactive)
  (lls/conf-set 'target-clang-opts
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
  "~/workspace/llvm-project.git/machine-outliner")

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
