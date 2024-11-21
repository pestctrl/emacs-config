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
(require 'load-llvm-mode)
(require 'my-clang-options)

;; =========================== LLVM Rebuild ==========================

(defvar llvm-core-count 8)

(defun lls/ninja-build-tools (build-dir tools-list &optional verbose)
  (format "set -o pipefail && CLICOLOR_FORCE=1 ninja -C %s -j %d %s %s 2>&1 | tee ninja.log"
          build-dir llvm-core-count
          (if verbose "-v" "")
          (string-join tools-list " ")))

;; =============================== Init ==============================

(defclass llvm-config ()
  ((root-dir :initarg :root-dir :type string)
   (bin-dirs-fun :initarg :bin-dirs-fun :type function)
   (build-dirs-fun :initarg :build-dirs-fun :type function)
   (build-release-dir :initarg :build-release-dir :type string)
   (build-debug-dir :initarg :build-debug-dir :type string)
   (target :initarg :target :type string)
   (compile-command-fun :initarg :cc :type function :initform (lambda ()))
   (dis-command-fun :initarg :dc :type function :initform (lambda ()))
   (llc-command-fun :initarg :llc :type function :initform (lambda ()))
   (tramp-connection :initarg :tramp :type list :initform nil)
   ;; Target + CPU -> compilation command options
   (target-clang-opts-fun :initarg :clang-opts-fun :type function :initform (lambda ()))
   (aux-props :initarg :aux-props :type list :initform nil)))

;; (defvar lls/llvm-config nil)

(defvar lls/llvm-configs (make-hash-table :test #'equal))

(defvar lls/target-init-fun nil)

(defun lls/get-active-configs ()
  (hash-table-values lls/llvm-configs))

(defun lls/get-llvm-config (&optional tab-name)
  (let ((tab-name (or tab-name (alist-get 'name (tab-bar--current-tab)))))
    (gethash tab-name lls/llvm-configs)))

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

(defun lls/tramp-connection ()
  (lls/conf-get 'tramp-connection))

(defun lls/trampify (path)
  (if-let ((vec (lls/tramp-connection)))
      (tramp-make-tramp-file-name vec path)
    path))

(defun lls/un-trampify (path)
  (if-let ((vec (lls/tramp-connection)))
      (with-parsed-tramp-file-name path nil
        localname)
    path))

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
    (funcall lls/target-init-fun)))
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
            (when (string-match (rx line-start
                                    "/scratch"
                                    (group "/benson/_repos-work/tools" (* (not "/")) "/"))
                                dir)
              (match-string 1 dir))))
      (remove-if #'(lambda (path)
                     (and (string-match-p (rx "/benson/_repos-work/tools")
                                          path)
                          (not (string-match-p tools-dir path))))
                 x)
    x))

(when my-ec/at-ti
  (advice-add 'projectile-relevant-known-projects
              :filter-return
              #'projectile-dont-switch-when-conf-available))


;;===---------------------------------------------------------------------===;;

(defun lls/get-llvm-root-dir ()
  (lls/ensure-initialized)
  (lls/conf-get 'root-dir))

(defun lls/get-llvm-build-dirs ()
  (lls/ensure-initialized)
  (funcall (lls/conf-get 'build-dirs-fun)))

(defun lls/get-llvm-bin-dir ()
  (car (lls/get-llvm-bin-dirs)))

(defun lls/get-llvm-bin-dirs ()
  (lls/ensure-initialized)
  (append (mapcar #'(lambda (x) (expand-file-name "bin" x))
                  (lls/get-llvm-build-dirs))
          (funcall (lls/conf-get 'bin-dirs-fun))))

(defun lls/get-llvm-build-dir ()
  (car (lls/get-llvm-build-dirs)))

(defun lls/add-llvm-build-dir (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (lls/ensure-initialized)
  (lls/conf-set 'build-dirs
                (cons dir
                      (lls/conf-get 'build-dirs))))

(defun lls/get-clang-options (&rest args)
  (apply (lls/conf-get 'target-clang-opts-fun)
         args))

;; =============================== Misc ==============================

(defun my/completing-read (prompt collection)
  (let ((len (length collection)))
    (cond ((< len 1)
           (user-error "Uhhh, no %ss? " prompt))
          ((= len 1) (car collection))
          (t (completing-read (format "Which %s? " prompt)
                              collection)))))

(defun lls/prompt-tool (tool-regexp &optional directories)
  (lls/un-trampify
   (my/completing-read tool-regexp
                       (lls/get-tool tool-regexp
                                     (or (and (eq 'string (type-of directories))
                                              (list directories))
                                         directories)))))

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
(cl-defun lls/default-comp-fun (&key compiler file action output rest)
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
         "-o -"
         (or (and output
                  (format "| tee %s" output))
             ""))
   " "))

(defun ll/read-pass ()
  (completing-read "Which pass? "
                   '("finalize-isel"
                     "machine-scheduler"
                     "greedy")))

(cl-defun lls/default-llc-comm (&key llc file action output pass)
  (let ((llc (or llc "llc")))
    (concat
     (cond
      ((member action '(run-pass stop-before stop-after start-before start-after))
       (format "%s %s -o - -%s=%s"
               llc file (symbol-name action) pass))
      (t
       (read-string
        "llc invocation: "
        (string-join
         (list
          llc
          file
          "-o -")
         " "))))
     (or (and output
              (format " | tee %s" output))
         ""))))

(defun lls/default-dis-comm (file _action)
  (concat "llvm-objdump --disassemble "
          file " "))

(defun lls/lldb (binary)
  (interactive
   (list (lls/prompt-tool (rx (or "clang" "llc") line-end))))
  (realgud--lldb
   (format "lldb %s"
           binary)))

(defun lls/default-target-init ()
  (let ((root-dir (lls/guess-root-dir-fun))
        tramp-conn)
    (when (tramp-tramp-file-p root-dir)
      (with-parsed-tramp-file-name root-dir nil
        (setf tramp-conn v)))
    (make-instance
     'llvm-config
     :tramp tramp-conn
     :root-dir root-dir
     :build-dirs-fun (lls/guess-build-dirs-fun root-dir)
     :target (completing-read "Which target? " '("X86" "ARM" "Hexagon" "AIE" "RISCV"))
     :bin-dirs-fun (lambda ()
                     (list
                      (--> "/usr/bin/"
                           (if (not tramp-conn) it
                             (tramp-make-tramp-file-name tramp-conn it)))))
     :cc #'lls/default-comp-fun
     :dc #'lls/default-dis-comm
     :llc #'lls/default-llc-comm
     :clang-opts-fun #'cc/get-clang-options)))

(when (not lls/target-init-fun)
  (setq lls/target-init-fun
        ;; TODO: load llvm-mode
        #'lls/default-target-init))

(defun lls/guess-root-dir-fun ()
  (let ((repo-remotes
         '("https://github.com/llvm/llvm-project.git"
           "git@github.com:llvm/llvm-project"
           "https://github.com/MPACT-ORG/llvm-project"
           "https://github.com/Xilinx/llvm-aie")))
    (if (-->
         (shell-command-to-string "git remote get-url origin")
         (string-trim it)
         (member it repo-remotes))
        (vc-root-dir)
      (--> (directory-files "~/workspace" t "^[^.]")
           (remove-if-not #'(lambda (it)
                              (let ((default-directory it))
                                (-->
                                 (shell-command-to-string "git remote get-url origin")
                                 (string-trim it)
                                 (member it repo-remotes))))
                          it)
           (completing-read "Repo directory? " it)))))

(defun lls/guess-build-dirs-fun (root-dir)
  (lambda ()
    (let ((build-dir (expand-file-name "build" root-dir)))
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
                                (t (string< x y))))))))))

(provide 'llvm-shared)
;;; llvm-shared.el ends here
