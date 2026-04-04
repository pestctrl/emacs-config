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
(require 'load-llvm-mode)
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

(defun comp-dev/get-config (&optional tab-name)
  (let ((tab-name (or tab-name (alist-get 'name (tab-bar--current-tab)))))
    (gethash tab-name comp-dev/configs)))

(defvar comp-dev/configs (make-hash-table :test #'equal))

(defvar lls/target-init-fun nil)

(defun lls/get-active-configs ()
  (hash-table-values comp-dev/configs))

(defun lls/set-llvm-config (conf &optional tab-name)
  (puthash (or tab-name (alist-get 'name (tab-bar--current-tab)))
           conf
           comp-dev/configs))

;; (defvar lls/llvm-config nil)

(defun lls/conf-get (sym)
  (comp-dev/ensure-initialized)
  (slot-value (comp-dev/get-config) sym))

(defun lls/conf-get-safe (sym)
  (if-let ((conf (comp-dev/get-config)))
      (slot-value conf sym)
    nil))

(defun lls/conf-aux-get (sym)
  (comp-dev/ensure-initialized)
  (-->
   (lls/conf-get 'aux-props)
   (alist-get sym it)))

(defun lls/conf-set (key val)
  (comp-dev/ensure-initialized)
  (setf (slot-value (comp-dev/get-config) key)
        val))

(defun lls/default-initialize ()
  (interactive)
  (let ((lls/target-init-fun #'lls/default-target-init))
    (comp-dev/initialize)))

(defun comp-dev/initialize ()
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
                                    (remove-if-not #'(lambda (x) (comp-dev/get-config x)))))))
             (comp-dev/get-config tab-name))))
    (funcall lls/target-init-fun)))
  (load-llvm-mode (lls/conf-get 'root-dir))
  (message "comp-dev initialize!"))

(defun comp-dev/initialized? ()
  (and (comp-dev/get-config)
       (typep (comp-dev/get-config) 'comp-dev-config)))

(defun comp-dev/ensure-initialized ()
  (when (not (comp-dev/initialized?))
    (if (not (functionp lls/target-init-fun))
        (error "Please register an init function for llvm")
      (comp-dev/initialize))))

(defun lls/get-cached-value (key fun)
  (or (lls/conf-get key)
      (lls/conf-set key (funcall fun))))

;;===---------------------------------------------------------------------===;;

(defun lls/get-llvm-root-dir ()
  (comp-dev/ensure-initialized)
  (lls/conf-get 'root-dir))

(defun lls/get-llvm-build-dirs ()
  (comp-dev/ensure-initialized)
  (funcall (lls/conf-get 'build-dirs-fun)))

(defun lls/get-llvm-bin-dir ()
  (car (lls/get-llvm-bin-dirs)))

(defun lls/get-llvm-bin-dirs ()
  (comp-dev/ensure-initialized)
  (append (mapcar #'(lambda (x) (expand-file-name "bin" x))
                  (lls/get-llvm-build-dirs))
          (funcall (lls/conf-get 'bin-dirs-fun))))

(defun lls/prompt-llvm-build-dir ()
  (completing-read "Which directory? " (lls/get-llvm-build-dirs)))

(defun lls/get-llvm-build-dir ()
  (car (lls/get-llvm-build-dirs)))

(defun lls/add-llvm-build-dir (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (comp-dev/ensure-initialized)
  (lls/conf-set 'build-dirs
                (cons dir
                      (lls/conf-get 'build-dirs))))

;; =============================== Misc ==============================

(defun my/completing-read (prompt collection &optional initial-input)
  (let ((len (length collection)))
    (cond ((< len 1)
           (user-error "Uhhh, no %ss? " prompt))
          ((= len 1) (car collection))
          (t (completing-read (format "Which %s? " prompt)
                              collection nil nil initial-input)))))

(defun lls/prompt-tool (tool-regexp &optional directories)
  (let (;;(vertico-sort-function nil)
        )
    (my/completing-read tool-regexp
                        (lls/get-tool tool-regexp
                                      (or (and (eq 'string (type-of directories))
                                               (list directories))
                                          directories))
                        (awhen (ti/current-tools-directory)
                          (concat (file-name-nondirectory it)
                                  " ")))))

(defun lls/get-tool (tool-regexp &optional directories)
  (cl-mapcan #'(lambda (dir)
                 (when (file-exists-p dir)
                   (when (string-match-p "/sim/sds11.*" dir)
                     (message "Checking %s..." dir))
                   (directory-files dir t tool-regexp)))
             (or directories
                 (comp-dev/get-bin-dirs (comp-dev/get-config)))))

(defun lls/get-clang-command-fun (&rest args)
  (apply (lls/conf-get 'compile-command-fun)
         args))

(defun lls/get-llc-command-fun (&rest args)
  (apply (lls/conf-get 'llc-command-fun) args))

(defun lls/get-dis-command-fun (&rest args)
  (apply (lls/conf-get 'dis-command-fun) args))

;; ========================= LLVM Build Dirs =========================
(cl-defun lls/default-comp-fun (&key compiler file action output flags)
  (-->
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
         flags
         "-o -"
         (or (and output
                  (format "| tee %s" output))
             ""))
   (flatten-list it)
   (string-join it " ")))

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
  (interactive)
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
  (if (-->
       (shell-command-to-string "git remote get-url origin")
       (string-trim it)
       (string-match-p ".*llvm.*" it))
      (vc-root-dir)
    (--> (directory-files "~/workspace" t "^[^.]")
         (remove-if-not #'(lambda (it)
                            (let ((default-directory it))
                              (-->
                               (shell-command-to-string "git remote get-url origin")
                               (string-trim it)
                               (member it repo-remotes))))
                        it)
         (completing-read "Repo directory? " it))))

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

(defun lls/cmake-here (directory build-type target)
  (interactive
   (list (expand-file-name "llvm"
                           (projectile-project-root))
         (completing-read "Build Type? "
                          '("Release"
                            "Debug"
                            "RelWithDebInfo"))
         (lls/conf-get 'target)))
  (let* ((comm-temp
          "cmake %s -G Ninja -DCMAKE_BUILD_TYPE='%s' -DLLVM_ENABLE_PROJECTS='clang;llvm' -DLLVM_TARGETS_TO_BUILD='%s' -DCMAKE_C_COMPILER='clang' -DCMAKE_CXX_COMPILER='clang++' -DLLVM_ENABLE_ASSERTIONS=On")
         (command (read-string
                   "CMake Command? "
                   (format comm-temp directory build-type target))))
    (compile command)))

(provide 'lib-comp-dev)
;;; lib-comp-dev.el ends here
