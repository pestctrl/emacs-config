;;; llvm-comp-dev.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2026-04-04 15:16]

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
(require 'lib-comp-dev)
(require 'load-llvm-mode)
(require 'clang-command)

(defclass llvm-comp-dev-config (comp-dev-config)
  nil)

(cl-defmethod comp-dev/tool-name ((config llvm-comp-dev-config) tool)
  (pcase tool
    ('compiler "clang$")))

(defun lls/guess-root-dir-fun ()
  (if (-->
       (shell-command-to-string "git remote get-url origin")
       (string-trim it)
       (string-match-p ".*llvm.*" it))
      (projectile-project-root)
    (--> (directory-files "~/workspace" t "^[^.]")
         (remove-if-not #'(lambda (it)
                            (let ((default-directory it))
                              (-->
                               (shell-command-to-string "git remote get-url origin")
                               (string-trim it)
                               (member it repo-remotes))))
                        it)
         (completing-read "Repo directory? " it))))

(defun llvm/default-target-init ()
  (interactive)
  (let ((root-dir (lls/guess-root-dir-fun))
        tramp-conn)
    (when (tramp-tramp-file-p root-dir)
      (with-parsed-tramp-file-name root-dir nil
        (setf tramp-conn v)))
    (load-llvm-mode root-dir)
    (make-instance
     'llvm-comp-dev-config
     :root-dir root-dir
     :target (completing-read "Which target? " '("X86" "ARM" "Hexagon" "AIE" "RISCV"))

     ;; :tramp tramp-conn
     ;; :build-dirs-fun (lls/guess-build-dirs-fun root-dir)
     ;; :cc #'lls/default-comp-fun
     ;; :dc #'lls/default-dis-comm
     ;; :llc #'lls/default-llc-comm
     ;; :clang-opts-fun #'cc/get-clang-options
     )))

(add-to-list 'comp-dev/init-functions
             'llvm/default-target-init)

(defun lls/llvm-build-dirs (root-dir)
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
                              (t (string< x y)))))))))

(defun lls/llvm-bin-dirs (root-dir)
  (mapcar #'(lambda (dir)
              (expand-file-name "bin" dir))
          (lls/llvm-build-dirs root-dir)))

(cl-defmethod comp-dev/get-bin-dirs ((config llvm-comp-dev-config))
  (cons
   "/usr/bin/"
   (lls/llvm-bin-dirs (comp-dev/conf-get 'root-dir))))

(cl-defmethod comp-dev/get-file-types ((config llvm-comp-dev-config))
  '(c pp-c llvm-ir asm obj exe))

(cl-defmethod comp-dev/get-c-action-table ((config llvm-comp-dev-config))
  '((assembly     :key ?a :major-mode asm-mode  :buffer-string "assembly"           :description "[a]ssembly"               :end-state asm)
    (debug        :key ?d :major-mode llvm-mode :buffer-string "debug"              :description "[d]ebug pass"             :end-state asm)
    (LLVMIR       :key ?l :major-mode llvm-mode :buffer-string "llvm-ir"            :description "[l]lvm-ir"                :end-state llvm-ir)
    (before-after :key ?p :major-mode llvm-mode :buffer-string "print-before-after" :description "[p]rint before/after"     :end-state asm)
    (changed      :key ?P :major-mode llvm-mode :buffer-string "print-changed"      :description "[P]rint before/after all" :end-state asm)))

(cl-defmethod comp-dev/tool-name ((config llvm-comp-dev-config) type)
  (pcase type
    ('compiler "clang")))

;; (output-dis   :key ?A    :major-mode asm-mode  :buffer-string "dissasembly"        :description "output-dis[A]ssemble"     :end-state nil)

(cl-defmethod comp-dev/process-file ((config llvm-comp-dev-config) start-type end-type tool file output flags)
  (pcase start-type
    ('c
     (-->
      (list tool
            (clang/get-clang-options)
            file
            flags
            (pcase end-type
              ('pp-c "-E")
              ('llvm-ir "-S -emit-llvm")
              ('asm "-S")
              ('obj "-c")
              ('exe (error "unimplemented")))
            "-o -"
            (or (and output
                     (format "| tee %s" output))
                ""))
      (flatten-list it)
      (string-join it " ")))
    ('llvm-ir
     (-->
      (list tool file "-o -")
      (string-join it " ")))))

(defun lls/get-llvm-root-dir ()
  (comp-dev/ensure-initialized)
  (comp-dev/conf-get 'root-dir))

(defun lls/get-llvm-build-dirs ()
  (comp-dev/ensure-initialized)
  (funcall (comp-dev/conf-get 'build-dirs-fun)))

(defun lls/get-llvm-bin-dir ()
  (car (lls/get-llvm-bin-dirs)))

(defun lls/get-llvm-bin-dirs ()
  (comp-dev/ensure-initialized)
  (append (mapcar #'(lambda (x) (expand-file-name "bin" x))
                  (lls/get-llvm-build-dirs))
          (funcall (comp-dev/conf-get 'bin-dirs-fun))))

(defun lls/prompt-llvm-build-dir ()
  (completing-read "Which directory? " (lls/get-llvm-build-dirs)))

(defun lls/get-llvm-build-dir ()
  (car (lls/get-llvm-build-dirs)))

(defun lls/add-llvm-build-dir (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (comp-dev/ensure-initialized)
  (comp-dev/conf-set 'build-dirs
                     (cons dir
                           (comp-dev/conf-get 'build-dirs))))

(defun lls/get-clang-command-fun (&rest args)
  (apply (comp-dev/conf-get 'compile-command-fun)
         args))

(defun lls/get-llc-command-fun (&rest args)
  (apply (comp-dev/conf-get 'llc-command-fun) args))

(defun lls/get-dis-command-fun (&rest args)
  (apply (comp-dev/conf-get 'dis-command-fun) args))

;; ========================= LLVM Build Dirs =========================
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
   (list (comp-dev/prompt-tool (rx (or "clang" "llc") line-end))))
  (realgud--lldb
   (format "lldb %s"
           binary)))

(defun lls/cmake-here (directory build-type target)
  (interactive
   (list (expand-file-name "llvm"
                           (projectile-project-root))
         (completing-read "Build Type? "
                          '("Release"
                            "Debug"
                            "RelWithDebInfo"))
         (comp-dev/conf-get 'target)))
  (let* ((comm-temp
          "cmake %s -G Ninja -DCMAKE_BUILD_TYPE='%s' -DLLVM_ENABLE_PROJECTS='clang;llvm' -DLLVM_TARGETS_TO_BUILD='%s' -DCMAKE_C_COMPILER='clang' -DCMAKE_CXX_COMPILER='clang++' -DLLVM_ENABLE_ASSERTIONS=On")
         (command (read-string
                   "CMake Command? "
                   (format comm-temp directory build-type target))))
    (compile command)))

(provide 'llvm-comp-dev)
;;; llvm-comp-dev.el ends here
