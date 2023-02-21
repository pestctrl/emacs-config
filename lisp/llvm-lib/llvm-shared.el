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

;; =========================== LLVM Rebuild ==========================

(defvar llvm-core-count 8)

(defun lls/ninja-build-tools (build-dir tools-list &optional verbose)
  (format "ninja -C %s -j %d %s %s"
          build-dir llvm-core-count
          (if verbose "-v" "")
          (string-join tools-list " ")))

;; =============================== Init ==============================

(defvar lls/llvm-root-dir nil)
(defvar lls/llvm-build-dirs nil)
(defvar lls/llvm-bin-dirs nil)

(defvar lls/target-init-fun
  nil)

(defun lls/init-llvm-shared (root-dir build-dirs &optional bindirs)
  ;; TODO: make interactive
  (let ((r (rx (or "RelWithAsserts" "Release"))))
    (setq lls/llvm-root-dir (or root-dir
                                (read-file-name "llvm-project directory? "))
          lls/llvm-build-dirs
          (sort build-dirs
                #'(lambda (x y)
                    (cond ((string-match-p r y) nil)
                          ((string-match-p r x) t)
                          (t (string< x y)))))
          lls/llvm-bin-dirs bindirs)))

(defun lls/uninitialized? ()
  (not (and lls/llvm-root-dir
            lls/llvm-build-dirs)))

(defun lls/get-llvm-root-dir ()
  (when (lls/uninitialized?)
    (funcall lls/target-init-fun #'lls/init-llvm-shared))
  lls/llvm-root-dir)

(defun lls/get-llvm-build-dirs ()
  (when (lls/uninitialized?)
    (funcall lls/target-init-fun #'lls/init-llvm-shared))
  lls/llvm-build-dirs)

(defun lls/get-llvm-bin-dir ()
  (car (lls/get-llvm-bin-dirs)))

(defun lls/get-llvm-bin-dirs ()
  (when (lls/uninitialized?)
    (funcall lls/target-init-fun #'lls/init-llvm-shared))
  (append (mapcar #'(lambda (x) (expand-file-name "bin" x))
                  (lls/get-llvm-build-dirs))
          lls/llvm-bin-dirs))

(defun lls/get-llvm-build-dir ()
  (car (lls/get-llvm-build-dirs)))

(defun lls/add-llvm-build-dir (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (add-to-list 'lls/llvm-build-dirs
               dir))

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

(defvar lls/get-clang-command-fun
  (cl-function
   (lambda (compiler file action &key output rest)
     (string-join (list compiler
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
                  " "))))

(defvar lls/get-llc-command-fun
  (lambda (file _action)
    (concat "llc -o - "
            file " ")))

(defvar lls/get-dis-command-fun
  (lambda (file _action)
    (concat "llvm-objdump --disassemble "
            file " ")))

;; ========================= LLVM Build Dirs =========================

(setq lls/target-init-fun
      ;; TODO: load llvm-mode
      (lambda (callback)
        (funcall callback
                 (lls/guess-root-dir-fun)
                 (lls/guess-build-dirs-fun))))

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
