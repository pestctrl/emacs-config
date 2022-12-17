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
(defvar lls/llvm-root-dir nil)

;; ========================= LLVM Build Dirs =========================
(defvar lls/llvm-build-dirs nil)

(defun lls/guess-build-dirs ()
  (when-let ((toplevel (magit-toplevel (buffer-file-name (current-buffer)))))
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

(defvar lls/init-build-dirs
  (lambda ()
    (or lls/llvm-build-dirs
        (append (lls/guess-build-dirs) '("/usr/bin"))
        (list (read-file-name "Where is llvm build directory? ")))))

(defvar lls/get-build-dirs-fun
  #'(lambda ()
      (when (null lls/llvm-build-dirs)
        (setq lls/llvm-build-dirs
              (funcall lls/init-build-dirs)))
      lls/llvm-build-dirs))

(defun lls/add-build-directory (dir)
  (interactive
   (list (read-file-name "Where? ")))
  (add-to-list 'lls/llvm-build-dirs
               dir))

;; =========================== LLVM Rebuild ==========================

(defvar llvm-core-count 8)

(defun lls/ninja-build-tools (build-dir tools-list)
  (format "cd %s && ninja -j %d %s"
          build-dir llvm-core-count
          (string-join tools-list " ")))

;; =============================== Misc ==============================

(defun lls/get-tool (tool-regexp &optional directories)
  (cl-mapcan #'(lambda (dir)
                 (directory-files dir t tool-regexp))
             (or directories
                 (funcall lls/get-build-dirs-fun))))

(defvar lls/get-build-dir-fun
  #'(lambda ()
      (car (funcall lls/get-build-dirs-fun))))

(defvar lls/get-clang-command-fun
  (lambda (compiler file action &optional rest)
    (format "%s %s %s %s"
            compiler
            file
            (concat
             (pcase action
               ('compile "-c ")
               ('assemble "-S ")
               ('preprocess "-E ")
               ('llvm-ir "-S -emit-llvm "))
             "-o -")
            " "
            (string-join rest " "))))

(provide 'llvm-shared)
;;; llvm-shared.el ends here
