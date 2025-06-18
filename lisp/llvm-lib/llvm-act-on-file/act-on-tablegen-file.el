;;; act-on-tablegen-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-05-08 15:37]

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

(defun ll-tblgen/get-includes (file build-dir)
  (-->
   (list "llvm/include"
         "llvm/include/llvm/IR")
   (mapcar #'(lambda (x) (expand-file-name x (lls/get-llvm-root-dir))) it)
   ;; TODO Hard coding this value
   (cons build-dir it)
   (reverse it)
   (cons (file-name-directory file) it)
   (mapcar #'(lambda (x) (concat "-I" (lls/un-trampify x))) it)
   (string-join it " ")))

(defun ll-tblgen/gen-command (file flags output-file build-dir)
  (let ((bin (car (lls/get-tool "llvm-tblgen$" (list (expand-file-name "bin" build-dir))))))
    (format "%s %s %s %s"
            bin
            (lls/un-trampify file)
            (ll-tblgen/get-includes file build-dir)
            (string-join
             (list
              "--write-if-changed"
              (string-join flags " ")
              )
             " "))))

(defun ll-tblgen/cmake-extract-tblgen-commands (&optional buffer build-dir)
  (interactive
   (list (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((file-regexp (rx line-start
                             (group "set(LLVM_TARGET_DEFINITIONS "
                                    (group (+ (not (any  ")" "\n"))))
                                    ")")
                             line-end))
            (tblgen-regexp (rx line-start
                               ;; TODO: Hard coded LLVM
                               "tablegen(LLVM "
                               (group (+ (or "." alphanumeric)))
                               " "
                               (group (+ nonl))
                               ")"
                               line-end))
            (folder (file-name-directory (buffer-file-name buffer)))
            tablegen-files
            search-regions
            commands)

        (while (re-search-forward file-regexp nil t)
          (let ((str (match-string 2)))
            (set-text-properties 0 (length str) nil str)
            (push (list (expand-file-name str folder) (match-beginning 1) (match-end 1)) tablegen-files)))

        (nreverse tablegen-files)

        (let (previous-file
              previous-end)
          (dolist (file tablegen-files)
            (when previous-file
              (push (list previous-file
                          previous-end
                          (cadr file))
                    search-regions))
            (setq previous-file (car file)
                  previous-end (caddr file)))
          (push (list previous-file
                      previous-end
                      nil)
                search-regions))

        (dolist (file search-regions)
          (let ((file (car file))
                (start (cadr file))
                (end (caddr file)))
            (goto-char start)
            (while (re-search-forward tblgen-regexp end t)
              (let ((out (match-string 1))
                    (flags (match-string 2)))
                (setq out
                      (-->
                       (file-name-directory (buffer-file-name buffer))
                       (replace-regexp-in-string
                        (expand-file-name "llvm" (lls/get-llvm-root-dir))
                        (or build-dir (lls/get-llvm-build-dir))
                        it)
                       (expand-file-name out it)))
                (set-text-properties 0 (length out) nil out)
                (set-text-properties 0 (length flags) nil flags)
                (push (list (string-join (list (file-name-nondirectory file) flags (file-name-nondirectory out)) " => ")
                            (ll-tblgen/gen-command file (split-string flags " ") out (or build-dir (lls/get-llvm-build-dir)))
                            out)
                      commands)))))

        (if (called-interactively-p)
            (message (string-join (mapcar #'car commands) "\n"))
          commands)))))

;; TODO: replace references to default-directory with grabbing the
;; directory of file
(defun ll-tblgen (file)
  (interactive)
  (when-let ((f (find-file-noselect (expand-file-name "CMakeLists.txt"
                                                      (file-name-directory file)))))
    (let* ((action (read-key "[c]ompile [p]rint-records")))
      (when (not (member action '(?c ?p)))
        (error "Unknown action"))
      (let* ((build-dir (lls/prompt-llvm-build-dir))
             (commands (ll-tblgen/cmake-extract-tblgen-commands f build-dir))
             (key (completing-read "Which tablegen command? " (mapcar #'car commands)))
             (out-comm (alist-get key commands nil nil #'equal))
             (comm (car out-comm))
             (out (cadr out-comm)))
        (cond
         ((eq action ?p)
          (setq comm (concat comm " --print-records")))
         ((eq action ?c)
          (setq comm (concat (format "mkdir -p %s"
                                     (lls/un-trampify (diredp-parent-dir out)))
                             " && "
                             comm
                             (format " -o %s"
                                     (lls/un-trampify out))))))

        (compilation-start
         comm
         nil
         (lambda (_) "*tblgen*"))))))

(provide 'act-on-tablegen-file)
;;; act-on-tablegen-file.el ends here
