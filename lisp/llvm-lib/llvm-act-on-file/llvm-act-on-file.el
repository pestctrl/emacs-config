;;; llvm-act-on-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 18:46]

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
(require 'llvm-shared)
(require 'my-comp-minor-mode)
(require 'act-on-test-file)
(require 'act-on-c-file)
(require 'act-on-ll-file)
(require 'act-on-obj-file)
(require 'act-on-llvm-source-file)
(require 'act-on-llvm-dump-file)
(require 'act-on-tablegen-file)
(require 'act-on-asm-file)

(defvar ll/buffer-map (make-hash-table :test #'equal))

(defun ll/act-on-file (buffer)
  (interactive (list (or (and (eq major-mode 'dired-mode)
                              (dired-get-filename nil 'NO-ERROR))
                         (current-buffer))))
  (with-current-buffer buffer
    (when (buffer-modified-p)
      (save-buffer)))
  (let ((file (or (buffer-file-name)
                  (aprog1 (make-temp-file nil nil ".ll")
                    (write-file it)))))
    (when-let ((vec (lls/tramp-connection))
               ((not (file-remote-p file))))
      ;; If there's a cache entry, and the cache timestamp matches,
      ;; then we just reuse the file
      (-->
       (if-let* ((cache (gethash file ll/buffer-map))
                 (time (visited-file-modtime))
                 ((equal time (car cache))))
           (cdr cache)
         ;; Otherwise, generate a new tmp file, copy that over, and
         ;; update the cache.
         (let ((tmp (concat (tramp-make-tramp-temp-name vec)
                            "." (file-name-extension file))))
           (puthash file (cons time tmp) ll/buffer-map)
           (copy-file file tmp)
           tmp))
       (setq file it)))
    ;; Let further compilation-commands be run over tramp
    (let ((default-directory (file-name-directory file)))
      (pcase (file-name-extension file)
        ((and _ (guard (ll/is-test-file file)))
         (ll/act-on-test-file file))
        ((and "td" (guard (and (ll/is-llvm-source-file file))))
         (ll-tblgen file))
        ((and _ (guard (ll/is-llvm-source-file file)))
         (ll/act-on-llvm-source-file file))
        ((and _ (guard (ll/is-dump-file file)))
         (ll/act-on-llvm-dump-file file))
        ("o" (ll/act-on-obj-file file))
        ("out" (ll/act-on-obj-file file))
        ("tiout" (ll/act-on-obj-file file))
        ("c" (ll/act-on-c-file file))
        ("inc" (ll/act-on-c-file file))
        ("asm" (ll/act-on-asm-file file))
        ("ll" (ll/act-on-ll-file file))
        (_ (message "Not sure what you'd like me to do with this file"))))))

(provide 'llvm-act-on-file)
;;; llvm-act-on-file.el ends here
