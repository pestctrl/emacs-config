;;; act-on-obj-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-29 11:22]

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
(require 'action-map-lib)
(require 'anaphora)

(defvar ll/obj-file-action-map
  '((disassemble     :key ?d  :major-mode asm-mode  :buffer-string "asm"  :description "[d]isassemble")))

(defun ll/build-obj-command (file _action)
  (funcall lls/get-dis-command-fun file action))

(defun ll/act-on-obj-file (file)
  (let* ((action (aml/read-action-map ll/obj-file-action-map))
         (stop (when (member action '(stop-after stop-before))
                 (read-string "Which pass? "))))
    (aprog1
        (compilation-start
         (ll/build-obj-command file action)
         (aml/get-map-prop ll/obj-file-action-map action :major-mode)
         `(lambda (_)
            ,(format "*%s-%s*"
                     (file-name-nondirectory file)
                     (let ((str (aml/get-map-prop ll/obj-file-action-map action :buffer-string)))
                       (if (not (member action '(stop-before stop-after)))
                           str
                         (format str stop))))))
      (with-current-buffer it
        (make-variable-buffer-local 'compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              nil)))))

(provide 'act-on-obj-file)
;;; act-on-obj-file.el ends here
