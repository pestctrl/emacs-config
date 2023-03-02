;;; dired-native-compile.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-02 14:50]

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
(require 'dired)

(defun dired-native-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	     elc-file buffer-read-only failure)
    (condition-case err
	    (save-excursion (setq elc-file (native-compile filename)))
      (error
       (setq failure err)))
    (if failure
	    (progn
	      (dired-log "Byte compile error for %s:\n%s\n" filename failure)
	      (dired-make-relative filename))
      (dired-remove-file elc-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file elc-file)
      nil)))

(defun dired-do-native-compile (&optional arg) ; Bound to `B'
  "Byte compile marked Emacs Lisp files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories."
  (interactive (let* ((arg  current-prefix-arg)
                      (C-u  (and (consp arg)  arg)))
                 (when (and C-u  (> (prefix-numeric-value arg) 16)) (setq arg  '(16)))
                 (list arg)))
  (dired-map-over-marks-check #'dired-native-compile arg 'native-compile (diredp-fewer-than-2-files-p arg)))

(provide 'dired-native-compile)
;;; dired-native-compile.el ends here
