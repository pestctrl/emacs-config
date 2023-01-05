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

(defun ll/act-on-file (file)
  (interactive (list (or (and (eq major-mode 'dired-mode)
                              (dired-get-filename nil 'NO-ERROR))
                         (buffer-file-name (current-buffer))
                         (read-file-name "File? "))))
  (when (null file)
    (setq file (make-temp-file nil nil ".ll"))
    (write-file file))
  (pcase (file-name-extension file)
    ((and _ (guard (ll/is-test-file file)))
     (ll/act-on-test-file file))
    ("o" (ll/act-on-obj-file file))
    ("c" (ll/act-on-c-file file))
    ("ll" (ll/act-on-ll-file file))
    (_ (message "Not sure what you'd like me to do with this file"))))

(provide 'llvm-act-on-file)
;;; llvm-act-on-file.el ends here