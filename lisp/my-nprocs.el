;;; my-nprocs.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-12-27 10:07]

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
(require 'dash)

(defvar nprocs-cache nil)

(defun nprocs ()
  (or nprocs-cache
      (setq nprocs-cache
            (-->
             (shell-command-to-string "nproc")
             (string-to-number it)
             (let ((num it))
               (max (- num 16)
                    (/ num 2)))))))

(provide 'my-nprocs)
;;; my-nprocs.el ends here
