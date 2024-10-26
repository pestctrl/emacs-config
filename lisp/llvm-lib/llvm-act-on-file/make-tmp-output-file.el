;;; make-tmp-output-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-10-25 19:36]

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

;; TODO: Very dirty
(defvar ll/act-on-file-output nil)
(make-variable-buffer-local 'll/act-on-file-output)

(defun ll/make-tmp-file (file ext)
  (let ((file-directory (file-name-directory file))
        fname temporary-file-directory)
    (if (string-match-p (rx "/tmp") file)
        (setq fname (file-name-sans-extension file)
              temporary-file-directory file-directory)
      (setq fname (file-name-nondirectory (file-name-sans-extension file))
            temporary-file-directory (expand-file-name "tmp" file-directory)))

    (make-temp-file (concat fname "-")
                    nil ext)))

(provide 'make-tmp-output-file)
;;; make-tmp-output-file.el ends here
