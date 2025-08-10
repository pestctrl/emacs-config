;;; nix-only.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-08-10 14:08]

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
(require 'anaphora)

;; TODO: Need to know if home-manager config is deployed or not
(defun nix-present-p ()
  (executable-find "nix"))

(when (nix-present-p)
  (aand (executable-find "ledger")
        (--> (file-truename it)
             (file-name-parent-directory it)
             (directory-file-name it)
             (file-name-parent-directory it)
             (directory-file-name it)
             (expand-file-name "share/info" it)
             (add-to-list 'Info-directory-list it))))

(provide 'nix-only)
;;; nix-only.el ends here
