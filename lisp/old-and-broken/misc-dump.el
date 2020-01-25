;;; misc-dump.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 09:15]

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

(defun dmenu-run ()
  (interactive)
  (shell-command "dmenu" nil "dmenu_run -b"))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name (string= exwm-class-name "Emacs"))
              (exwm-input-set-local-simulation-keys nil))))

(provide 'misc-dump)
;;; misc-dump.el ends here
