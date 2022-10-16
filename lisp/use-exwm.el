;;; use-exwm.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2021-08-15 12:55]

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

;; Handling loading of exwm will be handled by init.el. This macro is
;; used so I don't accidentally forget to :defer exwm. This way, every
;; :config block is automatically wrapped in an eval-after-load block.

;;; Code:

(defmacro use-exwm (&rest body)
  `(use-package exwm
     :ensure nil
     :defer t
     :quelpa (exwm :fetcher "github-ssh" :repo "pestctrl/exwm")
     ,@body))

(provide 'use-exwm)
;;; use-exwm.el ends here
