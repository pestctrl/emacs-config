;;; my-predicates.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-09-01 10:26]

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
(require 'emacs-custom-load-or-ask)

(ec/load-or-ask-pred 'my-ec/is-wsl "Are you running Emacs in WSL?")
(ec/load-or-ask-pred 'my/puppet-p "Are you running on Puppet's computer?")
(ec/load-or-ask-pred 'my-ec/enable-exwm "Do you want to load EXWM?")
(ec/load-or-ask-pred 'my-ec/at-ti "Are you at TI for work?")
(defvar is-windows (or my-ec/is-wsl
                       (eq system-type
                           'windows-nt)))

(ec/load-or-ask-pred 'my-ec/add-info-dir "Do you want an auxiliary info dir? ")

(when my-ec/add-info-dir
  (ec/load-or-ask-dir 'my-ec/info-dir "Info Directory? ")
  (add-to-list 'Info-directory-list my-ec/info-dir))

(setq my-ec/enable-exwm (and my-ec/enable-exwm (eq 'x window-system)))

(ec/load-or-ask-pred 'my-ec/load-full-config "Do you want to load full config for emacs?")
(ec/load-or-ask-pred 'my-ec/load-org-config "Do you want to load org config?")

(provide 'my-predicates)
;;; my-predicates.el ends here
