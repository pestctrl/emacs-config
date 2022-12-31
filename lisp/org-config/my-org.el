;;; my-org.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-03 14:42]

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
(require 'org)
(require 'org-overrides)

(require 'org-loop)
(require 'org-process)
(require 'org-project)
(require 'org-delay)

(require 'my-org-misc)
(require 'my-org-autosync)
(require 'my-org-indent)
(require 'my-org-agenda-commands)
(require 'my-org-capture-templates)
(require 'org-other-additions)

(require 'my-org-capture-shouldnt-mess-windows)

(require 'self-chat-mode)

(provide 'my-org)
;;; my-org.el ends here
