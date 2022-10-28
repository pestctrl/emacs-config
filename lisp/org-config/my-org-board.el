;;; my-org-board.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-10-26 19:07]

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
(require 'use-package)
(require 'org-attach)
(require 'my-org-agenda-files)

(when (setq my/is-plaintext-mega-folder t)
  (setq org-attach-id-dir
        (expand-file-name "org/org-board-data"
                          my/plaintext-mega-folder)))

(use-package org-board)
(add-to-list 'org-board-agent-header-alist
             '("Linux" . "--user-agent=\"Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.6) Gecko/20070802 SeaMonkey/1.1.4\""))
(setq org-board-wget-show-buffer nil)

(defun mob/warn-if-no-mega-folder ()
  (or my/is-plaintext-mega-folder
      (user-error "Hey, you should probably setup megasync before using this")))

(advice-add #'org-board-archive
            :before-while
            #'mob/warn-if-no-mega-folder)

(defun mob/warn-if-no-url ()
  (or (org-entry-get-multivalued-property (point) "URL")
      (user-error "Hey dummy, there's nothing for me to archive")))

(advice-add #'org-board-archive
            :before-while
            #'mob/warn-if-no-url)

(defun my/org-add-tag (tag)
  (let ((tags (org-get-tags nil t)))
    (when (not (member tag tags))
      (org-set-tags (cons tag tags)))))

(defun org-board-add-offline-tag (&rest args)
  (my/org-add-tag "offline"))

(advice-add #'org-board-archive :after
            #'org-board-add-offline-tag)

(provide 'my-org-board)
;;; my-org-board.el ends here
