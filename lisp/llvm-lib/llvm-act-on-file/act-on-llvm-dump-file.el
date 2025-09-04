;;; act-on-llvm-dump-file.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-02-01 17:51]

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
(require 'action-map-lib)

(defvar ll/dump-file-action-map
  '((assembly :key ?a  :major-mode asm-mode  :buffer-string "assembly" :description "[a]ssembly")))

(defun ll/is-dump-file (fname)
  (and (string-match-p (rx line-start "/tmp/")
                       (file-name-directory fname))
       (string-match-p (rx "/" (+ (not "/")) "-" (= 6 alphanumeric) "." (+ anything))
                       fname)))

(defun ll/dump-to-sh-file (fname)
  (expand-file-name
   (format "%s.sh"
           (-> fname
               (file-name-nondirectory)
               (file-name-sans-extension)))
   (file-name-directory fname)))

(defun ll/dump-extract-command-flags (fname)
  (with-current-buffer (find-file-noselect fname)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward
       (rx line-start (not "#")
           (group  "\"" (+? nonl) "\"")
           (group (+ nonl))
           line-end))
      (match-string 2))))

(defun ll/act-on-llvm-dump-file (fname)
  (let* ((action (aml/read-action-map ll/dump-file-action-map))
         (command-flags (ll/dump-extract-command-flags (ll/dump-to-sh-file fname)))
         (clang (lls/prompt-tool "clang$")))
    (compilation-start
     (concat
      (when (y-or-n-p "Would you like to `rr record`? ")
        "rr record ")
      clang command-flags " -S -o -")
     'asm-mode
     `(lambda (_)
        ,(format "*%s*" fname)))))

(provide 'act-on-llvm-dump-file)
;;; act-on-llvm-dump-file.el ends here
