;;; org-table-convert.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-12-29 10:07]

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

(defun gdb-stacktrace-to-org-table ()
  (interactive)
  (let ((regexp (rx (and line-start
                         "#"
                         ;; Stack Number
                         (group (+? digit))
                         (+ " ")
                         (optional
                          (and "0x"
                               (+ alnum)
                               " in "))
                         ;; Function Name
                         (group (+ nonl))
                         (optional " ")
                         "("
                         (*? anything)
                         ")"
                         (* space)  "at "
                         ;; File name
                         (group
                          ;;"/"
                          (+? nonl))
                         ":"
                         ;; Line Number
                         (group
                          (+ digit))
                         line-end
                         ))))
    (beginning-of-buffer)
    (save-excursion
      (while (re-search-forward regexp nil 'noerror)
        (replace-match
         (let ((path (match-string 3)))
           (save-match-data
             (format "|\\1|\\2|%s:\\4|[[\\3::\\4][Link]]|"
                     (cond ((string-match (rx (and "/scratch/benson/tools"
                                                   (* digit)
                                                   "/llvm_cgt/llvm-project/"
                                                   (group
                                                    (+ nonl))))
                                          path)
                            (format "$LLVM_PROJECT/%s" (match-string 1 path)))
                           ((string-match (rx (and "/scratch/benson/tools"
                                                   (* digit)
                                                   "/"
                                                   (group
                                                    (+ nonl))))
                                          path)
                            (format "$SANDBOX/%s" (match-string 1 path)))
                           (t path))))))))
    (org-table-sort-lines nil ?N)
    (save-excursion
      (insert "|-\n|#|Function Name|File & Line Number|Link|\n|-\n")
      (end-of-buffer)
      (insert "\n|-"))
    (org-table-align)))

(defun lldb-filename-to-org-link (filename)
  (setq filename
        (string-replace ":" "::" filename))
  (save-match-data
    (if (string-match (rx line-start "/scratch/benson/" (+ alphanumeric) "/llvm_cgt/llvm-project/"
                          (group (+ nonl)))
                      filename)
        (format "[[%s][LLVM/%s]]"
                filename
                (match-string 1 filename))
      (string-match (rx line-start (optional "/")
                        (+ (and (+ (not "/")) "/" ))
                        (group
                         (+ nonl)))
                    filename)
      (format "[[%s][%s]]"
              filename
              (match-string 1 filename)))))

(defun lldb-stacktrace-to-org-table ()
  (interactive)
  ;; (narrow-to-region (point) (point))
  ;; (save-excursion
  ;;   (yank))
  (let ((regexp
         (rx (and line-start (+ nonl)
                  "frame #" (group (+ digit))
                  ": 0x" (= 16 alphanumeric) " " (+ nonl) "`"
                  (group
                   (+? nonl))
                  (optional
                   "(" (+? nonl) ")")
                  (or
                   line-end
                   (and
                    " at "
                    (group
                     (+ nonl))))))))
    (while (re-search-forward regexp
                              nil t)
      (let ((num (match-string 1))
            (function (match-string 2))
            (filename (match-string 3)))

        (save-match-data
          (while
              (when (string-match-p "<[^<>]+>" function)
                (setq function
                      (replace-regexp-in-string "<[^<>]+>" "" function)))))

        (when filename
          (setq filename
                (lldb-filename-to-org-link filename)))

        (replace-match (format "|%s|%s|%s|" num function (or filename ""))))))
  (goto-char (point-min))
  (org-table-sort-lines nil ?N)
  (save-excursion
    (insert "|-\n|#|Function Name|File & Line Number|Link|\n|-\n")
    (end-of-buffer)
    (insert "\n|-"))
  (org-table-align))

(provide 'org-table-convert)
;;; org-table-convert.el ends here
