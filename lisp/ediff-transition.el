;;; ediff-transition.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-05-13 08:08]

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

;; TOOD: Apparently I can rewrite this in ediff-mult.el?

(defvar ediff-transition-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "N" #'ediff-transition-forward)
    (define-key keymap "P" #'ediff-transition-backward)
    keymap))

(defun ediff-transition-display (index file-list)
  (when ediff-transition-mode
    (ediff-really-quit nil))
  (ediff-files (nth index file-list)
               (nth (1+ index) file-list)
               '((lambda (&rest optional)
                   (ediff-transition-mode)))))

(defun ediff-transition-forward ()
  (interactive)
  (if (<= (1- (1- (length ediff-transition-list))) ediff-transition-index)
      (message "Reached end of file list")
    (cl-incf ediff-transition-index)
    (ediff-transition-display
     ediff-transition-index
     ediff-transition-list)))

(defun ediff-transition-backward ()
  (interactive)
  (if (<= ediff-transition-index 0)
      (message "Reached beginning of file list")
    (cl-decf ediff-transition-index)
    (ediff-transition-display
     ediff-transition-index
     ediff-transition-list)))

(define-minor-mode ediff-transition-mode ""
  :lighter ediff-trans
  :keymap ediff-transition-mode-map)

;; TODO: transition (haha) away from global variables
(defvar ediff-transition-list nil)
(defvar ediff-transition-index 0)

(defun ediff-transition (l)
  (when (< (length l) 2)
    (error "Must specify more than one file"))
  (setq ediff-transition-list l
        ediff-transition-index 0)
  (ediff-transition-display
   ediff-transition-index
   ediff-transition-list))

(defun ediff-transition-llvm-print-after-all (buffer)
  (interactive
   (list (current-buffer)))
  (let ((window--sides-inhibit-check t))
    (set-window-parameter (selected-window) 'window-side nil)
    (save-excursion
      (goto-char (point-min))
      (let ((r (rx line-start (optional "# ") "*** "))
            flist)
        (re-search-forward r)
        (let ((previous-index (match-beginning 0)))
          (while (re-search-forward r nil t)
            (let ((file (make-temp-file "llvm-print-after-all-" nil ".ll")))
              (write-region previous-index (match-beginning 0) file)
              (push file flist))
            (setq previous-index (match-beginning 0))))
        (ediff-transition (reverse flist))))))

;; (ediff-transition
;;  (mapcar #'(lambda (x)
;;              (expand-file-name x
;;                                "~/workspace/ediff-multiple"))
;;          '("1.txt" "2.txt" "3.txt" "4.txt")))

(provide 'ediff-transition)
;;; ediff-transition.el ends here
