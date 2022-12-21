;;; llvm-show-instr-info.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-19 15:34]

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

(defmacro ll/def-tablegen-file (name str)
  (let ((getter-name (intern (format "ll/%s-filename" (symbol-name name)))))
    `(defun ,getter-name (target)
       (expand-file-name
        (format ,str target target)
        (lls/get-llvm-root-dir)))))

;; TODO: Assuming that these files always exist
(ll/def-tablegen-file ISA "llvm/lib/Target/%s/%sISA.td")
(ll/def-tablegen-file Schedule "llvm/lib/Target/%s/%sSchedule.td")
(ll/def-tablegen-file InstrInfo "llvm/lib/Target/%s/%sInstrInfo.td")

(defmacro ll/get-tablegen-file (sym target)
  (let ((fun (intern (format "ll/%s-filename" (symbol-name sym)))))
    `(let ((file (,fun ,target)))
       (or (find-buffer-visiting file)
           (find-file-noselect file)))))

(defun ll/get-instructions-list (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (save-excursion
      (goto-char (point-min))
      (let ((r (rx line-start "def " (group (+ (or alphanumeric "_")))))
            l)
        (while (re-search-forward r nil t)
          (let ((str (match-string 1)))
            (set-text-properties 0 (length str) nil str)
            (push str l)))
        l))))

(defmacro push-single-match (place rx)
  `(save-excursion
     (goto-char (point-min))
     (re-search-forward ,rx)
     (push (match-string 1) ,place)))

(defun ll/show-instr-read (target)
  (let ((l (ll/get-instructions-list (ll/ISA-filename target)))
        (sym (symbol-name (symbol-at-point))))
    (if (member sym l)
        (completing-read (format "Which instruction? (default: %s) "
                                 sym)
                         l nil t nil nil sym)
      (completing-read "Which Instruction? "
                       l))))

(defun ll/prompt-for-instr-info ()
  (interactive)
  (let* ((target (read-string "Target? ")) ;; TODO: Add some indirection
         (instr (ll/show-instr-read target)))
    (ll/show-instr-info target instr)))

(defun ll/show-instr-info (target instr)
  (save-restriction
    (let* ((buf (get-buffer-create (format "*%s-instr-info*" instr)))
           itin
           l)
      (with-current-buffer (ll/get-tablegen-file ISA target)
        (save-excursion
          (goto-char (point-min))
          (re-search-forward (rx (group "def " (literal instr) (+? anything)) "\n\n"))
          (let ((str (match-string 1)))
            (string-match (rx "ItinClass<" (group (+ (or alphanumeric "_"))) ">") str)
            (setq itin (match-string 1 str))
            (push (cons "ISA definition" str) l))))

      (with-current-buffer (ll/get-tablegen-file Schedule target)
        (save-excursion
          (goto-char (point-min))
          ;; TODO: Better way to separate functionality
          (when my-ec/at-ti
            (re-search-forward (rx (group (+ "defvar _" (+ alphanumeric) " = " (+ digit) ";" (+ "\n")))))
            (--> (match-string 1)
                 (replace-regexp-in-string "\n+" "\n" it)
                 (compat-string-trim it)
                 (push (cons "Schedule Timing Definitions" it) l)))
          (re-search-forward (rx (group "class " (literal itin) "Data" symbol-end (+? (not ";")) ";")))
          (push (cons "Itinerary class" (match-string 1)) l)))

      (with-current-buffer buf
        (erase-buffer)
        (tablegen-mode)
        (dolist (i (reverse l))
          (insert (car i))
          (let ((fill-column 77))
            (banner-comment))
          (insert "\n\n")
          (insert (cdr i) "\n\n")))

      (display-buffer-in-side-window
       buf '((side . right)
             (window-width . 80))))))

(provide 'llvm-show-instr-info)
;;; llvm-show-instr-info.el ends here
