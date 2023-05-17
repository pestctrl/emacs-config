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
(ll/def-tablegen-file RegisterInfo "llvm/lib/Target/%s/%sRegisterInfo.td")

(defmacro ll/get-tablegen-file (sym target)
  (let ((fun (intern (format "ll/%s-filename" (symbol-name sym)))))
    `(let ((file (,fun ,target)))
       (find-file-noselect file))))

(defconst ll/ibase-regexp
  (rx (seq "<"
           (+? (not ",")) "," (*? (or white "\n"))
           "(outs" (optional " " (group (+ (not ")")))) ")," (*? (or white "\n"))
           "(ins" (optional " " (group (+ (not ")")))) ")," (*? (or white "\n"))
           "\"" (group (+ (not "\""))) "\""
           (+? (not ">"))
           ">," (* space) "\n")))

(defconst ll/tablegen-def
  (rx (group "def %s" (+? anything)) "\n\n"))

(defun ll/get-instructions-list (file)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((r (rx line-start "def " (group (+ (or alphanumeric "_")))))
            l)
        (while (re-search-forward r nil t)
          (let ((instr (match-string 1)))
            (re-search-forward ll/ibase-regexp nil t)
            (let* ((repr (->> (match-string 3)
                              (string-replace "\\t" " ")))
                   (str (format "%-40s%s" instr repr)))
              (set-text-properties 0 (length str) nil str)
              (push str l))))
        l))))

(defmacro push-single-match (place rx)
  `(save-excursion
     (goto-char (point-min))
     (re-search-forward ,rx)
     (push (match-string 1) ,place)))

(defun ll/show-instr-read (target)
  (let ((l (ll/get-instructions-list (ll/ISA-filename target)))
        (sym (symbol-name (symbol-at-point))))
    (car
     (string-split
      (if (member sym l)
          (completing-read (format "Which instruction? (default: %s) "
                                   sym)
                           l nil t nil nil sym)
        (completing-read "Which Instruction? "
                         l))
      " "))))

(defun ll/get-codegen-targets ()
  (-->
   (lls/get-llvm-root-dir)
   (expand-file-name "llvm/lib/Target" it)
   (directory-files it t "[^.]")
   (remove-if-not #'file-directory-p it)
   (mapcar #'file-name-nondirectory it)))

(defun ll/prompt-for-instr-info ()
  (interactive)
  (let* ((target (completing-read "Target? "
                                  (ll/get-codegen-targets)))
         (instr (ll/show-instr-read target)))
    (ll/show-instr-info target instr)))

(defun ll/get-dag-classes (target classes)
  (--> classes
       (mapcar #'(lambda (class)
                   (or (with-current-buffer (ll/get-tablegen-file InstrInfo target)
                         (save-excursion
                           (beginning-of-buffer)
                           (when (re-search-forward (format ll/tablegen-def class) nil t)
                             (let ((str "// InstrInfo.td\n"))
                               (add-text-properties 0 (length str) '(face font-lock-comment-face) str)
                               (concat str (match-string 1))))))
                       (with-current-buffer (ll/get-tablegen-file RegisterInfo target)
                         (save-excursion
                           (beginning-of-buffer)
                           (when (re-search-forward (format ll/tablegen-def class) nil t)
                             (let ((str "// RegisterInfo.td\n"))
                               (add-text-properties 0 (length str) '(face font-lock-comment-face) str)
                               (concat str (match-string 1))))))))
               it)
       (string-join it "\n\n")))

(defun ll/show-instr-info (target instr)
  (save-restriction
    (let* ((buf (get-buffer-create (format "*%s-instr-info*" instr)))
           itin ins outs dag-classes
           l)
      (with-current-buffer (ll/get-tablegen-file ISA target)
        (save-excursion
          (goto-char (point-min))
          (re-search-forward (format ll/tablegen-def instr))
          (let ((str (match-string 1)))
            (when my-ec/at-ti
              (string-match ll/ibase-regexp str)
              (-->
               (list (match-string 3 str)
                     (concat "inputs: " (setq ins (match-string 2 str)))
                     (concat "outputs: " (setq outs (match-string 1 str))))
               (string-join it "\n")
               (string-replace "\\t" " " it)
               (cons "Literal Representation" it)
               (push it l))

              (setq dag-classes
                    (-->
                     (append (string-split (or ins "") ",")
                             (string-split (or outs "") ","))
                     (remove-if #'string-empty-p it)
                     (mapcar #'(lambda (x)
                                 (set-text-properties 0 (length x) nil x)
                                 x)
                             it)
                     (mapcar #'string-trim it)
                     (mapcar #'(lambda (x)
                                 (car (string-split x ":")))
                             it)
                     (seq-uniq it)))

              (push (cons "DAG classes" (ll/get-dag-classes target dag-classes)) l))

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
                 (string-trim it)
                 (push (cons "Schedule Timing Definitions" it) l)))
          (re-search-forward (rx (group "class " (literal itin) "Data" symbol-end (+? (not ";")) ";")))
          (push (cons "Itinerary class" (match-string 1)) l)))

      (with-current-buffer buf
        (erase-buffer)
        (tablegen-mode)
        (save-excursion
          (dolist (i (reverse l))
            (insert (car i))
            (let ((fill-column 77))
              (banner-comment))
            (insert "\n\n")
            (insert (cdr i) "\n\n"))))

      (display-buffer-in-side-window
       buf '((side . right)
             (window-width . 80))))))

(provide 'llvm-show-instr-info)
;;; llvm-show-instr-info.el ends here
