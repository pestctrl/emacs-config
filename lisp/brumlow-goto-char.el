;;; brumlow-goto-char.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-12-30 14:11]

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
(defvar jump-to-last-char nil)
(defvar jump-to-last-count nil)
  
(defun jump-to-char-repate-forward ()
  "Repate the last jump-to-char forward"
  (interactive)
  (forward-char)
  (jump-to-char-fun jump-to-last-char (abs jump-to-last-count)))

(defun jump-to-char-repate-backwards ()
  "Repate the last jump-to-char backward"
  (interactive)
  (jump-to-char-fun jump-to-last-char (- (abs jump-to-last-count))))

(defun jump-to-char-fun (char count)
  "Jump to char function"
  (if (search-forward (string char) nil t count)
      (when (> count 0)
        (backward-char))
    (message "Search Failed: %s" (char-to-string char)))
  (setq jump-to-last-char char)
  (setq jump-to-last-count count) 
  (unless defining-kbd-macro 
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") 'jump-to-char-repate-forward)
       (define-key map (kbd ",") 'jump-to-char-repate-backwards)
       map))))

(defun jump-to-char (arg)
  "Jump to char"
  (interactive "p")
  (message nil)
   (let ((char (read-char "jump-to-char: ")))
     (jump-to-char-fun char arg)))

(defun jump-to-char-backward (arg)
  "Jump to char backwards"
  (interactive "p")
      (jump-to-char (- arg)))

(provide 'brumlow-goto-char)
;;; brumlow-goto-char.el ends here
