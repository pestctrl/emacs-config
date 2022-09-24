;;; side-window-split.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 13:36]

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
(require 'cl-lib)

(defvar my/side-window-slot-map
  '((left 0)
    (right 0)
    (top 0)
    (bottom 0)))

(defun side-window-delete-all ()
  (interactive)
  (setq my/side-window-slot-map
        '((left 0)
          (right 0)
          (top 0)
          (bottom 0)))
  (call-interactively #'window-toggle-side-windows))

(defun my/emacs-read-side ()
  (pcase (read-key "[k] Top [j] Bottom [h] Left [r] Right")
    (?k 'top)
    (?j 'bottom)
    (?h 'left)
    (?r 'right)))

(defmacro cl-incf-post (place)
  `(prog1 ,place
     (setf ,place (1+ ,place))))

(defun my/display-buffer-in-side-window (side buffer-or-name &optional slot-override)
  (interactive
   (list (my/emacs-read-side)
         (read-buffer-to-switch "Switch to buffer in side window: ")))
  (let ((buffer (if (bufferp buffer-or-name)
                    buffer-or-name
                  (get-buffer buffer-or-name)))
        (slot (or slot-override
                  (cl-incf-post (car (alist-get side my/side-window-slot-map))))))
    (display-buffer-in-side-window
     buffer
     `((side . ,side)
       (slot . ,slot)
       (dedicated . t)
       (window-parameters (no-delet-other-windows . t))))))

(defun my/find-file-side-window (side filename &optional slot-override)
  (interactive
   (list (my/emacs-read-side)
         (read-file-name "Find file in side window: " nil default-directory (confirm-nonexistent-file-or-buffer))))
  (let ((buffer (find-file-noselect filename)))
    (my/display-buffer-in-side-window side buffer slot-override)))

(defun side-left-window ()
  (interactive)
  (my/display-buffer-in-side-window
   'left
   (read-buffer "Switch to buffer in side window: "
                (current-buffer)
                (confirm-nonexistent-file-or-buffer))))

(defun side-right-window ()
  (interactive)
  (my/display-buffer-in-side-window
   'right
   (read-buffer "Switch to buffer in side window: "
                (current-buffer)
                (confirm-nonexistent-file-or-buffer))))

(defun side-bottom-window ()
  (interactive)
  (my/display-buffer-in-side-window
   'bottom
   (read-buffer "Switch to buffer in side window: "
                (current-buffer)
                (confirm-nonexistent-file-or-buffer))))

(defun side-top-window ()
  (interactive)
  (my/display-buffer-in-side-window
   'top
   (read-buffer "Switch to buffer in side window: "
                (current-buffer)
                (confirm-nonexistent-file-or-buffer))))

(provide 'side-window-split)
;;; side-window-split.el ends here
