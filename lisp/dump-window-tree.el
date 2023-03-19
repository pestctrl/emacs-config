;;; dump-window-tree.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-18 20:38]

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

(defun my/get-window-id (window)
  (let ((str (format "%s" window)))
    (when (string-match (rx "#<window " (group (+ digit))) str)
      (string-to-number (match-string 1 str)))))

(defun my/window-tree-to-dot (window)
  (when window
    (let ((window-id (my/get-window-id window))
          (buffer (window-buffer window))
          (child (window-child window)))
      (insert
       (format "%d [label=\"%d\n%s\"]\n"
               window-id window-id (if buffer (buffer-name buffer) "")))
      (while child
        (insert
         (format "%s -> %s\n"
                 window-id (my/get-window-id child)))
        (my/window-tree-to-dot child)
        (setq child (window-next-sibling child))))))

(defun my/dump-window-tree ()
  (interactive)
  (let* ((win (frame-root-window nil))
         (dot-file (make-temp-file "emacs-window-tree-" nil ".dot"))
         (png-file (concat (file-name-sans-extension dot-file) ".png")))
    (with-current-buffer (find-file-noselect dot-file)
      (insert "digraph g {\n")
      (my/window-tree-to-dot win)
      (insert "}\n")
      (save-buffer))
    (shell-command
     (format "dot -Tpng < %s > %s"
             dot-file
             png-file))
    (find-file png-file)))

(provide 'dump-window-tree)
;;; dump-window-tree.el ends here
