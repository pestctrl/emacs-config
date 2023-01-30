;;; my-org-capture-shouldnt-mess-windows.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-31 17:03]

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

(defun my/side-window-p (window)
  (window-parameter window 'window-side))

(defun my/org-capture-shouldnt-mess-windows (fun &rest args)
  (let ((buffer
         (save-window-excursion
           (when (my/side-window-p (selected-window))
             (select-window
              (display-buffer-pop-up-window
               (current-buffer)
               '((window-side . nil)))))
           (apply fun args)
           (current-buffer))))
    (pop-to-buffer buffer)))

(advice-add #'org-capture
            :around
            #'my/org-capture-shouldnt-mess-windows)

(defun my/org-capture-finalize-shouldnt-mess-windows (fun &rest args)
  (save-window-excursion
    (apply fun args)))

(advice-add #'org-capture-finalize
            :around
            #'my/org-capture-finalize-shouldnt-mess-windows)

(defun my/org-todo-side-window-hack (fun &rest args)
  (save-window-excursion
    (when (my/side-window-p (selected-window))
      (select-window
       (display-buffer-pop-up-window
        (current-buffer)
        '((window-side . nil)))))
    (apply fun args)))

(advice-add #'org-todo
            :around
            #'my/org-todo-side-window-hack)

(provide 'my-org-capture-shouldnt-mess-windows)
;;; my-org-capture-shouldnt-mess-windows.el ends here
