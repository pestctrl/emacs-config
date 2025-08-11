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

(require 'org)

(defun my/side-window-p (window)
  (window-parameter window 'window-side))

(defun my/org-capture-display-use-side-window (buffer alist)
  ;; I think the alist argument is never used
  (pop-to-buffer
   buffer

   '(display-buffer-in-side-window
     (side . left)
     (window-width . 85)
     (window-parameters
      . ((no-delete-other-windows . t)
         ;;(dedicated . t)
         )))))

(advice-add #'org-display-buffer-split
            :override
            #'my/org-capture-display-use-side-window)

(defun my/window-dedicated-p (orig &optional window)
  ;; Excuse me? Side windows are not dedicated.
  (or (and (not org-capture-mode)
           (funcall orig window))
      (eq (funcall orig window)
          t)))

(advice-add #'window-dedicated-p
            :around
            #'my/window-dedicated-p)

(defun my/org-capture-finalize-shouldnt-mess-windows (orig &rest args)
  (cl-letf* ((orig-set-window-configuration (symbol-function 'set-window-configuration))

             ((symbol-function 'set-window-configuration)
              #'(lambda (&rest args)
                  (unless (equal (car args) (org-capture-get :return-to-wconf 'local))
                    (apply orig-set-window-configuration args)))))
    (apply orig args)))

(advice-add #'org-capture-finalize
            :around
            #'my/org-capture-finalize-shouldnt-mess-windows)

(defun my/org-todo-side-window-hack (fun &rest args)
  (save-window-excursion
    (set-window-parameter (selected-window) 'window-side nil)
    (let ((window--sides-inhibit-check t))
      (apply fun args))))

(advice-add #'org-todo
            :around
            #'my/org-todo-side-window-hack)

(defun my/get-first-parent-vertical-window-combination (window)
  (let ((win window)
        found)
    (while (and win (not found))
      ;; not a vertical combination
      (when (not (window-valid-p win))
        (throw 'invalid-window 'invalid-window))
      (if (or (not (window-combination-p win nil)))
          (setq win (window-parent win))
        (setq found win)))
    found))

;; TODO(pestctrl): This function will rebalanace a whole
;; subtree. Would be nice if the routine only balanced the first
;; window-combination child encountered, instead of recursively
;; walking the whole tree, balancing everything.
(defun my/rebalance-windows-vertical (window)
  (interactive
   (list
    (or (my/get-first-parent-vertical-window-combination
         (selected-window))
	    (error "Not a window or frame %s" window-or-frame))))
  (unless (window-combination-p window nil)
    (error "Not a vertical window combination"))
  ;; Balance vertically.
  (let ((frame (window-frame window)))
    (window--resize-reset frame)
    (balance-windows-1 window nil)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame))))

(defun my/balance-windows-after-delete-side (fun &optional window)
  (let* ((win (or window (selected-window)))
         (parent-win (window-parent win))
         (should-rebalance
          (and (my/side-window-p win)
               (my/side-window-p parent-win)
               (< 2 (window-child-count parent-win)))))
    (funcall fun win)
    (when should-rebalance
      (when (eq 'invalid-window
                (catch 'invalid-window
                  (my/rebalance-windows-vertical
                   (my/get-first-parent-vertical-window-combination
                    parent-win))))
        (message "Ohhh, some kind of window-error happened")))))

(advice-add #'delete-window
            :around
            #'my/balance-windows-after-delete-side)

(provide 'my-org-capture-shouldnt-mess-windows)
;;; my-org-capture-shouldnt-mess-windows.el ends here
