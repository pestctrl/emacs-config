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

(defun my/org-capture-finalize-shouldnt-mess-windows (&rest args)
  (save-window-excursion
    ;; This basically means that we don't change window configurations when
    ;; there are previous buffers to be seen. IDK if this will have unintended
    ;; consequences, because...
    (when (zerop (length (window-prev-buffers)))
      (delete-window))
    ;; current-window-configuration will encode a buffer that's about to be
    ;; deleted. I tested it, and it does what I want, so maybe there's no
    ;; problem?
    (org-capture-put :return-to-wconf (current-window-configuration))))

(advice-add #'org-capture-finalize
            :before
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
  ;; Balance vertically.
  (let ((frame (window-frame window)))
    (window--resize-reset frame)
    (balance-windows-1 window)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame))))

(defun my/balance-windows-after-delete-side (fun &optional window)
  (let* ((win (or window (selected-window)))
         (parent-win (window-parent win))
         (should-rebalance
          (and (my/side-window-p win)
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
