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

(defun my/rebalance-windows-vertical (&optional window-or-frame)
  (interactive)
  (let* ((window
	      (cond
	       ((or (not window-or-frame)
		        (frame-live-p window-or-frame))
	        (frame-root-window window-or-frame))
	       ((or (window-live-p window-or-frame)
		        (window-child window-or-frame))
	        window-or-frame)
	       (t
	        (error "Not a window or frame %s" window-or-frame))))
	     (frame (window-frame window)))
    ;; Balance vertically.
    (window--resize-reset (window-frame window))
    (balance-windows-1 window)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame))))

(defun my/balance-windows-after-delete-side (fun &optional window)
  (let* ((win (or window (selected-window)))
         (should-rebalance
          (my/side-window-p win)))
    (funcall fun win)
    (when should-rebalance
      (-->
       (window-list)
       (remove-if-not #'my/side-window-p it)
       (first it)
       (with-selected-window it
         (call-interactively #'my/rebalance-windows-vertical))))))

(advice-add #'delete-window
            :around
            #'my/balance-windows-after-delete-side)

(provide 'my-org-capture-shouldnt-mess-windows)
;;; my-org-capture-shouldnt-mess-windows.el ends here
