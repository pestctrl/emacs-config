;;; tab-bar-patch.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-03-24 16:08]

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

(advice-add #'tab-bar-new-tab-to
            :override
            #'my/tab-bar-new-tab-to)

(defun my/tab-bar-new-tab-to (&optional tab-number)
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (from-tab (tab-bar--tab)))

    (when tab-bar-new-tab-choice
      ;; Handle the case when it's called in the active minibuffer.
      (when (minibuffer-selected-window)
        (select-window (minibuffer-selected-window)))
      ;; Remove window parameters that can cause problems
      ;; with `delete-other-windows' and `split-window'.
      (unless (eq tab-bar-new-tab-choice 'clone)
        (set-window-parameter nil 'window-atom nil)
        (let ((side (window-parameter nil 'window-side)))
          (when side
            (walk-window-tree
             (lambda (window)
               (when (eq side (window-parameter window 'window-side))
                 (set-window-parameter window 'window-side nil)))
             nil t))))
      (let ((ignore-window-parameters t))
        (if (eq tab-bar-new-tab-choice 'clone)
            ;; Create new unique windows with the same layout
            (window-state-put (window-state-get))
          (delete-other-windows)
          (if (eq tab-bar-new-tab-choice 'window)
              ;; Create new unique window from remaining window
              (window-state-put (window-state-get))
            ;; Create a new window to get rid of old window parameters
            ;; (e.g. prev/next buffers) of old window.
            (split-window) (delete-window))))

      (let ((buffer
             (if (and (functionp tab-bar-new-tab-choice)
                      (not (memq tab-bar-new-tab-choice '(clone window))))
                 (funcall tab-bar-new-tab-choice)
               (if (stringp tab-bar-new-tab-choice)
                   (or (get-buffer tab-bar-new-tab-choice)
                       (find-file-noselect tab-bar-new-tab-choice))))))
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer))))

    (when from-index
      (setf (nth from-index tabs) from-tab))

    (let* ((to-tab (tab-bar--current-tab-make
                    (when (eq tab-bar-new-tab-group t)
                      `((group . ,(alist-get 'group from-tab))))))
           (to-number (and tab-number (prefix-numeric-value tab-number)))
           (to-index (or (if to-number
                             (if (< to-number 0)
                                 (+ (length tabs) (1+ to-number))
                               (1- to-number)))
                         (pcase tab-bar-new-tab-to
                           ('leftmost 0)
                           ('rightmost (length tabs))
                           ('left (or from-index 1))
                           ('right (1+ (or from-index 0)))
                           ((pred functionp)
                            (funcall tab-bar-new-tab-to))))))
      (setq to-index (max 0 (min (or to-index 0) (length tabs))))
      (cl-pushnew to-tab (nthcdr to-index tabs))

      (when (eq to-index 0)
        ;; `pushnew' handles the head of tabs but not frame-parameter
        (tab-bar-tabs-set tabs))

      (when tab-bar-history-mode
        (puthash (selected-frame) nil tab-bar-history-back)
        (puthash (selected-frame) nil tab-bar-history-forward)
        (setq tab-bar-history-omit t))

      (run-hook-with-args 'tab-bar-tab-post-open-functions
                          (nth to-index tabs)))

    (when tab-bar-show
      (if (not tab-bar-mode)
          ;; Turn on `tab-bar-mode' since a tab was created.
          ;; Note: this also updates `tab-bar-lines'.
          (tab-bar-mode 1)
        (tab-bar--update-tab-bar-lines)))

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Added new tab at %s" tab-bar-new-tab-to))))

(provide 'tab-bar-patch)
;;; tab-bar-patch.el ends here
