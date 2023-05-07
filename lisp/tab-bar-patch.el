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

(advice-add #'tab-bar-select-tab
            :override
            #'my/tab-bar-select-tab)

(defun my/tab-bar-select-tab (&optional tab-number)
  "Switch to the tab by its absolute position TAB-NUMBER in the tab bar.
When this command is bound to a numeric key (with a key prefix or modifier key
using `tab-bar-select-tab-modifiers'), calling it without an argument
will translate its bound numeric key to the numeric argument.
Also the prefix argument TAB-NUMBER can be used to override
the numeric key, so it takes precedence over the bound digit key.
For example, `<MODIFIER>-2' will select the second tab, but `C-u 15
<MODIFIER>-2' will select the 15th tab.  TAB-NUMBER counts from 1.
Negative TAB-NUMBER counts tabs from the end of the tab bar."
  (interactive "P")
  (unless (integerp tab-number)
    (let ((key (event-basic-type last-command-event)))
      (setq tab-number (if (and (characterp key) (>= key ?1) (<= key ?9))
                           (- key ?0)
                         0))))

  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-number (cond ((< tab-number 0) (+ (length tabs) (1+ tab-number)))
                          ((zerop tab-number) (1+ from-index))
                          (t tab-number)))
         (to-index (1- (max 1 (min to-number (length tabs)))))
         (minibuffer-was-active (minibuffer-window-active-p (selected-window))))

    (unless (eq from-index to-index)
      (let* ((from-tab (tab-bar--tab))
             (to-tab (nth to-index tabs))
             (wc (alist-get 'wc to-tab))
             (ws (alist-get 'ws to-tab)))

        ;; During the same session, use window-configuration to switch
        ;; tabs, because window-configurations are more reliable
        ;; (they keep references to live buffers) than window-states.
        ;; But after restoring tabs from a previously saved session,
        ;; its value of window-configuration is unreadable,
        ;; so restore its saved window-state.
        (cond
         ((and (window-configuration-p wc)
               ;; Check for such cases as cloning a frame with tabs.
               ;; When tabs were cloned to another frame, then fall back
               ;; to using `window-state-put' below.
               (eq (window-configuration-frame wc) (selected-frame)))
          (let ((wc-point (alist-get 'wc-point to-tab))
                (wc-bl  (seq-filter #'buffer-live-p (alist-get 'wc-bl to-tab)))
                (wc-bbl (seq-filter #'buffer-live-p (alist-get 'wc-bbl to-tab)))
                (wc-history-back (alist-get 'wc-history-back to-tab))
                (wc-history-forward (alist-get 'wc-history-forward to-tab)))

            (set-window-configuration wc nil t)

            ;; set-window-configuration does not restore the value of
            ;; point in the current buffer, so restore it separately.
            (when (and (markerp wc-point)
                       (marker-buffer wc-point)
                       ;; FIXME: After dired-revert, marker relocates to 1.
                       ;; window-configuration restores point to global point
                       ;; in this dired buffer, not to its window point,
                       ;; but this is slightly better than 1.
                       ;; Maybe better to save dired-filename in each window?
                       (not (eq 1 (marker-position wc-point))))
              (goto-char wc-point))

            (when wc-bl  (set-frame-parameter nil 'buffer-list wc-bl))
            (when wc-bbl (set-frame-parameter nil 'buried-buffer-list wc-bbl))

            (when tab-bar-history-mode
              (puthash (selected-frame)
                       (and (window-configuration-p (alist-get 'wc (car wc-history-back)))
                            wc-history-back)
                       tab-bar-history-back)
              (puthash (selected-frame)
                       (and (window-configuration-p (alist-get 'wc (car wc-history-forward)))
                            wc-history-forward)
                       tab-bar-history-forward))))

         (ws
          ;; `window-state-put' fails when called in the minibuffer
          (when (minibuffer-selected-window)
            (select-window (minibuffer-selected-window)))
          (let ((window--sides-inhibit-check t))
            (set-window-parameter (selected-window) 'window-side nil)
            (window-state-put ws nil 'safe))))

        ;; Select the minibuffer when it was active before switching tabs
        (when (and minibuffer-was-active (active-minibuffer-window))
          (select-window (active-minibuffer-window)))

        ;; When the minibuffer was activated in one tab, but exited in
        ;; another tab, then after going back to the first tab, it has
        ;; such inconsistent state that the current buffer is the minibuffer,
        ;; but its window is not active.  So try to undo this mess.
        (when (and (minibufferp) (not (active-minibuffer-window)))
          (other-window 1))

        (when tab-bar-history-mode
          (setq tab-bar-history-omit t))

        (when from-index
          (setf (nth from-index tabs) from-tab))
        (setf (nth to-index tabs) (tab-bar--current-tab-make (nth to-index tabs)))

        (unless tab-bar-mode
          (message "Selected tab '%s'" (alist-get 'name to-tab))))

      (force-mode-line-update))))

(defun my/tab-bar-new-tab-to (&optional tab-number)
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (from-tab (tab-bar--tab)))

    (when tab-bar-new-tab-choice
      ;; Handle the case when it's called in the active minibuffer.
      (when (minibuffer-selected-window)
        (select-window (minibuffer-selected-window)))
      (let ((ignore-window-parameters t)
            (window--sides-inhibit-check t))
        (if (eq tab-bar-new-tab-choice 'clone)
            ;; Create new unique windows with the same layout
            (window-state-put (window-state-get))
          ;; Remove window parameters that can cause problems
          ;; with `delete-other-windows' and `split-window'.
          (set-window-parameter nil 'window-side nil)
          (set-window-parameter nil 'window-atom nil)
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
