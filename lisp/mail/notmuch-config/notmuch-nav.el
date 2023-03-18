;;; notmuch-nav.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-20 22:31]

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

(defun notmuch-tree-up-thread ()
  (interactive)
  (let ((l (1- (length (notmuch-tree-get-prop :tree-status)))))
    (while (progn (previous-line)
                  (< l (length (notmuch-tree-get-prop :tree-status)))))))

(defun notmuch-tree-message (&optional prev pop-at-end)
  "Move to the next or previous matching message"
  (interactive "P")
  (forward-line (if prev -1 nil))
  (if pop-at-end
      (notmuch-tree-quit pop-at-end)
    (when (window-live-p notmuch-tree-message-window)
      (notmuch-tree-show-message-in))))

(defun notmuch-tree-prev-message (&optional pop-at-end)
  "Move to previous matching message."
  (interactive "P")
  (notmuch-tree-message t pop-at-end))

(defun notmuch-tree-next-message (&optional pop-at-end)
  "Move to next matching message."
  (interactive "P")
  (notmuch-tree-message nil pop-at-end))

(defun notmuch-tree-next-sibling ()
  (interactive)
  (let ((l (length (notmuch-tree-get-prop :tree-status)))
        (orig (point)))
    (while (progn (next-line)
                  (and (not (bobp))
                       (> (length (notmuch-tree-get-prop :tree-status)) l))))
    (if (= l (length (notmuch-tree-get-prop :tree-status)))
        (when (window-live-p notmuch-tree-message-window)
          (notmuch-tree-show-message-in))
      (goto-char orig)
      (message "No next sibling"))))

(defun notmuch-tree-prev-sibling ()
  (interactive)
  (let ((l (length (notmuch-tree-get-prop :tree-status))))
    (while (progn (previous-line)
                  (and (not (bobp))
                       (< l (length (notmuch-tree-get-prop :tree-status)))))))
  (when (window-live-p notmuch-tree-message-window)
    (notmuch-tree-show-message-in)))

(provide 'notmuch-nav)
;;; notmuch-nav.el ends here
