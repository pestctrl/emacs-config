;;; transient-bug.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-11-17 13:32]

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
(require 'transient)

(defun my/transient--show ()
  (transient--timer-cancel)
  (setq transient--showp t)
  (let ((transient--shadowed-buffer (current-buffer))
        (focus nil))
    (setq transient--buffer (get-buffer-create transient--buffer-name))
    (with-current-buffer transient--buffer
      (when transient-enable-popup-navigation
        (setq focus (or (button-get (point) 'command)
                        (and (not (bobp))
                             (button-get (1- (point)) 'command))
                        (transient--heading-at-point))))
      (erase-buffer)
      (run-hooks 'transient-setup-buffer-hook)
      (when transient-force-fixed-pitch
        (transient--force-fixed-pitch))
      (setq window-size-fixed (if (window-full-height-p) 'width t))
      (when (bound-and-true-p tab-line-format)
        (setq tab-line-format nil))
      (setq header-line-format nil)
      (setq mode-line-format
            (if (or (natnump transient-mode-line-format)
                    (eq transient-mode-line-format 'line))
                nil
              transient-mode-line-format))
      (setq mode-line-buffer-identification
            (symbol-name (oref transient--prefix command)))
      (if transient-enable-popup-navigation
          (setq-local cursor-in-non-selected-windows 'box)
        (setq cursor-type nil))
      (setq display-line-numbers nil)
      (setq show-trailing-whitespace nil)
      (transient--insert-groups)
      (when (or transient--helpp transient--editp)
        (transient--insert-help))
      (when-let ((line (transient--separator-line)))
        (insert line)))
    (unless (window-live-p transient--window)
      (setq transient--window
            (display-buffer transient--buffer
                            transient-display-buffer-action)))
    (when (window-live-p transient--window)
      (with-selected-window transient--window
        (goto-char (point-min))
        (when transient-enable-popup-navigation
          (transient--goto-button focus))
        (transient--fit-window-to-buffer transient--window)))))

(advice-add #'transient--show
            :override
            #'my/transient--show)

(provide 'transient-bug)
;;; transient-bug.el ends here
