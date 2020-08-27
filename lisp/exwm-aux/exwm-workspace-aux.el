;;; exwm-workspace-aux.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-19 10:28]

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
(require 'exwm)
(require 'exwm-randr)
(require 'dash)
(require 'clojure-swap)

(defvar exwm-randr/current-offset 0)

(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

(defun exwm-workspace-after-monitor-change ()
  (let ((monitors (my/get-screens)))
    (setq exwm-workspace-number (length monitors))))

(defun setup-workspace-monitors ()
  (setq exwm-randr/current-offset 0)
  (setq exwm-randr-workspace-monitor-plist
        (loop for m in (my/get-screens)
              for i from 0
              collect i
              collect m)))

(defun my/get-next-workspace-number ()
  (-> exwm-workspace-current-index
      1+
      (mod exwm-workspace-number)))

(defun my/next-workspace ()
  (interactive)
  (exwm-workspace-switch-create (my/get-next-workspace-number)))

(exwm-global-set-key (kbd "<s-tab>") #'my/next-workspace)

(defun my/swap-screens ()
  (interactive)
  (clj-swap exwm-randr/current-offset
            (lambda (x)
              (mod (1+ x)
                   2)))
  (let ((monitors (my/get-screens)))
    (setq exwm-randr-workspace-monitor-plist
          (loop for i from 0 below exwm-workspace-number
                for m = (nth (mod (+ i exwm-randr/current-offset)
                                  exwm-workspace-number)
                             monitors)
                collect i
                collect m)))
  (exwm-randr-refresh)
  (exwm-workspace-switch-create (mod (1- exwm-workspace-current-index)
                                     2)))

(provide 'exwm-workspace-aux)
;;; exwm-workspace-aux.el ends here
