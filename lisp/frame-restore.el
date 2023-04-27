;;; frame-restore.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-10-13 09:25]

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
(require 'frameset)

(defvar my/framelist nil)

(defun fr/save-if-appropriate ()
  (interactive)
  (let ((non-tty-frames
         (remove-if-not #'(lambda (frame)
                            (with-selected-frame frame
                              window-system))
                        (visible-frame-list))))
    (when (not (zerop (length non-tty-frames)))
      (setq my/framelist
            (frameset-save (visible-frame-list))))))

(run-at-time nil 60 #'fr/save-if-appropriate)

(defun fr/restore ()
  (interactive)
  (let ((frameset--target-display `(display . ,(getenv "DISPLAY"))))
    (frameset-restore
     my/framelist
     :force-display nil
     :reuse-frames t)
    (dolist (f (frame-list))
      (unless (frame-visible-p f)
        (delete-frame f)))))

(provide 'frame-restore)
;;; frame-restore.el ends here
