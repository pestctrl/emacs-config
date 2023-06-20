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

(defun fr/non-tty-frames ()
  (remove-if-not #'(lambda (frame)
                     (with-selected-frame frame
                       (and window-system
                            (not (string= "F1" (frame-parameter frame 'name))))))
                 (visible-frame-list)))

(defvar fr/working-state nil)

(defun fr/give-up-working-state ()
  (interactive)
  (setq fr/working-state t))

(fr/give-up-working-state)

(add-to-list 'server-after-make-frame-hook
             (lambda (&rest _)
               (when (<= (length (fr/non-tty-frames)) 1)
                 (setq fr/working-state nil))))

(defun fr/save-if-appropriate (&rest _)
  (interactive)
  (let ((non-tty-frames
         (fr/non-tty-frames)))
    (when (and fr/working-state
               (not (zerop (length (fr/non-tty-frames)))))
      (setq my/framelist
            (frameset-save (fr/non-tty-frames)))
      (message "%s Framelist saved!"
               (format-time-string "%Y-%m-%d %H:%M:%S")))))

(run-at-time nil (* 60 2) #'fr/save-if-appropriate)

(defun keep-display-ignore-daemon (ret)
  (if (and ret
           (daemonp))
      nil
    ret))

(advice-add #'frameset-keep-original-display-p
            :filter-return
            #'keep-display-ignore-daemon)

(defun fr/restore ()
  (interactive)
  (setq fr/working-state t)
  (frameset-restore
   my/framelist
   :force-display t
   :reuse-frames t)
  (dolist (f (frame-list))
    (unless (frame-visible-p f)
      (delete-frame f))))

(provide 'frame-restore)
;;; frame-restore.el ends here
