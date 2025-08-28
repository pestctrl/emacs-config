;;; org-roam-update-agenda.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-07-02 10:25]

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
(require 'org-roam-util)

(defvar orua/agenda-files nil)

(defun my/update-org-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (append
         orua/agenda-files
         (my/get-org-roam-files-by-tags '("Project" "active"))
         (my/get-org-roam-most-recent-dailies))))

(defun my/get-org-roam-most-recent-dailies ()
  (let ((roam-daily-directory
         (expand-file-name "daily" org-roam-directory)))
    (when (file-exists-p roam-daily-directory)
      (last (directory-files roam-daily-directory t "^[^#\\.]") 10))))

(advice-add #'org-agenda
            :before
            #'my/update-org-agenda-files)

(defun my/view-org-agenda-files ()
  (interactive)
  (find-file
   (consult--read
    org-agenda-files
    :prompt "org-agenda-files: "
    :sort nil ;; cands are already sorted
    :require-match t
    :state (consult--preview-org-agenda-files)
    :category 'org-roam-node)))

(defun consult--preview-org-agenda-files ()
  "Create preview function for nodes."
  (let ((open (consult--temporary-files))
        (preview (consult--buffer-preview))
        (state  (window-state-get)))
    (lambda (action cand)
      (when (eq action 'exit)
        (progn
          ;; Restore saved window state
          ;; To move point to the original position
          (window-state-put state)
          (funcall open)))
      (funcall preview action
               (and cand
                    (eq action 'preview)
                    (funcall open cand))))))

(provide 'org-roam-update-agenda)
;;; org-roam-update-agenda.el ends here
