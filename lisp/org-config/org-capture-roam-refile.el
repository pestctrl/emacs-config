;;; org-capture-roam-refile.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-04-29 16:27]

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
(require 'org-capture)

(define-minor-mode org-capture-mode
  "Minor mode for special key bindings in a capture buffer.

Turning on this mode runs the normal hook `org-capture-mode-hook'."
  :lighter " Cap"
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<org-capture-mode-map>Capture buffer.  Finish \
`\\[org-capture-finalize]', refile `\\[org-capture-refile]', \
abort `\\[org-capture-kill]', org-roam-refile `\\[org-capture-roam-refile]'.")))

(defun org-capture-roam-refile ()
  "Finalize the current capture and then refile the entry.
Refiling is done from the base buffer, because the indirect buffer is then
already gone.  Any prefix argument will be passed to the refile command."
  (interactive)
  (unless (eq (org-capture-get :type 'local) 'entry)
    (user-error "Refiling from a capture buffer makes only sense \
for `entry'-type templates"))
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
	     (pos (make-marker))
	     (org-capture-is-refiling t)
	     (kill-buffer (org-capture-get :kill-buffer 'local))
	     (jump-to-captured (org-capture-get :jump-to-captured 'local))
	     (refile-targets (org-capture-get :refile-targets 'local)))
    ;; Since `org-capture-finalize' may alter buffer contents (e.g.,
    ;; empty lines) around entry, use a marker to refer to the
    ;; headline to be refiled.  Place the marker in the base buffer,
    ;; as the current indirect one is going to be killed.
    (set-marker pos (save-excursion (org-back-to-heading t) (point)) base)
    ;; `org-capture-finalize' calls `org-capture-goto-last-stored' too
    ;; early.  We want to wait for the refiling to be over, so we
    ;; control when the latter function is called.
    (org-capture-put :kill-buffer nil :jump-to-captured nil)
    (let ((org-refile-targets (or refile-targets org-refile-targets)))
      (org-capture-finalize)
      (save-window-excursion
        (with-current-buffer base
	      (org-with-point-at pos
	        (call-interactively 'org-roam-refile)))))
    (when kill-buffer
      (with-current-buffer base (save-buffer))
      (kill-buffer base))
    (when jump-to-captured (org-capture-goto-last-stored))))

(provide 'org-capture-roam-refile)
;;; org-capture-roam-refile.el ends here
