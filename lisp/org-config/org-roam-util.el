;;; org-roam-util.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-01-19 22:02]

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
(require 'org-roam)
(require 'dash)

(defun my/org-roam-filter-by-tag (tag-name)
  (cond ((stringp tag-name)
         (lambda (node)
           (member tag-name (org-roam-node-tags node))))
        ((listp tag-name)
         (lambda (node)
           (let ((tags (org-roam-node-tags node)))
             (-all? (lambda (tag)
                      (member tag tags))
                    tag-name))))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun org-roam-refile (node)
  "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point."
  (interactive
   (list (org-roam-node-read nil nil nil 'require-match)))
  (let* ((regionp (org-region-active-p))
         (region-start (and regionp (region-beginning)))
         (region-end (and regionp (region-end)))
         (file (org-roam-node-file node))
         (nbuf (or (find-buffer-visiting file)
                   (find-file-noselect file)))
         level reversed)
    (if (equal (org-roam-node-at-point) node)
        (user-error "Target is the same as current node")
      (if regionp
          (progn
            (org-kill-new (buffer-substring region-start region-end))
            (org-save-markers-in-region region-start region-end))
        (progn
          (if (org-before-first-heading-p)
              (org-roam-demote-entire-buffer))
          (org-copy-subtree 1 nil t)))
      (with-current-buffer nbuf
        (org-with-wide-buffer
         (goto-char (org-roam-node-point node))
         (setq level (org-get-valid-level (funcall outline-level) 1)
               reversed (org-notes-order-reversed-p))
         (goto-char
          (if reversed
              (or (outline-next-heading) (point-max))
            (or (save-excursion (org-get-next-sibling))
                (org-end-of-subtree t t)
                (point-max))))
         (unless (bolp) (newline))
         (org-paste-subtree level nil nil t)
         (and org-auto-align-tags
              (let ((org-loop-over-headlines-in-active-region nil))
                (org-align-tags)))
         (when (fboundp 'deactivate-mark) (deactivate-mark))))
      (if regionp
          (delete-region (point) (+ (point) (- region-end region-start)))
        (org-preserve-local-variables
         (delete-region
          (and (org-back-to-heading t) (point))
          (min (1+ (buffer-size)) (org-end-of-subtree t t) (point)))))
      ;; If the buffer end-up empty after the refile, kill it and delete its
      ;; associated file.
      (when (eq (buffer-size) 0)
        (if (buffer-file-name)
            (delete-file (buffer-file-name)))
        (set-buffer-modified-p nil)
        ;; If this was done during capture, abort the capture process.
        (when (and org-capture-mode
                   (buffer-base-buffer (current-buffer)))
          (org-capture-kill))
        (kill-buffer (current-buffer))))))

(defun org/heading-to-roam-notes ()
  (interactive)
  (org-back-to-heading)
  (let ((heading (org-get-heading t t t))
        (regexp
         (rx line-start
             "[[" (group (+ nonl))
             "][" (group (+ nonl))
             "]]" line-end)))
    (when (string-match regexp heading)
      (let* ((link (match-string 1 heading))
             (title (match-string 2 heading))
             (roam-title (format "Article: %s" title))
             (article
              (or (cdr (assoc roam-title (org-roam-node-read--completions nil nil)))
                  (let ((article (org-roam-node-create :title roam-title)))
                    (org-roam-capture-
                     :node article
                     :templates (list (car org-roam-capture-templates)))
                    (org-capture-finalize)
                    (cdr (assoc roam-title (org-roam-node-read--completions nil nil)))))))
        (org-roam-refile article)
        (let ((m (org-roam-node-marker article)))
          (pop-to-buffer-same-window (marker-buffer m)))))))

(provide 'org-roam-util)
;;; org-roam-util.el ends here
