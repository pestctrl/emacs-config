;;; org-roam-convert-to-notes.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-12-25 13:53]

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

(defun my/org-roam-convert-to-notes ()
  (interactive)
  (org-back-to-heading)
  (let ((url (org-entry-get (point) "URL"))
        (heading (org-get-heading t t t t))
        (buff (current-buffer))
        notes-buffer
        heading-text
        node-title node)
    (string-match (rx "[[" (+ (not "]")) "][" (group (+ (not "]"))) "]]")
                  heading)
    (setq heading-text (match-string 1 heading))

    (setq node-title (concat "Web Notes: " heading-text))

    (let ((candidates (org-roam-node-read--completions
                       (lambda (node)
                         (string-match-p (concat "^" node-title)
                                         (org-roam-node-title node))))))
      (when (> (length candidates) 0)
        (org-capture-kill)
        (org-roam-node-visit (cdar candidates))
        (user-error "Notes already exist on this URL!")))

    (setq node (org-roam-node-create
                :title
                node-title))

    (org-roam-capture-
     :node node
     :props '(:finalize find-file)
     :templates
     `(("d" "default" plain "%?" :target
        (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                   ,(string-join
                     (list
                      "#+title: ${title}"
                      "#+filetags: web_notes"
                      ""
                      (format "Link: %s" url)
                      "Author: %^{Author? }"
                      "")
                     "\n"))
        :unnarrowed t)))

    (setq notes-buffer (current-buffer))

    (save-window-excursion
      (with-current-buffer buff
        (my/org-board-prompt)
        (org-cut-subtree)
        (with-current-buffer notes-buffer
          (save-excursion
            (goto-char (point-max))
            (yank)))
        (org-capture-kill)))))

(provide 'org-roam-convert-to-notes)
;;; org-roam-convert-to-notes.el ends here
