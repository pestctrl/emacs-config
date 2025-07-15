;;; my-org-roam-journal.el ---  -*- lexical-binding: t -*-

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
(require 'org-roam-util)

(defvar my/current-logger-cache (make-hash-table))
(defvar my/org-roam-logger-filter-fun nil)

(defvar my/org-roam-logger-templates
  '(("j" "default" entry "* %<%H:%M> %?"
     :unnarrowed t
     :target
     (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n"
                    ("Journal" "%<%b %d, %Y>")))
    ("J" "Journal with source" entry "* %<%H:%M> %?\n:PROPERTIES:\n:LOCATION: %a\n:END:"
     :unnarrowed t
     :target
     (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n"
                    ("Journal" "%<%b %d, %Y>")))
    ("s" "source location" entry "* %^{Short Description? }\n:PROPERTIES:\n:DATE: [%<%Y-%m-%d %H:%M>]\n:LOCATION: %a\n:END:\n\n%?"
     :unnarrowed t
     :target
     (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n"
                    ("Reference")))))

(defun my/org-roam-logger-capture-current (arg)
  (interactive "P")
  (let* ((tab-sym (intern (alist-get 'name (tab-bar--current-tab))))
         (node
          (or (and (not (equal arg '(16)))
                   (not (equal arg '(64)))
                   (gethash tab-sym my/current-logger-cache))
              (puthash tab-sym
                       (org-roam-node-read
                        nil
                        (when (not (equal arg '(64)))
                          my/org-roam-logger-filter-fun))
                       my/current-logger-cache))))
    ;; On NEW nodes, org-roam-node-read generates an empty struct with
    ;; only a few things, one of which being an id. Do a sanity check to
    ;; make sure that we re-init the current node with a node that has
    ;; the file name. Only do this initialization if we have an ID for
    ;; the org-roam.
    ;;
    ;; ASSUMPTION: org-roam-capture- initializes node with
    ;; org-roam-node-id field.
    ;;
    ;; TODO: Ummm, this doesn't work in the case where the file gets deleted
    ;; after a first capture + capture abort.
    (when (and (null (org-roam-node-file node))
               (org-roam-node-id node))
      (setq node
            (org-roam-node-from-id (org-roam-node-id node))))

    (if (equal arg '(4))
        (-> node
            (org-roam-node-file)
            (find-file-noselect)
            (pop-to-buffer-same-window))
      (org-roam-capture-
       :node node
       :templates my/org-roam-logger-templates))))

(provide 'my-org-roam-logger)
;;; my-org-roam-logger.el ends here
