;;; org-scan-tags-indent.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-08-23 09:55]

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
(require 'cl)

(defun get-parent-indent-level ()
  (save-excursion
    (let ((levels 0))
      (while (and (org-up-heading-safe)
                  (org-get-todo-state))
        (when (not (string= "CAT" (org-get-todo-state)))
          (cl-incf levels)))
      levels)))

(defun my/org-scan-tags (action matcher todo-only &optional start-level)
  "Scan headline tags with inheritance and produce output ACTION.

    ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
    or `agenda' to produce an entry list for an agenda view.  It can also be
    a Lisp form or a function that should be called at each matched headline, in
    this case the return value is a list of all return values from these calls.

    MATCHER is a function accepting three arguments, returning
    a non-nil value whenever a given set of tags qualifies a headline
    for inclusion.  See `org-make-tags-matcher' for more information.
    As a special case, it can also be set to t (respectively nil) in
    order to match all (respectively none) headline.

    When TODO-ONLY is non-nil, only lines with a TODO keyword are
    included in the output.

    START-LEVEL can be a string with asterisks, reducing the scope to
    headlines matching this string."
  (require 'org-agenda)
  (let* ((re (concat "^"
                     (if start-level
                         ;; Get the correct level to match
                         (concat "\\*\\{" (number-to-string start-level) "\\} ")
                       org-outline-regexp)
                     " *\\(" (regexp-opt org-todo-keywords-1 'words) "\\)?"
                     " *\\(.*?\\)\\([ \t]:\\(?:" org-tag-re ":\\)+\\)?[ \t]*$"))
         (props (list 'face 'default
                      'done-face 'org-agenda-done
                      'undone-face 'default
                      'mouse-face 'highlight
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'help-echo
                      (format "mouse-2 or RET jump to Org file %S"
                              (abbreviate-file-name
                               (or (buffer-file-name (buffer-base-buffer))
                                   (buffer-name (buffer-base-buffer)))))))
         (org-map-continue-from nil)
         lspos tags tags-list
         (tags-alist (list (cons 0 org-file-tags)))
         (llast 0) rtn rtn1 level category i txt
         todo marker entry priority
         ts-date ts-date-type ts-date-pair)
    (unless (or (member action '(agenda sparse-tree)) (functionp action))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
        (org-overview)
        (org-remove-occur-highlights))
      (while (let (case-fold-search)
               (re-search-forward re nil t))
        (setq org-map-continue-from nil)
        (catch :skip
          ;; Ignore closing parts of inline tasks.
          (when (and (fboundp 'org-inlinetask-end-p) (org-inlinetask-end-p))
            (throw :skip t))
          (setq todo (and (match-end 1) (match-string-no-properties 1)))
          (setq tags (and (match-end 4) (org-trim (match-string-no-properties 4))))
          (goto-char (setq lspos (match-beginning 0)))
          (setq level (org-reduced-level (org-outline-level))
                category (org-get-category))
          (when (eq action 'agenda)
            (setq ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
                  ts-date (car ts-date-pair)
                  ts-date-type (cdr ts-date-pair)))
          (setq i llast llast level)
          ;; remove tag lists from same and sublevels
          (while (>= i level)
            (when (setq entry (assoc i tags-alist))
              (setq tags-alist (delete entry tags-alist)))
            (setq i (1- i)))
          ;; add the next tags
          (when tags
            (setq tags (org-split-string tags ":")
                  tags-alist
                  (cons (cons level tags) tags-alist)))
          ;; compile tags for current headline
          (setq tags-list
                (if org-use-tag-inheritance
                    (apply 'append (mapcar 'cdr (reverse tags-alist)))
                  tags)
                org-scanner-tags tags-list)
          (when org-use-tag-inheritance
            (setcdr (car tags-alist)
                    (mapcar (lambda (x)
                              (setq x (copy-sequence x))
                              (org-add-prop-inherited x))
                            (cdar tags-alist))))
          (when (and tags org-use-tag-inheritance
                     (or (not (eq t org-use-tag-inheritance))
                         org-tags-exclude-from-inheritance))
            ;; Selective inheritance, remove uninherited ones.
            (setcdr (car tags-alist)
                    (org-remove-uninherited-tags (cdar tags-alist))))
          (when (and

                 ;; eval matcher only when the todo condition is OK
                 (and (or (not todo-only) (member todo org-todo-keywords-1))
                      (if (functionp matcher)
                          (let ((case-fold-search t) (org-trust-scanner-tags t))
                            (funcall matcher todo tags-list level))
                        matcher))

                 ;; Call the skipper, but return t if it does not
                 ;; skip, so that the `and' form continues evaluating.
                 (progn
                   (unless (eq action 'sparse-tree) (org-agenda-skip))
                   t)

                 ;; Check if timestamps are deselecting this entry
                 (or (not todo-only)
                     (and (member todo org-todo-keywords-1)
                          (or (not org-agenda-tags-todo-honor-ignore-options)
                              (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item))))))

            ;; select this headline
            (cond
             ((eq action 'sparse-tree)
              (and org-highlight-sparse-tree-matches
                   (org-get-heading) (match-end 0)
                   (org-highlight-new-match
                    (match-beginning 1) (match-end 1)))
              (org-show-context 'tags-tree))
             ((eq action 'agenda)
              (setq txt (org-agenda-format-item
                         ""
                         (concat
                          (if (eq org-tags-match-list-sublevels 'indented)
                              (make-string (get-parent-indent-level) ?.) "")
                          (org-get-heading))
                         (make-string level ?\s)
                         category
                         tags-list)
                    priority (org-get-priority txt))
              (goto-char lspos)
              (setq marker (org-agenda-new-marker))
              (org-add-props txt props
                'org-marker marker 'org-hd-marker marker 'org-category category
                'todo-state todo
                'ts-date ts-date
                'priority priority
                'type (concat "tagsmatch" ts-date-type))
              (push txt rtn))
             ((functionp action)
              (setq org-map-continue-from nil)
              (save-excursion
                (setq rtn1 (funcall action))
                (push rtn1 rtn)))
             (t (user-error "Invalid action")))

            ;; if we are to skip sublevels, jump to end of subtree
            (unless org-tags-match-list-sublevels
              (org-end-of-subtree t)
              (backward-char 1))))
        ;; Get the correct position from where to continue
        (if org-map-continue-from
            (goto-char org-map-continue-from)
          (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
               (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(defun my/org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
  The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq todo-only (car org-agenda-overriding-arguments)
          match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
          org-tags-match-list-sublevels)
         (completion-ignore-case t)
         (org--matcher-tags-todo-only todo-only)
         rtn rtnall files file pos matcher
         buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (catch 'exit
      ;; TODO: this code is repeated a lot...
      (when org-agenda-sticky
        (setq org-agenda-buffer-name
              (if (stringp match)
                  (format "*Org Agenda(%s:%s)*"
                          (or org-keys (or (and todo-only "M") "m")) match)
                (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
      (setq matcher (org-make-tags-matcher match))
      ;; Prepare agendas (and `org-tag-alist-for-agenda') before
      ;; expanding tags within `org-make-tags-matcher'
      (org-agenda-prepare (concat "TAGS " match))
      (setq match (car matcher)
            matcher (cdr matcher))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
            (list 'org-tags-view
                  `(quote ,org--matcher-tags-todo-only)
                  `(if current-prefix-arg nil ,org-agenda-query-string)))
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq buffer (if (file-exists-p file)
                           (org-get-agenda-file-buffer file)
                         (error "No such file %s" file)))
          (if (not buffer)
              ;; If file does not exist, error message to agenda
              (setq rtn (list
                         (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                    rtnall (append rtnall rtn))
            (with-current-buffer buffer
              (unless (derived-mode-p 'org-mode)
                (error "Agenda file %s is not in Org mode" file))
              (save-excursion
                (save-restriction
                  (if (eq buffer org-agenda-restrict)
                      (narrow-to-region org-agenda-restrict-begin
                                        org-agenda-restrict-end)
                    (widen))
                  (setq rtn (org-scan-tags 'agenda
                                           matcher
                                           org--matcher-tags-todo-only))
                  (setq rtnall (append rtnall rtn))))))))
      (org-agenda--insert-overriding-header
        (with-temp-buffer
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert (substitute-command-keys
                     "Press \
  \\<org-agenda-mode-map>`\\[universal-argument] \\[org-agenda-redo]' \
  to search again\n")))
          (add-text-properties pos (1- (point))
                               (list 'face 'org-agenda-structure))
          (buffer-string)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
        (insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties
       (point-min) (point-max)
       `(org-agenda-type tags
                         org-last-args (,org--matcher-tags-todo-only ,match)
                         org-redo-cmd ,org-agenda-redo-command
                         org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

(advice-add 'org-scan-tags
            :override
            #'my/org-scan-tags)

(provide 'org-scan-tags-indent)
;;; org-scan-tags-indent.el ends here
