(require 'org)

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                    (point-min)
                    (point)))
             (end (if globalp
                    (point-max)
                    (if (eq state 'children)
                      (save-excursion
                        (outline-next-heading)
                        (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                       (save-excursion
                         (outline-next-heading)
                           (point)))
                     (msg (format
                            (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                            (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                  (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))


(defun my/org-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	      (progn
	        (beginning-of-line)
	        (setq struct (org-list-struct))
	        (setq eoh (point-at-eol))
	        (setq eos (org-list-get-item-end-before-blank (point) struct))
	        (setq has-children (org-list-has-child-p (point) struct)))
	    (org-back-to-heading)
	    (setq eoh (save-excursion (outline-end-of-heading) (point)))
	    (setq eos (save-excursion
		            (org-end-of-subtree t t)
		            (unless (eobp) (forward-char -1))
		            (point)))
	    (setq has-children
	          (or
	           (save-excursion
		         (let ((level (funcall outline-level)))
		           (outline-next-heading)
		           (and (org-at-heading-p t)
			            (> (funcall outline-level) level))))
	           (and (eq org-cycle-include-plain-lists 'integrate)
		            (save-excursion
		              (org-list-search-forward (org-item-beginning-re) eos t))))))
      ;; Determine end invisible part of buffer (EOL)
      (beginning-of-line 2)
      (while (and (not (eobp))          ;this is like `next-line'
		          (get-char-property (1- (point)) 'invisible))
	    (goto-char (next-single-char-property-change (point) 'invisible))
	    (and (eolp) (beginning-of-line 2)))
      (setq eol (point)))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-pre-cycle-hook 'empty))
      (org-unlogged-message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
	    (goto-char eos)
	    (outline-next-heading)
	    (when (org-invisible-p) (org-flag-heading nil))))
     ((and (or (>= eol eos)
	           (not (string-match "\\S-" (buffer-substring eol eos))))
	       (or has-children
	           (not (setq children-skipped
			              org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-pre-cycle-hook 'children))
      (if (org-at-item-p)
	      (org-list-set-item-visibility (point-at-bol) struct 'children)
	    (org-show-entry)
	    (org-with-limited-levels (org-show-children))
	    (org-show-set-visibility 'tree)
	    ;; Fold every list in subtree to top-level items.
	    (when (eq org-cycle-include-plain-lists 'integrate)
	      (save-excursion
	        (org-back-to-heading)
	        (while (org-list-search-forward (org-item-beginning-re) eos t)
	          (beginning-of-line 1)
	          (let* ((struct (org-list-struct))
		             (prevs (org-list-prevs-alist struct))
		             (end (org-list-get-bottom-point struct)))
		        (dolist (e (org-list-get-all-items (point) struct prevs))
		          (org-list-set-item-visibility e struct 'folded))
		        (goto-char (if (< end eos) end eos)))))))
      (org-unlogged-message "CHILDREN")
      (save-excursion
	    (goto-char eos)
	    (outline-next-heading)
	    (when (org-invisible-p) (org-flag-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-cycle-hook 'children)))
     ((eq org-cycle-subtree-status 'subtree)
      (org-show-subtree)
      (org-unlogged-message "ALL")
      (setq org-cycle-subtree-status 'all))
     ((or children-skipped
	      (and (eq last-command this-command)
	           (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-pre-cycle-hook 'subtree))
      (org-flag-region eoh eos nil 'outline)
      (org-unlogged-message
       (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-cycle-hook 'subtree)))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (org-flag-region eoh eos t 'outline)
      (org-unlogged-message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (unless (org-before-first-heading-p)
	    (run-hook-with-args 'org-cycle-hook 'folded))))))

(advice-add #'org-cycle-internal-local
            :override
            #'my/org-cycle-internal-local)

(provide 'org-hide-property-drawers)
