;;; my-extra-template-type.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-07-18 13:43]

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

(defun my/org-capture-fill-template (&optional template initial annotation)
  "Fill a TEMPLATE and return the filled template as a string.
The template may still contain \"%?\" for cursor positioning.
INITIAL content and/or ANNOTATION may be specified, but will be overridden
by their respective `org-store-link-plist' properties if present.

Expansion occurs in a temporary Org mode buffer."
  (let* ((template (or template (org-capture-get :template)))
	 (buffer (org-capture-get :buffer))
	 (file (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
	 (time (let* ((c (or (org-capture-get :default-time) (current-time)))
		      (d (decode-time c)))
		 (if (< (nth 2 d) org-extend-today-until)
		     (org-encode-time 0 59 23 (1- (nth 3 d)) (nth 4 d) (nth 5 d))
		   c)))
	 (v-t (format-time-string (org-time-stamp-format nil) time))
	 (v-T (format-time-string (org-time-stamp-format t) time))
	 (v-u (format-time-string (org-time-stamp-format nil t) time))
	 (v-U (format-time-string (org-time-stamp-format t t) time))
	 (v-c (and kill-ring (current-kill 0)))
	 (v-x (or (org-get-x-clipboard 'PRIMARY)
		  (org-get-x-clipboard 'CLIPBOARD)
		  (org-get-x-clipboard 'SECONDARY)
		  ""))			;ensure it is a string
	 ;; `initial' and `annotation' might have been passed.  But if
	 ;; the property list has them, we prefer those values.
	 (v-i (or (plist-get org-store-link-plist :initial)
		  (and (stringp initial) (org-no-properties initial))
		  (org-capture-get :initial)
		  ""))
	 (v-a
	  (let ((a (or (plist-get org-store-link-plist :annotation)
		       annotation
		       (org-capture-get :annotation)
		       "")))
	    ;; Is the link empty?  Then we do not want it...
	    (if (equal a "[[]]") "" a)))
	 (l-re "\\[\\[\\(.*?\\)\\]\\(\\[.*?\\]\\)?\\]")
	 (v-A (if (and v-a (string-match l-re v-a))
		  (replace-match "[[\\1][%^{Link description}]]" nil nil v-a)
		v-a))
	 (v-l (if (and v-a (string-match l-re v-a))
		  (replace-match "[[\\1]]" nil nil v-a)
		v-a))
	 (v-L (if (and v-a (string-match l-re v-a))
		  (replace-match "\\1" nil nil v-a)
		v-a))
	 (v-n user-full-name)
	 (v-k (if (marker-buffer org-clock-marker)
		  (org-no-properties org-clock-heading)
		""))
	 (v-K (if (marker-buffer org-clock-marker)
                  (let ((original-link-plist org-store-link-plist)
                        (clocked-task-link (org-with-point-at org-clock-marker
                                             (org-store-link nil nil))))
                    (setq org-store-link-plist original-link-plist)
                    clocked-task-link)
	        ""))
	 (v-f (or (org-capture-get :original-file-nondirectory) ""))
	 (v-F (or (org-capture-get :original-file) ""))
         (src-block-mode
          (with-current-buffer (org-capture-get :original-buffer)
            (let ((mode-string (symbol-name major-mode)))
              (progn
                (string-match (rx line-start
                                  (group (+ nonl)) "-mode"
                                  line-end)
                              mode-string)
                (match-string 1 mode-string)))))
	 (org-capture--clipboards
	  (delq nil
		(list v-i
		      (org-get-x-clipboard 'PRIMARY)
		      (org-get-x-clipboard 'CLIPBOARD)
		      (org-get-x-clipboard 'SECONDARY)
		      v-c))))
    (setq org-store-link-plist (plist-put org-store-link-plist :annotation v-a))
    (setq org-store-link-plist (plist-put org-store-link-plist :initial v-i))
    (unless template
      (setq template "")
      (message "no template") (ding)
      (sit-for 1))
    (save-window-excursion
      (switch-to-buffer-other-window (get-buffer-create "*Capture*"))
      (erase-buffer)
      (setq buffer-file-name nil)
      (setq mark-active nil)
      (insert template)
      (org-mode)
      (goto-char (point-min))
      ;; %[] insert contents of a file.
      (save-excursion
	(while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	  (let ((filename (expand-file-name (match-string 1)))
		(beg (copy-marker (match-beginning 0)))
		(end (copy-marker (match-end 0))))
	    (unless (org-capture-escaped-%)
	      (delete-region beg end)
	      (set-marker beg nil)
	      (set-marker end nil)
	      (condition-case error
		  (insert-file-contents filename)
		(error
		 (insert (format "%%![could not insert %s: %s]"
				 filename
				 error))))))))
      ;; Mark %() embedded elisp for later evaluation.
      (org-capture-expand-embedded-elisp 'mark)
      ;; Expand non-interactive templates.
      (let ((regexp "%\\(:[-A-Za-z]+\\|<\\([^>\n]+\\)>\\|[aAcfFikKlLntTuUxs]\\)"))
	(save-excursion
	  (while (re-search-forward regexp nil t)
	    ;; `org-capture-escaped-%' may modify buffer and cripple
	    ;; match-data.  Use markers instead.  Ditto for other
	    ;; templates.
	    (let ((pos (copy-marker (match-beginning 0)))
		  (end (copy-marker (match-end 0)))
		  (value (match-string 1))
		  (time-string (match-string 2)))
	      (unless (org-capture-escaped-%)
		(delete-region pos end)
		(set-marker pos nil)
		(set-marker end nil)
		(let* ((inside-sexp? (org-capture-inside-embedded-elisp-p))
		       (replacement
			(pcase (string-to-char value)
			  (?< (format-time-string time-string time))
			  (?:
			   (or (plist-get org-store-link-plist (intern value))
			       ""))
			  (?i
			   (if inside-sexp? v-i
			     ;; Outside embedded Lisp, repeat leading
			     ;; characters before initial place holder
			     ;; every line.
			     (let ((lead (concat "\n"
						 (org-current-line-string t))))
			       (replace-regexp-in-string "\n" lead v-i nil t))))
                          (?s
                           (if (string-empty-p v-i)
                               ""
			     (concat
                              "#+begin_src " src-block-mode "\n"
                              (string-trim v-i "\n") "\n"
                              "#+end_src\n")))
			  (?a v-a)
			  (?A v-A)
			  (?c v-c)
			  (?f v-f)
			  (?F v-F)
			  (?k v-k)
			  (?K v-K)
			  (?l v-l)
			  (?L v-L)
			  (?n v-n)
			  (?t v-t)
			  (?T v-T)
			  (?u v-u)
			  (?U v-U)
			  (?x v-x))))
		  (insert
		   (if inside-sexp?
		       ;; Escape sensitive characters.
		       (replace-regexp-in-string "[\\\"]" "\\\\\\&" replacement)
		     replacement))))))))
      ;; Expand %() embedded Elisp.  Limit to Sexp originally marked.
      (org-capture-expand-embedded-elisp)
      ;; Expand interactive templates.  This is the last step so that
      ;; template is mostly expanded when prompting happens.  Turn on
      ;; Org mode and set local variables.  This is to support
      ;; completion in interactive prompts.
      (let ((org-inhibit-startup t)) (org-mode))
      (org-clone-local-variables buffer "\\`org-")
      (let (strings)			; Stores interactive answers.
	(save-excursion
	  (let ((regexp "%\\^\\(?:{\\([^}]*\\)}\\)?\\([CgGLptTuU]\\)?"))
	    (while (re-search-forward regexp nil t)
	      (let* ((items (and (match-end 1)
				 (save-match-data
				   (split-string (match-string-no-properties 1)
						 "|"))))
		     (key (match-string 2))
		     (beg (copy-marker (match-beginning 0)))
		     (end (copy-marker (match-end 0)))
		     (prompt (nth 0 items))
		     (default (nth 1 items))
		     (completions (nthcdr 2 items)))
		(unless (org-capture-escaped-%)
		  (delete-region beg end)
		  (set-marker beg nil)
		  (set-marker end nil)
		  (pcase key
		    ((or "G" "g")
		     (let* ((org-last-tags-completion-table
			     (org-global-tags-completion-table
			      (cond ((equal key "G") (org-agenda-files))
				    (file (list file))
				    (t nil))))
			    (org-add-colon-after-tag-completion t)
			    (ins (mapconcat
				  #'identity
				  (let ((crm-separator "[ \t]*:[ \t]*"))
                                    (completing-read-multiple
				     (if prompt (concat prompt ": ") "Tags: ")
				     org-last-tags-completion-table nil nil nil
				     'org-tags-history))
				  ":")))
		       (when (org-string-nw-p ins)
			 (unless (eq (char-before) ?:) (insert ":"))
			 (insert ins)
			 (unless (eq (char-after) ?:) (insert ":"))
			 (when (org-at-heading-p) (org-align-tags)))))
		    ((or "C" "L")
		     (let ((insert-fun (if (equal key "C") #'insert
					 (lambda (s) (org-insert-link 0 s)))))
		       (pcase org-capture--clipboards
			 (`nil nil)
			 (`(,value) (funcall insert-fun value))
			 (`(,first-value . ,_)
			  (funcall insert-fun
				   (read-string "Clipboard/kill value: "
						first-value
						'org-capture--clipboards
						first-value)))
			 (_ (error "Invalid `org-capture--clipboards' value: %S"
				   org-capture--clipboards)))))
		    ("p"
		     ;; We remove keyword properties inherited from
		     ;; target buffer so `org-read-property-value' has
		     ;; a chance to find allowed values in sub-trees
		     ;; from the target buffer.
		     (setq-local org-keyword-properties nil)
		     (let* ((origin (set-marker (make-marker)
						(org-capture-get :pos)
						(org-capture-get :buffer)))
			    ;; Find location from where to get allowed
			    ;; values.  If `:target-entry-p' is
			    ;; non-nil, the current headline in the
			    ;; target buffer is going to be a parent
			    ;; headline, so location is fine.
			    ;; Otherwise, find the parent headline in
			    ;; the target buffer.
			    (pom (if (org-capture-get :target-entry-p) origin
				   (let ((level (progn
						  (while (org-up-heading-safe))
						  (org-current-level))))
				     (org-with-point-at origin
				       (let ((l (if (org-at-heading-p)
						    (org-current-level)
						  most-positive-fixnum)))
					 (while (and l (>= l level))
					   (setq l (org-up-heading-safe)))
					 (if l (point-marker)
					   (point-min-marker)))))))
			    (value
			     (org-read-property-value prompt pom default)))
		       (org-set-property prompt value)))
		    ((or "t" "T" "u" "U")
		     ;; These are the date/time related ones.
		     (let* ((upcase? (equal (upcase key) key))
			    (org-end-time-was-given nil)
			    (time (org-read-date upcase? t nil prompt)))
		       (org-insert-timestamp
			time (or org-time-was-given upcase?)
			(member key '("u" "U"))
			nil nil (list org-end-time-was-given))))
		    (`nil
		     ;; Load history list for current prompt.
		     (setq org-capture--prompt-history
			   (gethash prompt org-capture--prompt-history-table))
                     (push (org-completing-read
                            (org-format-prompt (or prompt "Enter string") default)
			    completions
			    nil nil nil 'org-capture--prompt-history default)
			   strings)
		     (insert (car strings))
		     ;; Save updated history list for current prompt.
		     (puthash prompt org-capture--prompt-history
			      org-capture--prompt-history-table))
		    (_
		     (error "Unknown template placeholder: \"%%^%s\""
			    key))))))))
	;; Replace %n escapes with nth %^{...} string.
	(setq strings (nreverse strings))
	(save-excursion
	  (while (re-search-forward "%\\\\\\([1-9][0-9]*\\)" nil t)
	    (unless (org-capture-escaped-%)
	      (replace-match
	       (nth (1- (string-to-number (match-string 1))) strings)
	       nil t)))))
      ;; Make sure there are no empty lines before the text, and that
      ;; it ends with a newline character or it is empty.
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (line-beginning-position))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (if (bobp) (delete-region (point) (line-end-position))
	(end-of-line)
	(delete-region (point) (point-max))
	(insert "\n"))
      ;; Return the expanded template and kill the capture buffer.
      (untabify (point-min) (point-max))
      (set-buffer-modified-p nil)
      (prog1 (buffer-substring-no-properties (point-min) (point-max))
	(kill-buffer (current-buffer))))))

(advice-add #'org-capture-fill-template
            :override
            #'my/org-capture-fill-template)

(provide 'my-extra-template-type)
;;; my-extra-template-type.el ends here
