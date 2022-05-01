;;; org-overrides.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-04-17 10:50]

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

(defun my/org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	   (region-p (org-region-active-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc
	   line lines left right n n1)
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
			 (copy-sequence
			  "Press key for an agenda command:
--------------------------------        <   Buffer, subtree/region restriction
a   Agenda for current week or day      >   Remove restriction
t   List of all TODO entries            e   Export agenda views
m   Match a TAGS/PROP/TODO query        T   Entries with special TODO kwd
s   Search for keywords                 M   Like m, but only TODO entries
/   Multi-occur                         S   Like s, but only TODO entries
?   Find :FLAGGED: entries              C   Configure custom agenda commands
*   Toggle sticky agenda views          #   List stuck projects (!=configure)
"))
			(start 0))
		    (while (string-match
			    "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			    header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(setq header-end (point-marker))
	(while t
	  (setq custom1 custom)
	  (when (eq rmheader t)
	    (org-goto-line 1)
	    (re-search-forward ":" nil t)
	    (delete-region (match-end 0) (point-at-eol))
	    (forward-char 1)
	    (looking-at "-+")
	    (delete-region (match-end 0) (point-at-eol))
	    (move-marker header-end (match-end 0)))
	  (goto-char header-end)
	  (delete-region (point) (point-max))

	  ;; Produce all the lines that describe custom commands and prefixes
	  (setq lines nil)
	  (while (setq entry (pop custom1))
	    (setq key (car entry) desc (nth 1 entry)
		  type (nth 2 entry)
		  match (nth 3 entry))
	    (if (> (length key) 1)
                (when-let* ((first-char (string-to-char key))
                            ((not (member first-char prefixes))))
		  (cl-pushnew (string-to-char key) prefixes :test #'equal)
                  (push (format
                         "%-4s%-14s"
                         (org-add-props (copy-sequence (substring key 0 1))
			     '(face bold))
                         (or (cdr (assoc (concat selstring
						 (char-to-string first-char))
					 prefix-descriptions))
			     "Prefix key"))
                        lines))
	      (setq line
		    (format
		     "%-4s%-14s"
		     (org-add-props (copy-sequence key)
			 '(face bold))
		     (cond
		      ((string-match "\\S-" desc) desc)
		      ((eq type 'agenda) "Agenda for current week or day")
		      ((eq type 'agenda*) "Appointments for current week or day")
		      ((eq type 'alltodo) "List of all TODO entries")
		      ((eq type 'search) "Word search")
		      ((eq type 'stuck) "List of stuck projects")
		      ((eq type 'todo) "TODO keyword")
		      ((eq type 'tags) "Tags query")
		      ((eq type 'tags-todo) "Tags (TODO)")
		      ((eq type 'tags-tree) "Tags tree")
		      ((eq type 'todo-tree) "TODO kwd tree")
		      ((eq type 'occur-tree) "Occur tree")
		      ((functionp type) (if (symbolp type)
					    (symbol-name type)
					  "Lambda expression"))
		      (t "???"))))
	      (cond
	       ((not (org-string-nw-p match)) nil)
	       (org-agenda-menu-show-matcher
		(setq line
		      (concat line ": "
			      (cond
			       ((stringp match)
				(propertize match 'face 'org-warning))
			       ((listp type)
				(format "set of %d commands" (length type)))))))
	       (t
		(org-add-props line nil 'help-echo (concat "Matcher: " match))))
	      (push line lines)))
	  (setq lines (nreverse lines))
	  ;; (when prefixes
	  ;;   (mapc (lambda (x)
	  ;;           (push
	  ;;            (format "%s   %s"
	  ;;       	     (org-add-props (char-to-string x)
	  ;;       		 nil 'face 'bold)
	  ;;       	     (or (cdr (assoc (concat selstring
	  ;;       				     (char-to-string x))
	  ;;       			     prefix-descriptions))
	  ;;       		 "Prefix key"))
	  ;;            lines))
	  ;;         prefixes))

	  ;; Check if we should display in two columns
	  (if org-agenda-menu-two-columns
	      (progn
		(setq n (length lines)
		      n1 (+ (/ n 2) (mod n 2))
		      right (nthcdr n1 lines)
		      left (copy-sequence lines))
		(setcdr (nthcdr (1- n1) left) nil))
	    (setq left lines right nil))
	  (while left
	    (insert "\n" (pop left))
	    (when right
	      (if (< (current-column) 40)
		  (move-to-column 40 t)
		(insert "   "))
	      (insert (pop right))))

	  ;; Make the window the right size
	  (goto-char (point-min))
	  (if second-time
	      (when (not (pos-visible-in-window-p (point-max)))
		(org-fit-window-to-buffer))
	    (setq second-time t)
	    (org-fit-window-to-buffer))

	  ;; Hint to navigation if window too small for all information
	  (setq header-line-format
		(when (not (pos-visible-in-window-p (point-max)))
		  "Use C-v, M-v, C-n or C-p to navigate."))

	  ;; Ask for selection
	  (cl-loop
	   do (progn
		(message "Press key for agenda command%s:"
			 (if (or restrict-ok org-agenda-overriding-restriction)
			     (if org-agenda-overriding-restriction
				 " (restriction lock active)"
			       (if restriction
				   (format " (restricted to %s)" restriction)
				 " (unrestricted)"))
			   ""))
		(setq c (read-char-exclusive)))
	   until (not (memq c '(14 16 22 134217846)))
	   do (org-scroll c))

	  (message "")
	  (cond
	   ((assoc (char-to-string c) custom)
	    (setq selstring (concat selstring (char-to-string c)))
	    (throw 'exit (cons selstring restriction)))
	   ((memq c prefixes)
	    (setq selstring (concat selstring (char-to-string c))
		  prefixes nil
		  rmheader (or rmheader t)
		  custom (delq nil (mapcar
				    (lambda (x)
				      (if (or (= (length (car x)) 1)
					      (/= (string-to-char (car x)) c))
					  nil
					(cons (substring (car x) 1) (cdr x))))
				    custom))))
	   ((eq c ?*)
	    (call-interactively 'org-toggle-sticky-agenda)
	    (sit-for 2))
	   ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	    (message "Restriction is only possible in Org buffers")
	    (ding) (sit-for 1))
	   ((eq c ?1)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction 'buffer))
	   ((eq c ?0)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction (if region-p 'region 'subtree)))
	   ((eq c ?<)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction
		  (cond
		   ((eq restriction 'buffer)
		    (if region-p 'region 'subtree))
		   ((memq restriction '(subtree region))
		    nil)
		   (t 'buffer))))
	   ((eq c ?>)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction nil))
	   ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	    (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
           ((and (> (length selstring) 0) (eq c ?\d))
            (delete-window)
            (org-agenda-get-restriction-and-command prefix-descriptions))

	   ((equal c ?q) (user-error "Abort"))
	   (t (user-error "Invalid key %c" c))))))))

(advice-add #'org-agenda-get-restriction-and-command
            :override
            #'my/org-agenda-get-restriction-and-command)

(provide 'org-overrides)
;;; org-overrides.el ends here
