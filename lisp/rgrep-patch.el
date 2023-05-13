;;; rgrep-patch.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2023-04-30 12:37]

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

(advice-add #'rgrep
            :override
            #'my/rgrep)

(advice-add #'lgrep
            :override
            #'my/lgrep)

;; (rgrep "rgrep" "*.el" "~/emacs-git")

(defun my/lgrep (regexp &optional files dir confirm)
  "Run grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in the \"*grep*\" buffer.  While grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep].

If CONFIRM is non-nil, the user will be given an opportunity to edit the
command before it's run."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
				                   nil nil 'grep-history)))
      ((not grep-template)
       (error "grep.el: No `grep-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		        (files (grep-read-files regexp))
		        (dir (read-directory-name "In directory: "
					                      nil default-directory t))
		        (confirm (equal current-prefix-arg '(4))))
	       (list regexp files dir confirm))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (unless (string-equal (file-remote-p dir) (file-remote-p default-directory))
      (let ((default-directory dir))
        (grep-compute-defaults)))
    (let ((command regexp))
      (if (null files)
	      (if (string= command grep-command)
	          (setq command nil))
	    (setq dir (file-name-as-directory (expand-file-name dir)))
	    (unless (or (not grep-use-directories-skip)
                    (eq grep-use-directories-skip t))
	      (setq grep-use-directories-skip
		        (grep-probe grep-program
			                `(nil nil nil "--directories=skip" "foo"
				                  ,(null-device))
			                nil 1)))
	    (setq command (grep-expand-template
		               grep-template
		               regexp
		               files
		               nil
		               (and grep-find-ignored-files
			                (concat " --exclude="
				                    (mapconcat
                                     (lambda (ignore)
                                       (cond ((stringp ignore)
                                              (shell-quote-argument
                                               ignore grep-quoting-style))
                                             ((consp ignore)
                                              (and (funcall (car ignore) dir)
                                                   (shell-quote-argument
                                                    (cdr ignore)
                                                    grep-quoting-style)))))
				                     grep-find-ignored-files
				                     " --exclude=")))
		               (and (eq grep-use-directories-skip t)
			                '("--directories=skip"))))
	    (when command
	      (if confirm
	          (setq command
		            (read-from-minibuffer "Confirm: "
					                      command nil nil 'grep-history))
	        (add-to-history 'grep-history command))))
      (when command
        (prog1
	        (let ((default-directory dir))
	          ;; Setting process-setup-function makes exit-message-function work
	          ;; even when async processes aren't supported.
              (grep--save-buffers)
	          (compilation-start
               (if (and grep-use-null-device null-device (null-device))
	               (concat command " " (null-device))
	             command)
	           #'grep-mode))
	      ;; Set default-directory if we started lgrep in the *grep* buffer.
	      (if (eq next-error-last-buffer (current-buffer))
	          (setq default-directory dir)))))))

(defun my/rgrep (regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.  As whitespace triggers
completion when entering a pattern, including it requires
quoting, e.g. `\\[quoted-insert]<space>'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in the \"*grep*\" buffer.  While the recursive grep is running,
you can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to visit the lines where matches were found.  To kill the job
before it finishes, type \\[kill-compilation].

This command shares argument histories with \\[lgrep] and \\[grep-find].

When called programmatically and FILES is nil, REGEXP is expected
to specify a command to run.

If CONFIRM is non-nil, the user will be given an opportunity to edit the
command before it's run.

Interactively, the user can use the \\`M-c' command while entering
the regexp to indicate whether the grep should be case sensitive
or not."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
				                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
		        (files (grep-read-files regexp))
		        (dir (read-directory-name "Base directory: "
					                      nil default-directory t))
		        (confirm (equal current-prefix-arg '(4))))
	       (list regexp files dir confirm))))))
  ;; If called non-interactively, also compute the defaults if we
  ;; haven't already.
  (unless grep-find-template
    (grep-compute-defaults))
  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-accessible-directory-p dir))
      (setq dir default-directory))
    (unless (string-equal (file-remote-p dir) (file-remote-p default-directory))
      (let ((default-directory dir))
        (grep-compute-defaults)))
    (if (null files)
	    (if (not (string= regexp (if (consp grep-find-command)
				                     (car grep-find-command)
				                   grep-find-command)))
	        (compilation-start regexp #'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (let* ((case-fold-search (read-regexp-case-fold-search regexp))
             (command (rgrep-default-command regexp files nil)))
	    (when command
	      (if confirm
	          (setq command
		            (read-from-minibuffer "Confirm: "
					                      command nil nil 'grep-find-history))
	        (add-to-history 'grep-find-history command))
          (grep--save-buffers)
	      (prog1
              (let ((default-directory dir))
	            (compilation-start command #'grep-mode))
	        ;; Set default-directory if we started rgrep in the *grep* buffer.
	        (if (eq next-error-last-buffer (current-buffer))
	            (setq default-directory dir))))))))

(provide 'rgrep-patch)
;;; rgrep-patch.el ends here
