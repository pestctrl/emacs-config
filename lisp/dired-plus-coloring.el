;;; dired-plus-coloring.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-10-14 09:47]

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
(defvar dired-font-lock-keywords
  (list
   ;;
   ;; Dired marks.
   (list dired-re-mark '(0 dired-mark-face))
   ;;
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; file name itself.  We search for Dired defined regexps, and then use the
   ;; Dired defined function `dired-move-to-filename' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the file name.
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string dired-marker-char) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
   ;;
   ;; Flagged files.
   (list (concat "^[" (char-to-string dired-del-marker) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))
   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
;;;   ;;
;;;   ;; Files that are group or world writable.
;;;   (list (concat dired-re-maybe-mark dired-re-inode-size
;;;		 "\\([-d]\\(....w....\\|.......w.\\)\\)")
;;;	 '(1 dired-warning-face)
;;;	 '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
   ;; However, we don't need to highlight the file name, only the
   ;; permissions, to win generally.  -- fx.
   ;; Fixme: we could also put text properties on the permission
   ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d]....\\(w\\)....")	; group writable
	 '(1 dired-perm-write-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d].......\\(w\\).")	; world writable
	 '(1 dired-perm-write-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d]..\\(s\\)......")	; suid
	 '(1 'dired-set-id))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d].....\\([sS]\\)...")	; guid
	 '(1 'dired-set-id))
   ;;
   ;; Subdirectories.
   (list dired-re-dir
	 '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
          (list (concat
                 "\\(" (regexp-opt completion-ignored-extensions)
                 "\\|#\\|\\.#.+\\)$")
	   '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\|\\.#.+\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the filename,
		    ;; move back to the start of the filename
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (dired-move-to-filename)))
	     nil (0 dired-ignored-face))))
   ;;
   ;; Broken Symbolic link.
   (list dired-re-sym
         (list (lambda (end)
                 (let* ((file (dired-file-name-at-point))
                        (truename (ignore-errors (file-truename file))))
                   ;; either not existent target or circular link
                   (and (not (and truename (file-exists-p truename)))
                        (search-forward-regexp "\\(.+\\) \\(->\\) ?\\(.+\\)" end t))))
               '(dired-move-to-filename)
               nil
               '(1 'dired-broken-symlink)
               '(2 dired-symlink-face)
               '(3 '(face dired-broken-symlink dired-symlink-filename t))))
   ;;
   ;; Symbolic link to a directory.
   (list dired-re-sym
         (list (lambda (end)
                 (when-let* ((file (dired-file-name-at-point))
                             (truename (ignore-errors (file-truename file))))
                   (and (file-directory-p truename)
		        (search-forward-regexp "\\(.+-> ?\\)\\(.+\\)" end t))))
               '(dired-move-to-filename)
               nil
               '(1 dired-symlink-face)
               '(2 '(face dired-directory-face dired-symlink-filename t))))
   ;;
   ;; Symbolic link to a non-directory.
   (list dired-re-sym
         (list (lambda (end)
                 (when-let ((file (dired-file-name-at-point)))
                   (let ((truename (ignore-errors (file-truename file))))
                     (and (or (not truename)
		              (not (file-directory-p truename)))
		          (search-forward-regexp "\\(.+-> ?\\)\\(.+\\)"
                                                 end t)))))
               '(dired-move-to-filename)
               nil
               '(1 dired-symlink-face)
               '(2 '(face default dired-symlink-filename t))))
   ;;
   ;; Sockets, pipes, block devices, char devices.
   (list dired-re-special
	 '(".+" (dired-move-to-filename) nil (0 'dired-special)))
   ;;
   ;; Explicitly put the default face on file names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat dired-re-maybe-mark dired-re-inode-size dired-re-perms ".*:$")
	 '(".+" (dired-move-to-filename) nil (0 'default)))
   ;;
   ;; Directory headers.
   (list dired-subdir-regexp '(1 dired-header-face))
   )
  "Additional expressions to highlight in Dired mode.")

(defvar diredp-font-lock-keywords-1
  (list
   '("^  \\(.+:\\)$" 1 diredp-dir-heading) ; Directory headers
   '("^  wildcard.*$" 0 'default)   ; Override others, e.g. `l' for `diredp-other-priv'.
   '("^  (No match).*$" 0 'default) ; Override others, e.g. `t' for `diredp-other-priv'.
   '("[^ .]\\(\\.[^. /]+\\)$" 1 diredp-file-suffix) ; Suffix, including `.'.
   '("\\([^ ]+\\) -> .+$" 1 diredp-symlink)         ; Symbolic links

   ;; 1) Date/time and 2) filename w/o suffix.
   ;;    This is a bear, and it is fragile - Emacs can change `dired-move-to-filename-regexp'.
   (if (or (not (fboundp 'version<))  (version< emacs-version "23.2"))
       (list dired-move-to-filename-regexp
             (list 1 'diredp-date-time t t) ; Date/time
             (list (concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredp-compressed-extensions)
                                                 "\\)[*]?$")) ; Compressed-file name
                   nil nil (list 0 diredp-compressed-file-name 'keep t)))
     `(,dired-move-to-filename-regexp
       (7 diredp-date-time t t) ; Date/time, locale (western or eastern)
       (2 diredp-date-time t t) ; Date/time, ISO
       (,(concat "\\(.+\\)\\(" (concat (funcall #'regexp-opt diredp-compressed-extensions)
                                       "\\)[*]?$"))
        nil nil (0 diredp-compressed-file-name keep t)))) ; Compressed-file suffix
   (if (or (not (fboundp 'version<))  (version< emacs-version "23.2"))
       (list dired-move-to-filename-regexp
             (list 1 'diredp-date-time t t) ; Date/time
             (list "\\(.+\\)$" nil nil (list 0 diredp-file-name 'keep t))) ; Filename
     `(,dired-move-to-filename-regexp
       (7 diredp-date-time t t) ; Date/time, locale (western or eastern)
       (2 diredp-date-time t t) ; Date/time, ISO
       ("\\(.+\\)$" nil nil (0 diredp-file-name keep t)))) ; Filename (not a compressed file)

   ;; Files to ignore.
   ;;   Use face `diredp-ignored-file-name' for omission by file-name extension.
   ;;   Use face `diredp-omit-file-name' for omission by entire file name.
   (let* ((omit-exts   (or (and (boundp 'dired-omit-extensions)  dired-omit-extensions)
                           completion-ignored-extensions))
          (omit-exts   (and omit-exts  (mapconcat #'regexp-quote omit-exts "\\|")))
          (compr-exts  (and diredp-ignore-compressed-flag
                            (concat "\\|" (mapconcat #'regexp-quote diredp-compressed-extensions "\\|")))))
     (list (concat "^  \\(.*\\(" omit-exts compr-exts "\\)[*]?\\)$") ; [*]? allows for executable flag (*).
           1 diredp-ignored-file-name t))
   `(,(concat "^.*" dired-move-to-filename-regexp
              "\\(" diredp-omit-files-font-lock-regexp "\\)[*]?$") ; [*]? allows for executable flag (*).
     (0 diredp-omit-file-name t))

   ;; Compressed-file (suffix)
   (list (concat "\\(" (funcall #'regexp-opt diredp-compressed-extensions) "\\)[*]?$")
         1 diredp-compressed-file-suffix t)
   '("\\([*]\\)$" 1 diredp-executable-tag t) ; Executable (*)

   ;; Inode, hard-links, & file size (. and , are for the decimal point, depending on locale)
   ;; See comment for `directory-listing-before-filename-regexp' in `files.el' or `files+.el'.
   '("\\(\\([0-9]+\\([.,][0-9]+\\)?\\)[BkKMGTPEZY]?[ /]?\\)" 1 diredp-number)

   ;; Directory names - exclude d:/..., Windows drive letter in a dir heading.
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(d\\)[^:]")
         '(1 diredp-dir-priv t) '(".+" (dired-move-to-filename) nil (0 diredp-dir-name t)))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\(x\\)") ; o x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([lsStT]\\)") ; o misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\(w\\).") ; o w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\(r\\)..") ; o r
         '(1 diredp-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\(x\\)...") ; g x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([lsStT]\\)...") ; g misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\(w\\)....") ; g w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\(r\\).....") ; g r
         '(1 diredp-read-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\(x\\)...") ; u x
         '(1 diredp-exec-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([lsStT]\\)...") ; u misc
         '(1 diredp-other-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\(w\\)....") ; u w
         '(1 diredp-write-priv))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\(r\\).....") ; u r
         '(1 diredp-read-priv))

   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]........\\([-rwxlsStT]\\)") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].......\\([-rwxlsStT]\\).") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]......\\([-rwxlsStT]\\)..") ; u -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].....\\([-rwxlsStT]\\)...") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]....\\([-rwxlsStT]\\)....") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]...\\([-rwxlsStT]\\).....") ; u -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]..\\([-rwxlsStT]\\)......") ; o -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl].\\([-rwxlsStT]\\).......") ; g -
         '(1 diredp-no-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "[-dl]\\([-rwxlsStT]\\)........") ; u -
         '(1 diredp-no-priv keep))

   (list (concat dired-re-maybe-mark dired-re-inode-size "\\([bcsmpS]\\)") ; (rare)
         '(1 diredp-rare-priv keep))
   (list (concat dired-re-maybe-mark dired-re-inode-size "\\(l\\)[-rwxlsStT]") ; l
         '(1 diredp-link-priv keep))

   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
         1 diredp-flag-mark-line t)     ; Flag/mark lines
   (list (concat "^\\([^\n " (char-to-string dired-del-marker) "]\\)") ; Flags, marks (except D)
         1 diredp-flag-mark t)

   (list (concat "^\\([" (char-to-string dired-del-marker) "].*$\\)") ; Deletion-flagged lines
         1 diredp-deletion-file-name t)
   (list (concat "^\\([" (char-to-string dired-del-marker) "]\\)") ; Deletion flags (D)
         1 diredp-deletion t)

   ) "2nd level of Dired highlighting.  See `font-lock-maximum-decoration'.")

(defun font-lock-refresh-defaults ()
  "Restart fontification in current buffer after recomputing from defaults.
Recompute fontification variables using `font-lock-defaults' and
`font-lock-maximum-decoration'.  Then restart fontification.

Use this function when you have changed any of the above
variables directly.

Note: This function will erase modifications done by
`font-lock-add-keywords' or `font-lock-remove-keywords', but will
preserve `hi-lock-mode' highlighting patterns."
  (font-lock-mode -1)
  (kill-local-variable 'font-lock-set-defaults)
  (font-lock-mode 1))

(defun diredp--set-up-font-locking ()
  "Add this to `dired-mode-hook' to provide for second-level fontifying."
  (set (make-local-variable 'font-lock-defaults)
       ;; Two levels.  Use 3-element list, since it is standard to have one more
       ;; than the number of levels.  This is necessary for it to work with
       ;; `font(-lock)-menus.el'.
       '((dired-font-lock-keywords
          dired-font-lock-keywords
          diredp-font-lock-keywords-1)
         t nil nil beginning-of-line))
  ;; Refresh `font-lock-keywords' from `font-lock-defaults'
  (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults)))

(add-hook 'dired-mode-hook 'diredp--set-up-font-locking)

(provide 'dired-plus-coloring)
;;; dired-plus-coloring.el ends here
