;;; ripgrep.el ---  -*- lexical-binding: t -*-

;; https://www.dyerdwelling.family/emacs/20260109094340-emacs--a-single-function-ripgrep-alternative-to-rgrep/

;;; Commentary:

;;; Code:

(defconst ripgrep-error-regexp
  `((,(concat "^\\(?:"
              ;; Parse using NUL characters when `--null' is used.
              ;; Note that we must still assume no newlines in
              ;; filenames due to "foo: Is a directory." type
              ;; messages.
              "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\):"
              "\\|"
              ;; Fallback if `--null' is not used, use a tight regexp
              ;; to handle weird file names (with colons in them) as
              ;; well as possible.  E.g., use [1-9][0-9]* rather than
              ;; [0-9]+ so as to accept ":034:" in file names.
              "\\(?1:"
              "\\(?:[a-zA-Z]:\\)?"      ; Allow "C:..." for w32.
              "[^\n:]+?[^\n/:]\\):[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:\\(?:[1-9][0-9]*\\)[\t ]*:"
              "\\)")
     1 2
     ;; Calculate column positions (col . end-col) of first grep match on a line
     (,(lambda ()
         (when grep-highlight-matches
           (let* ((beg (match-end 0))
                  (end (save-excursion (goto-char beg) (line-end-position)))
                  (mbeg
                   (text-property-any beg end 'font-lock-face grep-match-face)))
             (when mbeg
               (- mbeg beg)))))
      .
      ,(lambda ()
         (when grep-highlight-matches
           (let* ((beg (match-end 0))
                  (end (save-excursion (goto-char beg) (line-end-position)))
                  (mbeg
                   (text-property-any beg end 'font-lock-face grep-match-face))
                  (mend
                   (and mbeg (next-single-property-change
                              mbeg 'font-lock-face nil end))))
             (when mend
               (- mend beg 1))))))
     nil nil
     (3 '(face nil display ":")))
    ("^Binary file \\(.+\\) matches" 1 nil nil 0 1)))

(defun ripgrep (search-term &optional directory glob)
  "Run ripgrep (rg) with SEARCH-TERM and optionally DIRECTORY and GLOB.
If ripgrep is unavailable, fall back to Emacs's rgrep command. Highlights SEARCH-TERM in results.
By default, only the SEARCH-TERM needs to be provided. If called with a
universal argument, DIRECTORY and GLOB are prompted for as well."
  (interactive
   (let* ((univ-arg current-prefix-arg)
          (default-search-term
           (cond
            ((use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end)))
            ((thing-at-point 'symbol t))
            ((thing-at-point 'word t))
            (t ""))))
     (list
      (read-string (if (string-empty-p default-search-term)
                       "Search for: "
                     (format "Search for (default `%s`): " default-search-term))
                   nil nil default-search-term)
      (when univ-arg (read-directory-name "Directory: "))
      (when univ-arg (read-string "File pattern (glob, default: ): " nil nil "")))))
  (let* ((directory (expand-file-name (or directory default-directory)))
         (glob (or glob ""))
         (buffer-name "*grep*"))
    (if (executable-find "rg")
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (setq default-directory directory)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n" directory))
              (if (not (string= "" glob))
                  (insert (format "[o] Glob: %s\n\n" glob)))
              (insert "Searching...\n\n"))
            (grep-mode)
            (setq-local compilation-error-regexp-alist ripgrep-error-regexp)
            (setq-local my/grep-search-term search-term)
            (setq-local my/grep-directory directory)
            (setq-local my/grep-glob glob))

          (pop-to-buffer buffer)
          (goto-char (point-min))

          (make-process
           :name "ripgrep"
           :buffer buffer
           :command `("rg" "--color=never" "--max-columns=500"
                      "--column" "--line-number" "--no-heading"
                      "--smart-case" "-e" ,search-term
                      "--glob" ,glob ,directory)
           :filter `(lambda (proc string)
                      (when (buffer-live-p (process-buffer proc))
                        (with-current-buffer (process-buffer proc)
                          (let ((inhibit-read-only t)
                                (moving (= (point) (process-mark proc))))
                            (setq string (replace-regexp-in-string "[\r\0\x01-\x08\x0B-\x0C\x0E-\x1F]" "" string))
                            ;; Replace full directory path with ./ in the incoming output
                            (setq string (replace-regexp-in-string
                                          (concat "^" (regexp-quote ,directory))
                                          "./"
                                          string))
                            (save-excursion
                              (goto-char (process-mark proc))
                              (insert string)
                              (set-marker (process-mark proc) (point)))
                            (if moving (goto-char (process-mark proc)))))))
           :sentinel
           `(lambda (proc _event)
              (when (memq (process-status proc) '(exit signal))
                (with-current-buffer (process-buffer proc)
                  (let ((inhibit-read-only t))
                    ;; Remove "Searching..." line
                    (goto-char (point-min))
                    (while (re-search-forward "Searching\\.\\.\\.\n\n" nil t)
                      (replace-match "" nil t))

                    ;; Clean up the output - replace full paths with ./
                    (goto-char (point-min))
                    (forward-line 3)
                    (let ((start-pos (point)))
                      (while (re-search-forward (concat "^" (regexp-quote ,directory)) nil t)
                        (replace-match "./" t t))

                      ;; Check if any results were found
                      (goto-char start-pos)
                      (when (= (point) (point-max))
                        (insert "No results found.\n")))

                    (goto-char (point-max))
                    (insert "\nRipgrep finished\n")

                    ;; Highlight search terms using grep's match face
                    (goto-char (point-min))
                    (forward-line 3)
                    (save-excursion
                      (while (re-search-forward (regexp-quote ,search-term) nil t)
                        (put-text-property (match-beginning 0) (match-end 0)
                                           'face 'match)
                        (put-text-property (match-beginning 0) (match-end 0)
                                           'font-lock-face 'match))))

                  ;; Set up keybindings
                  (local-set-key (kbd "D")
                                 (lambda ()
                                   (interactive)
                                   (ripgrep my/grep-search-term
                                            (read-directory-name "New search directory: ")
                                            my/grep-glob)))
                  (local-set-key (kbd "S")
                                 (lambda ()
                                   (interactive)
                                   (ripgrep (read-string "New search term: "
                                                         nil nil my/grep-search-term)
                                            my/grep-directory
                                            my/grep-glob)))
                  (local-set-key (kbd "o")
                                 (lambda ()
                                   (interactive)
                                   (ripgrep my/grep-search-term
                                            my/grep-directory
                                            (read-string "New glob: "))))
                  (local-set-key (kbd "g")
                                 (lambda ()
                                   (interactive)
                                   (ripgrep my/grep-search-term my/grep-directory my/grep-glob)))

                  (goto-char (point-min))
                  (message "ripgrep finished."))))
           )
          (message "ripgrep started..."))
      ;; Fallback to rgrep
      (progn
        (setq default-directory directory)
        (message (format "%s : %s : %s" search-term glob directory))
        (rgrep search-term (if (string= "" glob) "*" glob) directory)))))

(provide 'ripgrep)
;;; ripgrep.el ends here
