(require 'org-board)

(defun org-board-archive-dry-run ()
  "Archive the URL given by the current entry's `URL' property.

The attachment directory and the unique ID are created if not
already present.  See the docstring of `org-attach-dir'.

Every snapshot is stored in its own timestamped folder, and is
added as a link in the `ARCHIVED_AT' property."

  (interactive)
  (org-board-expand-regexp-alist)
  (let* ((attach-directory (file-name-as-directory (make-temp-name "/tmp/org-board-")))
         (urls (org-entry-get-multivalued-property (point) org-board-property))
         (options
          (org-board-options-handler
           (org-entry-get-multivalued-property (point) "WGET_OPTIONS")))
         (timestamp (org-board-make-timestamp))
         (output-directory (concat (file-name-as-directory attach-directory)
                                   (file-name-as-directory timestamp)))
         (org-id-token (org-id-get))
         (link-to-output (if (not org-board-make-relative)
			                 (concat "[[file:" output-directory "]["
				                     timestamp "]]")
			               (concat "[[file:" (file-relative-name output-directory)"][" timestamp "]]"))))
    (make-directory attach-directory)
    (org-board-wget-call org-board-wget-program
                         output-directory
                         options
                         urls)))

(defvar youtube-dl-binary
  (executable-find "yt-dlp"))

(defun org-board-dispatch-special (orig path directory args site)
  (cond
   ((and
     (= (length site) 1)
     (string-match-p "youtube" (car site)))
    (message "Invoking custom youtube-dl dispatcher!")
    (make-directory directory)
    (let ((default-directory directory))
      (start-process "org-board-youtube-dl" "*org-board-youtube-dl*" youtube-dl-binary
                     "-f 137+140" (car site))))
   ((and
     (= (length site) 1)
     (string-match-p "reddit" (car site)))
    (message "Invoking custom reddit dispatcher!")
    (make-directory directory)
    (let ((default-directory directory))
      (start-process "org-board-reddit-archive" "*org-board-reddit-archive*"
                     (executable-find "python3")
                     "/home/benson/RedditArchiver-standalone/RedditArchiver.py"
                     "-c"
                     "/home/benson/RedditArchiver-standalone/config.yaml"
                     "-i"
                     (car site))))
   (t (apply orig path directory args (list site)))))

(advice-add #'org-board-wget-call
            :around
            #'org-board-dispatch-special)

(provide 'org-board-custom-dispatcher)
