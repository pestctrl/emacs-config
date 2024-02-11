(defun my/convert-to-format ()
  (interactive)
  (cl-flet ((add-drawer (regex label)
              (save-excursion
                (when  (re-search-forward regex nil t)
                  (goto-char (line-beginning-position))
                  (insert ":" label ": ")))))
    (re-search-forward "^\\*\\*\\*\\* ")
    (goto-char (line-beginning-position))
    (org-narrow-to-subtree)
    (add-drawer "^https://goo.gl" "GOOGLE_MAPS_URL")
    (add-drawer (rx line-start (+ digit) "." (+ digit) ", ")
                "COORDINATES")
    (add-drawer "Tainan City"
                "ADDRESS")
    (add-drawer "^http"
                "WEBSITE")
    (add-drawer "^English:"
                "ENGLISH")
    (goto-char (line-end-position))
    (insert "\n:PROPERTIES:")
    (re-search-forward "goo.gl" nil t)
    (goto-char (line-end-position))
    (insert "\n:END:")
    (widen)))

(defun my/populate-taiwan-fields (address website maps-url coordinates)
  (interactive
   (list
    (read-string "Address? ")
    (read-string "Website? ")
    (read-string "Google Maps URL? ")
    (read-string "Coordinates? ")))
  (cl-flet ((add-valid (key value)
              (when (not (string-empty-p value))
                (org-entry-put (point) key value))))
    (add-valid "ADDRESS" address)
    (add-valid "COORDINATES" coordinates)
    (add-valid "WEBSITE" website)
    (add-valid "GOOGLE_MAPS_URL" maps-url)))

(provide 'taiwan)
