;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun ll/verify-itineraries ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((m (gethash "Argo" ll/def-class-hash)) a)
      (while (re-search-forward
              (rx "ItinClass<" (group (+ (or alphanumeric "_"))) ">")
              nil t)
        (let ((sym (intern (match-string 1))))
          (unless (gethash sym m)
            (push sym a))))
      (--> a
           (seq-uniq it)
           (mapcar #'symbol-name it)
           (mapcar #'(lambda (x) (concat x "\n")) it)
           (apply #'concat it)
           (message it)))))
