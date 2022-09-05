;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun oajm-check-for-allowed-buffer ()
  (unless (member major-mode '(org-mode org-agenda-mode fundamental-mode))
    (previous-buffer)
    (message (nth (random 2)
                  '("Uh oh, stinky!"
                    "Not allowed to be in that buffer!")))))

(define-minor-mode org-agenda-jail-mode ()
  :global t
  (if org-agenda-jail-mode
      (add-hook 'window-configuration-change-hook
                #'oajm-check-for-allowed-buffer)
    (remove-hook 'window-configuration-change-hook
                 #'oajm-check-for-allowed-buffer)))
