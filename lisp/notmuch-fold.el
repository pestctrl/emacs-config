;; Code Snippet from Julien Masson for tree-folding in notmuch-tree 

(defcustom notmuch-tree-overlay-string " [...]"
  "String displayed at the beginning of the overlay"
  :type 'string
  :group 'notmuch-tree)

;; Faces for overlays
(defface notmuch-tree-overlay-fold-face
  '((t :inherit font-lock-keyword-face))
  "Default face used to display `notmuch-tree-overlay-string'"
  :group 'notmuch-tree
  :group 'notmuch-faces)

(defvar notmuch-tree-overlays nil
  "List of overlays used to fold/unfold thread")

(defun notmuch-tree-find-overlay (buffer start end)
  "Return the first overlay found in `notmuch-tree-overlays'.

  The overlay found is located between START and END position in BUFFER."
  (cl-find-if (lambda (ov)
                (and (eq (overlay-buffer ov) buffer)
                     (<= (overlay-start ov) start)
                     (>= (overlay-end ov) end)))
              notmuch-tree-overlays))

(defun notmuch-tree-clean-up-overlays (&rest args)
  "Remove overlays not referenced to any buffer"
  (setq notmuch-tree-overlays (cl-remove-if #'overlay-buffer notmuch-tree-overlays)))

(defun notmuch-tree-remove-overlay (overlay)
  "Delete OVERLAY and remove it from `notmuch-tree-overlays' list"
  (setq notmuch-tree-overlays (remove overlay notmuch-tree-overlays))
  (delete-overlay overlay))

(defun notmuch-tree-add-overlay (start end)
  "Add an overlay from START to END in the current buffer.

  If non nil, `notmuch-tree-overlay-string' is added at the end of the line.
  The overlay created is added to `notmuch-tree-overlays' list"
  (let ((overlay (make-overlay start end)))
    (add-to-list 'notmuch-tree-overlays overlay)
    (overlay-put overlay 'invisible t)
    (when notmuch-tree-overlay-string
      (overlay-put overlay 'before-string
                   (propertize notmuch-tree-overlay-string
                               'face 'notmuch-tree-overlay-fold-face)))))

(defun notmuch-tree-thread-range ()
  "Return list of Start and End position of the current thread"
  (let (start end)
    (save-excursion
      (while (not (or (notmuch-tree-get-prop :first) (eobp)))
        (forward-line -1))
      (setq start (line-end-position))
      (notmuch-tree-next-thread)
      (setq end (- (point) 1))
      (list start end))))

(defun notmuch-tree-sub-thread-range ()
  "Return list of Start and End position of the current sub-thread"
  (if (notmuch-tree-get-prop :first)
      (notmuch-tree-thread-range)
    (let ((level (length (notmuch-tree-get-prop :tree-status)))
          (start (line-end-position))
          end)
      ;; find end position
      (save-excursion
        (forward-line)
        (while (and (< level (length (notmuch-tree-get-prop :tree-status)))
                    (not (eobp)))
          (forward-line))
        (setq end (- (point) 1)))
      (list start end))))

(defun notmuch-tree-toggle-folding-thread (&optional arg)
  "Fold / Unfold the current thread or sub-thread.

  With prefix arg (C-u) the whole thread is folded"
  (interactive "p")
  (cl-multiple-value-bind (start end)
      (if (and arg (= arg 1))
          (notmuch-tree-sub-thread-range)
        (notmuch-tree-thread-range))
    (unless (= start end)
      (let ((overlay (notmuch-tree-find-overlay (current-buffer) start end)))
        (if overlay
            (notmuch-tree-remove-overlay overlay)
          (notmuch-tree-add-overlay start end))))))

(advice-add #'notmuch-tree-worker
            :before
            #'notmuch-tree-clean-up-overlays)

(defun advice-replace-forward-line-with-next-line (orig &rest args)
  (letf (((symbol-function 'forward-line) (symbol-function 'next-line)))
    (apply orig args)))

;; Needed for n and p to work
(advice-add #'notmuch-tree-prev-matching-message
            :around
            #'advice-replace-forward-line-with-next-line)

(advice-add #'notmuch-tree-next-matching-message
            :around
            #'advice-replace-forward-line-with-next-line)


(defun notmuch-tree-next-sibling ()
  (interactive)
  (let ((l (length (notmuch-tree-get-prop :tree-status))))
    (while (progn (next-line)
                  (and (not (bobp))
                       (< l (length (notmuch-tree-get-prop :tree-status)))))))
  (when (window-live-p notmuch-tree-message-window)
    (notmuch-tree-show-message-in)))

(defun notmuch-tree-prev-sibling ()
  (interactive)
  (let ((l (length (notmuch-tree-get-prop :tree-status))))
    (while (progn (previous-line)
                  (and (not (bobp))
                       (< l (length (notmuch-tree-get-prop :tree-status)))))))
  (when (window-live-p notmuch-tree-message-window)
    (notmuch-tree-show-message-in)))

(provide 'notmuch-fold)

