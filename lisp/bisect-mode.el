(defvar bisect-top nil)
(make-variable-buffer-local 'bisect-top)
(defvar bisect-top-overlay nil)
(make-variable-buffer-local 'bisect-top-overlay)
(defvar bisect-middle-overlay nil)
(make-variable-buffer-local 'bisect-middle-overlay)
(defvar bisect-bottom nil)
(make-variable-buffer-local 'bisect-bottom)
(defvar bisect-bottom-overlay nil)
(make-variable-buffer-local 'bisect-bottom-overlay)
(defvar bisect-linewise t)

(defvar bisect-mode-map nil)

(unless bisect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'bisect-up-inclusive)
    (define-key map (kbd "U") #'bisect-up-exclusive)
    (define-key map (kbd "d") #'bisect-down-inclusive)
    (define-key map (kbd "D") #'bisect-down-exclusive)
    (define-key map (kbd "g") #'bisect-goto-current-middle)
    (define-key map (kbd "C-SPC") #'bisect-middle-here)
    (define-key map (kbd "q") #'bisect-mode)
    (setq bisect-mode-map map)))

(defmacro put-arrow-setq (sym point bitmap)
  `(overlay-put (setq ,sym (make-overlay ,point ,point))
                'before-string
                (propertize "!" 'display
                            (list 'left-fringe
                                  ',bitmap)
                            ;; 'face 'font-lock-comment-face
                            )))

(define-minor-mode bisect-mode ""
  nil nil
  bisect-mode-map
  (if (not bisect-mode)
      (mapcar #'delete-overlay (list bisect-middle-overlay
                                   bisect-top-overlay
                                   bisect-bottom-overlay))
    (let ((top    (if mark-active (region-beginning) (point-min)))
          (bottom (if mark-active (region-end)       (point-max))))
      (when mark-active (call-interactively #'set-mark-command))
      (setq bisect-top top)
      (setq bisect-bottom bottom)
      (put-arrow-setq bisect-top-overlay top right-arrow)
      (put-arrow-setq bisect-bottom-overlay bottom right-arrow)
      (put-arrow-setq bisect-middle-overlay bottom right-triangle))
    (bisect-find-middle)))

(defun bisect-find-middle ()
  (move-overlay bisect-bottom-overlay bisect-bottom bisect-bottom)
  (move-overlay bisect-top-overlay bisect-top bisect-top)
  (let* ((sum (if bisect-linewise
                  (+ (line-number-at-pos bisect-top)
                     (line-number-at-pos bisect-bottom))
                (+ bisect-top bisect-bottom)))
         (average (/ sum 2)))
    (if bisect-linewise
        (goto-line average)
      (goto-char average))
    (bisect-middle-here)))

(defun bisect-up-inclusive ()
  (interactive)
  (setq bisect-bottom (overlay-end bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-down-inclusive ()
  (interactive)
  (setq bisect-top (overlay-start bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-up-exclusive ()
  (interactive)
  (setq bisect-bottom (overlay-start bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-down-exclusive ()
  (interactive)
  (setq bisect-top (overlay-end bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-middle-here ()
  (interactive)
  (move-overlay bisect-middle-overlay (point-at-bol) (point-at-eol)))

(defun bisect-goto-current-middle ()
  (interactive)
  (goto-char (overlay-start bisect-middle-overlay)))
