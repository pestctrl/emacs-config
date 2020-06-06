(defvar bisect-top-overlay nil)
(make-variable-buffer-local 'bisect-top-overlay)
(defvar bisect-middle-overlay nil)
(make-variable-buffer-local 'bisect-middle-overlay)
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

(defun move-overlay-to-line (overlay line)
  (save-excursion
    (goto-line line)
    (move-overlay overlay
                  (point-at-bol)
                  (point-at-eol))))

(defun bisect-make-overlay (line bitmap)
  (save-excursion
    (goto-line line)
    (let ((overlay (make-overlay (point-at-bol) (point-at-eol))))
      (overlay-put overlay 'before-string 
                   (propertize "!" 'display
                               (list 'left-fringe
                                     bitmap)))
      (overlay-put overlay 'face font-lock-comment-face)
      overlay)))

(defun bisect-overlay-line (overlay &optional eol)
  (line-number-at-pos
   (funcall (if eol #'overlay-end #'overlay-start)
            overlay)))

(define-minor-mode bisect-mode ""
  nil nil
  bisect-mode-map
  (if (not bisect-mode)
      (mapcar #'delete-overlay (list bisect-middle-overlay
                                     bisect-top-overlay
                                     bisect-bottom-overlay))
    (let ((top    (line-number-at-pos (if mark-active (region-beginning) (point-min))))
          (bottom (line-number-at-pos (if mark-active (region-end)       (point-max)))))
      (when mark-active (call-interactively #'set-mark-command))
      (setq bisect-top-overlay (bisect-make-overlay top 'right-arrow))
      (setq bisect-bottom-overlay (bisect-make-overlay bottom 'right-arrow))
      (setq bisect-middle-overlay (bisect-make-overlay bottom 'right-triangle)))
    (bisect-find-middle)))

(defun bisect-find-middle ()
  (let* ((average (/ (+ (bisect-overlay-line bisect-top-overlay)
                        (bisect-overlay-line bisect-bottom-overlay))
                     2)))
    (goto-line average)
    (bisect-middle-here)))

(defun bisect-middle-here ()
  (interactive)
  (move-overlay-to-line bisect-middle-overlay (line-number-at-pos)))

(defun bisect-up-inclusive ()
  (interactive)
  (move-overlay-to-line bisect-bottom-overlay (bisect-overlay-line bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-down-inclusive ()
  (interactive)
  (move-overlay-to-line bisect-top-overlay (bisect-overlay-line bisect-middle-overlay))
  (bisect-find-middle))

(defun bisect-up-exclusive ()
  (interactive)
  (move-overlay-to-line bisect-bottom-overlay (1- (bisect-overlay-line bisect-middle-overlay)))
  (bisect-find-middle))

(defun bisect-down-exclusive ()
  (interactive)
  (move-overlay-to-line bisect-top-overlay (1+ (bisect-overlay-line bisect-middle-overlay)))
  (bisect-find-middle))

(defun bisect-goto-current-middle ()
  (interactive)
  (goto-char (overlay-start bisect-middle-overlay)))
