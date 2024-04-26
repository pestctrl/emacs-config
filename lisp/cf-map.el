;;; cfmap.el --- Sidebar showing a "mini-map" of a buffer

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: David Engster <deng@randomsample.de>
;; Keywords:
;; Version: 1.4

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is an implementation of a cfmap sidebar, i.e., a
;; smaller display of the current buffer on the left side.  It
;; highlights the currently shown region and updates its position
;; automatically.  You can navigate in the minibar by dragging the
;; active region with the mouse, which will scroll the corresponding
;; edit buffer.  Additionally, you can overlay information from the
;; tags gathered by CEDET's semantic analyzer.

;; Simply use M-x cfmap-mode to toggle activation of the cfmap.
;; Use 'M-x customize-group RET cfmap RET' to adapt cfmap to your
;; needs.

;;; KNOWN BUGS:

;; * Currently cannot deal with images.
;; * Display/movement can be a bit erratic at times.

;;; TODO:

;; * Fix known bugs.
;; * Make sidebar permanently visible. This requires something like a
;;   'window group' feature in Emacs, which is currently being worked on.
;; * Moving the active region with the keyboard / mouse-wheel ?


;;; News:
;;
;;;; Changes since v1.2:
;;
;; - New option: cfmap-hide-cursor (active by default)
;; - New option: cfmap-disable-mode-line (active by default)
;; - Make current line highlighting face configurable, change to dark gray.
;; - New default behavior for cfmap-automatically-delete-window:
;;   keep cfmap window as long as buffer is visible. Change variable
;;   to 't' to get old behavior.
;; - Bug fixes
;;
;;;; Changes since v1.1:
;;
;; - Change some defaults: better colors, reduced update delay.
;; - `cfmap-tag-only': New experimental feature to only display an
;;   'abstract view' of the buffer with overlays generated from
;;   Semantic information.  Works only for buffers parsed by Semantic.
;; - `cfmap-highlight-line': Highlight current line in Cfmap.
;; - Fix autoloads.
;; - Display lines denoting beginning/end of functions in Semantic
;;   overlays.
;;
;;;; Changes since v1.0:
;;
;; - Largely rewritten as a minor mode; use M-x cfmap-mode to
;;   enable/disable.
;; - Cfmap will now remain active for all buffers which derive from
;;   `prog-mode' (can be changed through `cfmap-major-modes').  The
;;   cfmap window will be automatically created or deleted (see new
;;   variables `cfmap-recreate-window' and
;;   `cfmap-automatically-delete-window').
;; - Possibility to set a minimum width of the cfmap window
;;   (`cfmap-minimum-width').
;; - Cfmap window will be marked so that you should not be able to
;;   enter it.
;; - Semantic overlays will be automatically updated during editing.
;; - Lots of bug fixes.

;; Silence byte compiler
(declare-function semantic-active-p "semantic/fw")
(declare-function semantic-fetch-tags "semantic")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-overlay "semantic/tag")
(declare-function semantic-tag-name "semantic/tag")

(defgroup cfmap nil
  "A cfmap sidebar for Emacs."
  :group 'convenience)

(defface cfmap-font-face
  '((default :family "DejaVu Sans Mono" :height 30))
  "Face used for text in cfmap buffer, notably the font family and height.
This height should be really small.  You probably want to use a
TrueType font for this.  After changing this, you should
recreate the cfmap to avoid problems with recentering."
  :group 'cfmap)

(defface cfmap-current-line-face
  '((((background dark)) (:background "dark gray"))
    (t (:background "dark gray")))
  "Face for the current line in the cfmap.
By default, both foreground and background are yellow."
  :group 'cfmap)

(defface cfmap-active-region-background
  '((((background dark)) (:background "#700000" :extend t))
    (t (:background "#C847D8FEFFFF" :extend t)))
  "Face for the active region in the cfmap.
By default, this is only a different background color."
  :group 'cfmap)

(defface cfmap-semantic-function-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
	   :inherit (font-lock-function-name-face cfmap-font-face)
	   :height 2.75 :background "#202414"))
    (t (:box (:line-width 1 :color "black")
	     :inherit (font-lock-function-name-face cfmap-font-face)
	     :height 2.75 :background "gray90")))
  "Face used for functions in the semantic overlay.")

(defface cfmap-semantic-variable-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
	   :inherit (font-lock-variable-name-face cfmap-font-face)
	    :height 2.75 :background "gray10"))
    (t (:box (:line-width 1 :color "black")
	     :inherit (font-lock-function-name-face cfmap-font-face)
	     :height 2.75 :background "gray90")))
  "Face used for variables in the semantic overlay.")

(defface cfmap-semantic-type-face
  '((((background dark))
     (:box (:line-width 1 :color "white")
	   :inherit (font-lock-type-face cfmap-font-face)
	   :height 2.75 :background "gray10"))
    (t (:box (:line-width 1 :color "black")
	     :inherit (font-lock-function-name-face cfmap-font-face)
	     :height 2.75 :background "gray90")))
  "Face used for types in the semantic overlay.")

(defcustom cfmap-width-fraction 0.15
  "Fraction of width which should be used for cfmap sidebar."
  :type 'number
  :group 'cfmap)

(defcustom cfmap-minimum-width 30
  "Minimum width of cfmap in characters (default size).
Use nil to disable."
  :type 'number
  :group 'cfmap)

(defcustom cfmap-window-location 'left
  "Location of the cfmap window.
Can be either the symbol `left' or `right'."
  :type '(choice (const :tag "Left" left)
		 (const :tag "Right" right))
  :group 'cfmap)

(defcustom cfmap-buffer-name " *CFMAP*"
  "Buffer name of cfmap sidebar."
  :type 'string
  :group 'cfmap)

(defcustom cfmap-update-delay 0.1
  "Delay in seconds after which sidebar gets updated.
Setting this to 0 will let the cfmap react immediately, but
this will slow down scrolling."
  :type 'number
  :set (lambda (sym value)
	 (set sym value)
	 (when (and (boundp 'cfmap-timer-object)
		    cfmap-timer-object)
	   (cancel-timer cfmap-timer-object)
	   (setq cfmap-timer-object
		 (run-with-idle-timer
		  cfmap-update-delay t 'cfmap-update))))
  :group 'cfmap)

(defcustom cfmap-always-recenter nil
  "Whether cfmap sidebar should be recentered after every point movement."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-recenter-type 'relative
  "Specifies the type of recentering the cfmap should use.
The cfmap can use different types of recentering, i.e., how the
cfmap should behave when you scroll in the main window or when
you drag the active region with the mouse.  The following
explanations will probably not help much, so simply try them and
choose the one which suits you best.

`relative' -- The position of the active region in the cfmap
corresponds with the relative position of this region in the
buffer.  This the default.

`middle' -- The active region will stay fixed in the middle of
the cfmap.

`free' -- The position will be more or less free.  When dragging
the active region, the cfmap will scroll when you reach the
bottom or top."
  :type '(choice (const :tag "Relative" relative)
		 (const :tag "Middle" middle)
		 (const :tag "Free" free))
  :group 'cfmap)

(defcustom cfmap-hide-scroll-bar t
  "Whether the cfmap should hide the vertical scrollbar."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-hide-fringes nil
  "Whether the cfmap should hide the fringes."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-dedicated-window t
  "Whether the cfmap should create a dedicated window."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-display-semantic-overlays t
  "Display overlays from CEDET's semantic analyzer.
If you use CEDET and the buffer's major-mode is supported, the
cfmap can display overlays generated by the semantic analyzer.
By default, it will apply the faces `cfmap-semantic-<X>-face',
with <X> being \"function\", \"variable\" and \"type\".  Also, it
will display the name of the tag in the middle of the overlay in
the corresponding font-lock face.

See also `cfmap-enlarge-certain-faces', which can be used as
fallback."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-enlarge-certain-faces 'as-fallback
  "Whether certain faces should be enlarged in the cfmap.
All faces listed in `cfmap-normal-height-faces' will be
displayed using the default font height, allowing you to still
read text using those faces.  By default, this should enlarge all
function names in the cfmap, given you have font locking
enabled.  This variable can have the following values:

'as-fallback (the default) -- The feature will only be activated
  if information from CEDET's semantic analyzer isn't available
  (see: `cfmap-display-semantic-overlays').
'always -- Always active.
nil -- Inactive."
  :type '(choice (const :tag "Fallback if CEDET unavailable." as-fallback)
		 (const :tag "Always active." always)
		 (const :tag "Inactive." nil))
  :group 'cfmap)

(defcustom cfmap-normal-height-faces '(font-lock-function-name-face)
  "List of faces which should be displayed with normal height.
When `cfmap-enlarge-certain-faces' is non-nil, all faces in
this list will be displayed using the default font height.  By
default, this list contains `font-lock-function-name-face', so
you can still read function names in the cfmap."
  :type '(repeat face)
  :group 'cfmap)

(defcustom cfmap-sync-overlay-properties '(face invisible)
  "Specifies which overlay properties should be synced.
Unlike text properties, overlays are not applied automatically to
the cfmap and must be explicitly synced.  This variable
specifies which overlay properties should be synced by
`cfmap-sync-overlays'.  Most importantly, this variable should
include 'invisible', so that hidden text does not appear in the
cfmap buffer."
  :type '(repeat symbol)
  :group 'cfmap)

(defcustom cfmap-major-modes '(prog-mode)
  "Major modes for which a cfmap should be created.
This can also be a parent mode like 'prog-mode.
If nil, a cfmap must be explicitly created for each buffer."
  :type '(repeat symbol)
  :group 'cfmap)

(defcustom cfmap-recreate-window t
  "Whether the cfmap window should be automatically re-created.
If this is non-nil, the side window for the cfmap will be
automatically re-created as soon as you kill it."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-automatically-delete-window 'visible
  "Whether the cfmap window should be automatically deleted.
You can choose between three different behaviors here: If this is
`nil', the cfmap window will never be automatically deleted. If
this is set to symbol 'visible, the cfmap stays active as long
as the cfmap's buffer is visible somewhere in the frame,
whether it is active or not. Any other value will delete the
cfmap window as soon as you enter a buffer which is not derived
from `cfmap-major-modes' (excluding the minibuffer)."
  :type '(choice (const :tag "Never delete automatically" nil)
		 (const :tag "Keep as long as buffer visible" visible)
		 (const :tag "Delete when entering unsupported buffer" t))
  :group 'cfmap)

(defcustom cfmap-tag-only nil
  "Whether the cfmap should only display parsed tags from CEDET."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-highlight-line t
  "Whether the cfmap should highlight the current line."
  :type 'boolean
  :group 'cfmap)

(defcustom cfmap-disable-mode-line t
  "Whether to disable the mode-line in the cfmap window."
  :type 'boolen
  :group 'cfmap)

(defcustom cfmap-hide-cursor t
  "Whether to hide the cursor in the cfmap window."
  :type 'boolen
  :group 'cfmap)

;;; Internal variables

;; The buffer currently displayed in the cfmap
(defvar cfmap-active-buffer nil)
;; Window start/end from the base buffer
(defvar cfmap-start nil)
(defvar cfmap-end nil)
;; General overlay for the cfmap
(defvar cfmap-base-overlay nil)
;; Overlay for the active region
(defvar cfmap-active-overlay nil)
;; Timer
(defvar cfmap-timer-object nil)
;; Lines the cfmap can display
(defvar cfmap-numlines nil)
(defvar cfmap-pointmin-overlay nil)
;; Line overlay
(defvar cfmap-line-overlay nil)


;;; Helpers

(defun cfmap-active-current-buffer-p ()
  "Whether the current buffer is displayed in the cfmap."
  (and (eq (current-buffer) cfmap-active-buffer)
       (get-buffer cfmap-buffer-name)
       ;; (with-current-buffer cfmap-buffer-name
       ;;   (eq cfmap-active-buffer (buffer-base-buffer)))
       ))

(defsubst cfmap-get-window ()
  "Get current cfmap window."
  (when (get-buffer cfmap-buffer-name)
    (get-buffer-window cfmap-buffer-name)))

(defsubst cfmap-kill-buffer ()
  "Kill the cfmap buffer."
  (when (get-buffer cfmap-buffer-name)
    (kill-buffer cfmap-buffer-name)))

(defun cfmap-create-window ()
  (let ((width (round (* (window-width)
			 cfmap-width-fraction)))
	buffer-window)
    (when (< width cfmap-minimum-width)
      (setq width cfmap-minimum-width))

    ;; The existing window becomes the cfmap
    (setq buffer-window (split-window-horizontally width))
      ;; Restore prev/next buffers in the new window

    (set-window-next-buffers buffer-window
			     (window-next-buffers))
    (set-window-prev-buffers buffer-window
			     (window-prev-buffers))
    ;; Set up the cfmap window:
    ;; You should not be able to enter the cfmap window.
    (set-window-parameter nil 'no-other-window t)
    ;; Switch to buffer.
    (switch-to-buffer
     (get-buffer-create cfmap-buffer-name) t t)
    ;; Do not fold lines in the cfmap.
    (setq truncate-lines t)
    ;; Make it dedicated.
    (when cfmap-dedicated-window
      (set-window-dedicated-p nil t))
    ;; Return cfmap window, but make sure we select the window where
    ;; the buffer is in.
    (prog1
	(selected-window)
      (select-window buffer-window))))

(defun cfmap-setup-hooks (&optional remove)
  "Hook cfmap into other modes.
If REMOVE is non-nil, remove cfmap from other modes."
  (if remove
      (progn
	(remove-hook 'outline-view-change-hook 'cfmap-sync-overlays)
	(remove-hook 'hs-hide-hook 'cfmap-sync-overlays)
	(remove-hook 'hs-show-hook 'cfmap-sync-overlays)
	(remove-hook 'flycheck-after-syntax-check-hook 'cfmap-sync-overlays))
    ;; outline-(minor-)mode
    (add-hook 'outline-view-change-hook 'cfmap-sync-overlays)
    ;; hideshow
    (add-hook 'hs-hide-hook 'cfmap-sync-overlays)
    (add-hook 'hs-show-hook 'cfmap-sync-overlays)
    (add-hook 'flycheck-after-syntax-check-hook 'cfmap-sync-overlays)))

;;; Cfmap creation / killing

;;;###autoload
(define-minor-mode cfmap-mode
  "Toggle cfmap mode."
  :global t
  :group 'cfmap
  :lighter " MMap"
  (if cfmap-mode
      (progn
	(when (and cfmap-major-modes
		   (apply 'derived-mode-p cfmap-major-modes))
	  (unless (cfmap-get-window)
	    (cfmap-create-window))
	  ;; Create cfmap.
	  (cfmap-new-cfmap))
	;; Create timer.
	(setq cfmap-timer-object
	      (run-with-idle-timer cfmap-update-delay t 'cfmap-update))
	;; Hook into other modes.
	(cfmap-setup-hooks))
    ;; Turn it off
    (cfmap-kill)
    (cfmap-setup-hooks t)))

(defun cfmap-create ()
  "Create a cfmap sidebar."
  (interactive)
  (cfmap-mode 1))

(defun cfmap-min-available-lane (left-pad line-beg line-end)
  (let ((lane-available (make-hash-table))
        (max-lanes 0))
    (save-excursion
      (goto-line line-beg)
      (save-excursion
        ;; TODO: off-by-one?
        (dotimes (i (- line-end line-beg))
          (let* ((columns
                  (-->
                   (- (line-end-position)
                      (line-beginning-position))
                   (- it left-pad)
                   (1- it)
                   (/ it 3))))
            (when (> columns max-lanes)
              (setq max-lanes columns)))
          (forward-line 1)))
      (dotimes (i max-lanes)
        (puthash i lane-available t))
      (save-excursion
        (dotimes (i (- line-end line-beg))
          (forward-char (+ 1 left-pad))

          (let ((end (line-end-position))
                (cur 0))
            (while (< (point) (line-end-position))
              (unless (looking-at-p " ")
                (puthash cur 0 lane-available))
              (forward-char 3)
              (cl-incf cur)))))
      )))

(with-current-buffer (get-buffer "*scratch0*")
  (cfmap-min-available-lane 3 12 20))

(defun cfmap-draw-arrow (dir start end num left-pad arrow-length)
  (cl-labels ((insert-arrow-part (type)
                (beginning-of-line)
                (forward-char num)
                (dotimes (i left-pad)
                  (if (eq (point) (line-end-position))
                      (insert " ")
                    (forward-char 1)))
                (cond
                 ((eq type 'line)
                  (if (eq (point) (line-end-position))
                      (insert "|" (make-string arrow-length ? ))
                    (delete-char 1)
                    (if (not (looking-at-p "|"))
                        (insert "|")
                      (insert "+"))))
                 ((eq type 'ingress)
                  (insert
                   (concat "+"
                           (make-string (1- arrow-length) ?-)
                           ">")))
                 ((eq type 'egress)
                  (insert
                   (concat "+"
                           (make-string arrow-length ?-))))))
              (my-next-line ()
                (forward-line 1)))
    (goto-line start)
    (insert-arrow-part
     (if (eq dir 'up)
         'ingress
       'egress))
    (let ((i 0)
          (end (1- (abs (- start end)))))
      (while (< i end)
        (my-next-line)
        (insert-arrow-part 'line)
        (cl-incf i))
      (my-next-line))
    (insert-arrow-part
     (if (eq dir 'up)
         'egress
       'ingress))))

(defvar cfmap-test
  '(-1
    (5 . 11)
    (7 . 3)
    (15 . 18)
    (2 . 22)
    (24 . 30)
    ;; (22 . 25)
    ))

(defun cfmap--point-inside (p r)
  (and
   (< (car r) p)
   (< p (cdr r))))

(defun cfmap--inside (r1 r2)
  (and
   (cfmap--point-inside (car r1) r2)
   (cfmap--point-inside (cdr r1) r2)))

(defun cfmap-overlapping (r1 r2)
  (or
   (cfmap--point-inside (car r2) r1)
   (cfmap--point-inside (cdr r2) r1)
   (cfmap--inside r1 r2)))

(defun cfmap-regionify (list)
  (let ((points (sort (cdr cfmap-test)
                      (lambda (x y)
                        (< (car x) (car y)))))
        (remaining list)
        regions
        cur-region)
    (labels ((make-one-region
              ()
              (let ((region-over nil)
                    (depth 0)
                    the-region super-region
                    sub-regions)
                (while (and (not region-over)
                            (not (zerop (length remaining))))
                  (let* ((curr-region (car remaining))
                         (curr-start (car curr-region))
                         (curr-end (cdr curr-region)))
                    (cond
                     ((not super-region)
                      (push curr-region the-region)
                      (setq super-region (copy-tree curr-region)
                            remaining (cdr remaining)))
                     ((not (cfmap-overlapping super-region curr-region))
                      ;; Region is officially over
                      (setq region-over t))
                     ((not (cfmap--inside curr-region super-region))
                      ;; Extend current region
                      (push curr-region the-region)
                      (setcdr super-region curr-end)
                      (setq remaining (cdr remaining)))
                     (t
                      (push (make-one-region) sub-regions)))))
                (list :region (reverse the-region)
                      :subregions (reverse sub-regions)))))
      (while (not (zerop (length remaining)))
        (push (make-one-region) regions))
      )
    regions))

(defun cfmap-max-live (region)
  )

;; (cfmap-regionify cfmap-test)

(defvar cfmap-arrow-depth 0)

(progn
  (with-current-buffer (get-buffer "*scratch0*")
    (setq cfmap-arrow-depth 3)
    (erase-buffer)
    ;; (dotimes (i 637)
    ;;   (insert (format "%3d" i) "\n"))
    (dotimes (i 637)
      (insert "\n"))
    (let* ((lines (reverse (cdr cfmap-test)))
           (len (length lines))
           (counter 0))
      (dolist (l lines)
        (let ((a (car l))
              (b (cdr l))
              start end direction)
          (if (< a b)
              (cfmap-draw-arrow 'down a b 0 (* counter 3) (* 3 (- len counter)))
            (cfmap-draw-arrow 'up b a 0 (* counter 3) (* 3 (- len counter))))
          )
        (cl-incf counter)))))

(defun render-cfmap-alist (alist buffer)
  )

(defun cfmap-new-cfmap ()
  "Create new cfmap BUFNAME for current buffer and window.
Re-use already existing cfmap window if possible."
  (interactive)
  (let ((currentbuffer (current-buffer))
	(win (cfmap-get-window))
        (line-numbers (count-lines (point-min) (point-max)))
	;; (indbuf (make-indirect-buffer (current-buffer)
	;; 			      (concat cfmap-buffer-name "_temp"))
        ;;         )
        (other-buffer (get-buffer-create (concat cfmap-buffer-name "_other")))
	(edges (window-pixel-edges)))
    (with-current-buffer other-buffer
      (dotimes (i line-numbers)
        (insert (number-to-string i) "\n")))
    ;; Remember the active buffer currently displayed in the cfmap.
    (setq cfmap-active-buffer (current-buffer))
    (with-selected-window win
      ;; Now set up the cfmap:
      (when (window-dedicated-p)
	(set-window-dedicated-p nil nil))
      (switch-to-buffer other-buffer t t)
      (cfmap-kill-buffer)
      (rename-buffer cfmap-buffer-name)
      ;; Do not fold lines in the cfmap.
      (setq truncate-lines t)
      (when cfmap-dedicated-window
	(set-window-dedicated-p nil t))
      ;; (setq cfmap-base-overlay (make-overlay (point-min) (point-max) nil t t))
      ;; (overlay-put cfmap-base-overlay 'face 'cfmap-font-face)
      ;; (overlay-put cfmap-base-overlay 'priority 1)
      ;; Add the hand mouse pointer to visible text. It doesn’t seem
      ;; possible to set the mouse cursor when there’s no text. See
      ;; `void-text-area-pointer'.
      ;; (overlay-put cfmap-base-overlay 'pointer 'hand)
      ;; (when cfmap-tag-only
      ;;   (overlay-put cfmap-base-overlay 'face
      ;; 		     `(:inherit cfmap-font-face
      ;;   			:foreground ,(face-background 'default))))
      ;; (setq cfmap-pointmin-overlay (make-overlay (point-min) (1+ (point-min))))
      (setq cfmap-start (window-start)
	    cfmap-end (window-end)
	    cfmap-active-overlay (make-overlay cfmap-start cfmap-end)
	    line-spacing 0)
      ;; (overlay-put cfmap-active-overlay 'face
      ;;   	   'cfmap-active-region-background)
      ;; (when cfmap-tag-only
      ;;   (overlay-put cfmap-active-overlay 'face
      ;;   	     `(:inherit 'cfmap-active-region-background
      ;;   		       :foreground ,(face-background 'cfmap-active-region-background))))
      ;; (overlay-put cfmap-active-overlay 'priority 5)
      (when cfmap-disable-mode-line
	(setq mode-line-format nil))
      (when cfmap-hide-cursor
	(setq cursor-type nil))
      (when cfmap-hide-scroll-bar
	(setq vertical-scroll-bar nil)
	(set-window-buffer nil (current-buffer)))
      (when cfmap-hide-fringes
	(set-window-fringes nil 0 0))
      (when (and (boundp 'linum-mode)
		 linum-mode)
	(linum-mode 0))
      (setq buffer-read-only t)
      ;; Calculate the actual number of lines displayable with the cfmap face.
      (setq cfmap-numlines
	    (floor
	     (/
	      (- (nth 3 edges) (nth 1 edges))
	      (car (progn (redisplay t) (window-line-height)))))))
    ;; (cfmap-sync-overlays)
    ))

(defun cfmap-kill ()
  "Kill cfmap."
  (interactive)
  (when (cfmap-get-window)
    (delete-window (cfmap-get-window)))
  (when cfmap-timer-object
    (cancel-timer cfmap-timer-object)))

;;; Cfmap update

(defun cfmap-update (&optional force)
  "Update cfmap sidebar if necessary.
This is meant to be called from the idle-timer or the post command hook.
When FORCE, enforce update of the active region."
  (interactive)
  ;; If we are in the minibuffer, do nothing.
  (unless (active-minibuffer-window)
    (if (cfmap-active-current-buffer-p)
	;; We are still in the same buffer, so just update the cfmap.
	(cfmap-update-current-buffer force)
      ;; We have entered a buffer for which no cfmap should be
      ;; displayed. Check if we should de
      (when (and (cfmap-get-window)
		 (cfmap-need-to-delete-window))
	;; We wait a tiny bit before deleting the window, since we
	;; might only be temporarily in another buffer.
	(run-with-timer 0.3 nil
			(lambda ()
			  (when (and (null (cfmap-active-current-buffer-p))
				     (cfmap-get-window))
			    (delete-window (cfmap-get-window)))))))))

(defun cfmap-need-to-delete-window ()
  "Check if we should delete the cfmap window.
This depends on `cfmap-automatically-delete-window'."
  (if (eq cfmap-automatically-delete-window 'visible)
      (null (get-buffer-window cfmap-active-buffer))
    (null cfmap-automatically-delete-window)))

(defun cfmap-update-current-buffer (force)
  "Update cfmap for the current buffer."
  (let* ((win (cfmap-get-window))
	 (start (window-start))
         (line-number (line-number-at-pos start))
	 (pt (point)))
    (when (and (null win)
	       cfmap-recreate-window)
      ;; The cfmap window is no longer visible, so create it again...
      (setq win (cfmap-create-window))
      ;; ...and switch to existing cfmap buffer.
      (with-selected-window win
	(when (window-dedicated-p)
	  (set-window-dedicated-p nil nil))
	(switch-to-buffer cfmap-buffer-name t t)
	(when cfmap-hide-fringes
	  (set-window-fringes nil 0 0))
	(when cfmap-dedicated-window
	  (set-window-dedicated-p nil t))))
    (with-selected-window win
      ;; Make sure the base overlay spans the whole buffer.
      (let ((line-point
             (save-excursion
               (goto-char (point-min))
               (forward-line (1- line-number))
               (point))))
        (set-window-start nil line-point))
      (goto-char pt)
      (beginning-of-line)
      (when cfmap-highlight-line
	(cfmap-highlight-line)))))

(defun cfmap-highlight-line ()
  "Highlight current line in the cfmap."
  (unless cfmap-line-overlay
    (setq cfmap-line-overlay (make-overlay (point) (1+ (point)) nil t))
    (overlay-put cfmap-line-overlay 'priority 6))
  (overlay-put
   cfmap-line-overlay 'face
   `(:background ,(face-background 'cfmap-current-line-face)
		 :foreground ,(face-foreground 'cfmap-current-line-face)))
  (move-overlay cfmap-line-overlay (point) (line-beginning-position 2)))

;;; Overlay movement

(defun cfmap-line-to-pos (line)
  "Return point position of line number LINE."
  (save-excursion
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))
    (point)))


(provide 'cfmap)

;;; cfmap.el ends here
