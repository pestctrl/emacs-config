;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(require 'cl)

(defvar self-chat-num 0)
(defvar self-chat-characters nil)
(defvar self-chat-random-list nil)
(defvar self-chat-last nil)
(defvar self-chat-last2 nil)
(defvar self-chat-highlights nil)
(defvar self-chat-alist nil)

(define-derived-mode self-chat-mode fundamental-mode "self-chat"
  "This is a mode where I become crazy and talk to myself."
  (setq font-lock-defaults '(self-chat-highlights))
  (olivetti-mode 1))

(modify-syntax-entry ?\" " " self-chat-mode-syntax-table)

(define-key self-chat-mode-map (kbd "RET") #'self-chat-insert-next)
(define-key self-chat-mode-map (kbd "DEL") #'delete-self-chat-line)
(define-key self-chat-mode-map (kbd "C-DEL") #'sc/delete-full-line)
(define-key self-chat-mode-map (kbd "M-DEL") #'delete-self-chat-text)

(defun self-chat-insert-next ()
  (interactive)
  (call-interactively #'user-chat/body))

(defun chat-pre ()
  (when (or (not self-chat-last)
            (not (= self-chat-num self-chat-last)))
    (setq self-chat-last2 self-chat-last
          self-chat-last self-chat-num))
  (unless (and (eobp) (bobp))
    ;; Are we fixing up an old line, or creating a new one?
    (if (save-excursion
          (beginning-of-line)
          (not (looking-at (rx (+ nonl) ":"))))
        (progn
          (beginning-of-line)
          (kill-line))
      (call-interactively #'newline)
      (call-interactively #'newline)))
  (insert "> "))

(defun chat-post ()
  (if-let ((name (alist-get self-chat-num self-chat-alist)))
      (insert name ": ")))

(defun sc/delete-full-line ()
  (interactive)
  (set-mark-command nil)
  (previous-line)
  (previous-line)
  (end-of-line)
  (call-interactively #'kill-region))

(defun delete-self-chat-text ()
  (interactive)
  (beginning-of-line)
  (search-forward ":")
  (call-interactively #'forward-char)
  (call-interactively #'set-mark-command)
  (end-of-line)
  (call-interactively #'kill-region))

(defun delete-self-chat-line ()
  (interactive)
  (save-excursion
    (call-interactively #'set-mark-command)
    (beginning-of-line)
    (call-interactively #'kill-ring-save))
  (cond ((= 0 (length (current-kill 0)))
         (call-interactively #'backward-char))
        ((string-match-p
          (rx line-start ">" (optional " ") (optional (+ alpha) ":" (optional " ")) line-end)
          (current-kill 0))
         (sc/delete-full-line))
        (t
         (call-interactively #'delete-backward-char))))

(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)

(defmacro define-users (list)
  `(progn
     (setq self-chat-num 0
           self-chat-last nil
           self-chat-last2 nil
           self-chat-highlights nil
           self-chat-alist nil
           self-chat-characters
           ',(remove-if-not (lambda (char)
                              (cadddr char))
                            list))
     (defhydra user-chat (:exit t
                                :body-pre (chat-pre)
                                :after-exit (chat-post))
       ""
       ("RET" nil "Same")
       ("TAB" (when self-chat-last2 (setq self-chat-num self-chat-last2)) "Switch")
       ,@(let ((num 0))
           (mapcar #'(lambda (triple)
                       (let ((name (car triple))
                             (key (cadr triple)))
                         (prog1 `(,key (setq self-chat-num ,num) ,name)
                           (cl-incf num))))
                   list))
       ("r" (setq self-chat-num
                  (progn
                    (unless self-chat-random-list
                      (setq self-chat-random-list
                            (--> (length self-chat-characters)
                                 (number-sequence 0 (1- it))
                                 (shuffle it))))
                    (pop self-chat-random-list)))
        "Random"))
     ,@(let ((num 0))
         (mapcan #'(lambda (triple)
                     (let* ((name (car triple))
                            (color (caddr triple))
                            (face-sym (intern (format "sc-%s-face" (downcase name)))))
                       (prog1`((add-to-list 'self-chat-alist
                                            '(,num . ,name))
                               (defface ,face-sym '((t (:foreground ,color))) ,(concat name
                                                                                       "'s face for self-chat-mode"))
                               (defvar ,face-sym ',face-sym)
                               (add-to-list 'self-chat-highlights
                                            '(,(format "^> %s:.*$" name) . ,face-sym)))
                             (cl-incf num))))
                 list))))

(define-users (("Tau" "t" "turquoise1" t)
               ("Arc" "a" "gold" t)
               ("Tom" "T" "rosy brown" t)
               ("Dan" "d" "spring green" t)
               ("Ver" "v" "white" t)
               ("Coop" "c" "cornsilk" t)
               ("Net" "n" "magenta" t)

               ("Someone" "s" "dim gray" nil)
               ("Everyone" "e" "pale green" nil)
               ("Note" "N" "dim gray" nil)))

(provide 'self-chat-mode)
