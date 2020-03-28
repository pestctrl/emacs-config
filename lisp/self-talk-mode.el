;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defvar self-chat-num 0)
(defvar self-chat-last nil)
(defvar self-chat-last2 nil)
(defvar self-chat-highlights nil)
(defvar self-chat-alist nil)

(define-derived-mode self-chat-mode fundamental-mode "self-chat"
  "This is a mode where I become crazy and talk to myself."
  (setq font-lock-defaults '(self-chat-highlights))
  (call-interactively #'choose-user/body))

(define-key self-chat-mode-map (kbd "RET") #'self-chat-insert-next)

(defun self-chat-insert-next ()
  (interactive)
  (call-interactively #'newline)
  (call-interactively #'newline)
  (call-interactively #'user-chat/body))

(defun chat-pre ()
  (when (and self-chat-last
             (not (= self-chat-num self-chat-last)))
    (setq self-chat-last2 self-chat-last
          self-chat-last self-chat-num))
  (insert "> "))

(defun chat-post ()
  (insert (alist-get self-chat-num self-chat-alist) ": "))

(defmacro define-users (list)
  `(progn
     (setq self-chat-num 0
           self-chat-last nil
           self-chat-last2 nil
           self-chat-highlights nil
           self-chat-alist nil)
     (defhydra user-chat (:exit t
                          :body-pre (chat-pre)
                          :after-exit (chat-post))
       ""
       ("RET" nil "Same")
       ("TAB" (setq self-chat-num self-chat-last2) "Switch")
       ,@(let ((num 0))
           (mapcar #'(lambda (triple)
                       (let ((name (car triple))
                             (key (cadr triple)))
                         (prog1 `(,key (setq self-chat-num ,num) ,name)
                           (incf num))))
                   list))
       ("r" (setq self-chat-num (random ,(length list))) "Random"))
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
                             (incf num))))
                 list))))

(define-users (("Tau" "t" "turquoise1")
               ("Arc" "a" "gold")
               ("Tom" "T" "rosy brown")
               ("Dan" "d" "spring green")
               ("Ver" "v" "white")
               ("Coop" "c" "cornsilk")
               ("Everyone" "e" "pale green")))

(provide 'self-talk-mode)
