;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defvar self-chat-highlights nil)

(defface sc-tau-face3 `((t (:foreground "turquoise1"))) "")
(setq sc-tau-face 'sc-tau-face3)
(defface sc-arc-face `((t (:foreground "gold"))) "")
(setq sc-arc-face 'sc-arc-face)
(defface sc-tom-face `((t (:foreground "rosy brown"))) "")
(setq sc-tom-face 'sc-tom-face)
(defface sc-dan-face `((t (:foreground "spring green"))) "")
(setq sc-dan-face 'sc-dan-face)
(defface sc-ver-face `((t (:foreground "white"))) "")
(setq sc-ver-face 'sc-ver-face)
(defface sc-coop-face4 `((t (:foreground "cornsilk"))) "")
(setq sc-coop-face 'sc-coop-face4)

(setq self-chat-highlights
      '(
        ("^> Tau:.*$" . sc-tau-face)
        ("^> Arc:.*$" . sc-arc-face)
        ("^> Tom:.*$" . sc-tom-face)
        ("^> Dan:.*$" . sc-dan-face)
        ("^> Ver:.*$" . sc-ver-face)
        ("^> Coop:.*$" . sc-coop-face)
        ))

(define-derived-mode self-chat-mode fundamental-mode "self-chat"
  "This is a mode where I become crazy and talk to myself."
  (setq font-lock-defaults '(self-chat-highlights)))

(define-key self-chat-mode-map (kbd "RET") #'self-chat-insert-next)

(defvar self-chat-num 0)

(defvar self-chat-alist
  '((0 . "Tau")
    (1 . "Arc")
    (2 . "Tom")
    (3 . "Dan")
    (4 . "Ver")
    (5 . "Coop")))

(defun self-chat-insert-next ()
  (interactive)
  (call-interactively #'newline)
  (call-interactively #'newline)
  (insert "> ")
  (call-interactively #'choose-name/body))


(defhydra choose-name (:exit t)
  ""
  ("RET" nil "Same")
  ("t" (insert (alist-get 0 self-chat-alist) ": ") "Tau")
  ("a" (insert (alist-get 1 self-chat-alist) ": ") "Arc")
  ("T" (insert (alist-get 2 self-chat-alist) ": ") "Tom")
  ("d" (insert (alist-get 3 self-chat-alist) ": ") "Dan")
  ("v" (insert (alist-get 4 self-chat-alist) ": ") "Ver")
  ("c" (insert (alist-get 5 self-chat-alist) ": ") "Coop"))
