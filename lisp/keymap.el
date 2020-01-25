(defmacro exwm-global-set-key (keybinding function)
  `(progn
     (when (boundp 'exwm-input-global-keys)
         (add-to-list 'exwm-input-global-keys
                      (cons ,keybinding ,function)))
     (global-set-key ,keybinding ,function)))

(exwm-global-set-key (kbd "M-T") 'flop-frame)
(exwm-global-set-key (kbd "s-k") (lambda () (interactive) (kill-buffer (current-buffer))))

(defconst my/keymap-key (kbd "C-t"))

;; Disable C-t for all others
(with-eval-after-load "vterm"
  (define-key vterm-mode-map (kbd "C-t") nil))
(with-eval-after-load "ibuf-ext"
  (define-key ibuffer-mode-map my/keymap-key nil))
(with-eval-after-load "dired"
  (define-key dired-mode-map my/keymap-key nil))

(define-prefix-command '*root-map*)
(exwm-global-set-key (kbd "C-t") '*root-map*)

(define-key *root-map* (kbd "C-n") 'switch-window)
(define-key *root-map* (kbd "i") 'org-mru-clock-in)
(define-key *root-map* (kbd "C-i") 'leaving-computer)
(define-key *root-map* (kbd "C") 'org-resolve-clocks)
(define-key *root-map* (kbd "j") 'org-clock-goto)
(define-key *root-map* (kbd "o") 'switch-window)
(define-key *root-map* (kbd "n") 'toggle-notifications)
(define-key *root-map* (kbd "RET") 'dired-jump)
(define-key *root-map* (kbd "C-b") 'previous-buffer)

(provide 'keymap)
