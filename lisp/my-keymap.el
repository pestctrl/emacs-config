(defvar pestctrl-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode pestctrl-minor-mode ""
  :keymap pestctrl-minor-mode-map :global t)

;; (add-hook 'after-load-functions 'my-keys-have-priority)

;; (defun my-keys-have-priority (_file)
;;   "Try to ensure that my keybindings retain priority over other minor modes.

;; Called via the `after-load-functions' special hook."
;;   (unless (eq (caar minor-mode-map-alist) 'pestctrl-minor-mode)
;;     (let ((mykeys (assq 'pestctrl-minor-mode minor-mode-map-alist)))
;;       (message "%s" (key-binding (kbd "C-t")))
;;       (assq-delete-all 'pestctrl-minor-mode minor-mode-map-alist)
;;       (message "%s" (key-binding (kbd "C-t")))
;;       (add-to-list 'minor-mode-map-alist mykeys))))

(defmacro exwm-global-set-key (keybinding function)
  `(progn
     (use-exwm
       :config
       (with-eval-after-load "exwm"
         (add-to-list 'exwm-input-global-keys
                      (cons ,keybinding ,function))))))

(defmacro exwm-line-mode-set-key (keybinding function)
  `(progn
     (with-eval-after-load "exwm"
       (define-key exwm-mode-map ,keybinding ,function))
     (define-key pestctrl-minor-mode-map ,keybinding ,function)))

(pestctrl-minor-mode 1)

(exwm-global-set-key (kbd "s-k") #'kill-current-buffer)
(exwm-global-set-key (kbd "s-u") #'org-capture)

(exwm-line-mode-set-key (kbd "M-T") 'flop-frame)
(exwm-line-mode-set-key (kbd "M-Q") #'bury-buffer)
(exwm-line-mode-set-key (kbd "M-P") #'previous-buffer)
(exwm-line-mode-set-key (kbd "M-N") #'next-buffer)
(exwm-line-mode-set-key (kbd "M-o") #'other-window)
;; (add-to-list 'exwm-input-global-keys
;;              (cons (kbd "C-g") #'keyboard-quit))

(ec/load-or-ask-key 'my/keymap-key
                    'my/keymap-key-key
                    "What would you like *root-map* to be bound to?")

(use-exwm
  :config
  (with-eval-after-load "exwm"
    (add-to-list 'exwm-input-prefix-keys my/keymap-key-key)))

;; Disable C-t for all others
(with-eval-after-load "dired"
  (define-key dired-mode-map my/keymap-key nil))

(define-prefix-command '*root-map*)
(define-key *root-map* (kbd my/keymap-key) (key-binding (kbd "C-t")))
(exwm-line-mode-set-key (kbd my/keymap-key) '*root-map*)

(define-key *root-map* (kbd "C-n") 'switch-window)
(define-key *root-map* (kbd "i") 'org-mru-clock-in)
(define-key *root-map* (kbd "C-i") 'leaving-computer)
(define-key *root-map* (kbd "RET") 'dired-jump)
(define-key *root-map* (kbd "C-b") 'previous-buffer)

(with-eval-after-load "org"
  (define-key *root-map* (kbd "!") #'(lambda ()
                                       (interactive)
                                       (org-time-stamp-inactive t))))

(define-prefix-command '*lazy-map*)
(unless my/puppet-p
  (exwm-global-set-key (kbd "<f1>") '*lazy-map*))

(define-key *lazy-map* (kbd "1") #'org-capture)
(define-key *lazy-map* (kbd "2") #'org-agenda)

(defun toggle-notifications ()
  (interactive)
  (shell-command "kill -s USR1 $(pidof deadd-notification-center)"))
(define-key *root-map* (kbd "n") 'toggle-notifications)

(when (<= 27 emacs-major-version)
  (require 'switch-tabs)

  (defun projectile-switch-switch-tab (project arg)
    (let* ((dir-name (-> project
                        (directory-file-name)
                        (file-name-nondirectory)))
           (tab-name
            (cond ((member dir-name '("emacs-config" ".emacs.d")) "emacs-devel")
                  ((member dir-name '("agenda" "org" "work")) "org"))))
      (when tab-name
        (switch-or-create-tab tab-name))))

  (advice-add #'projectile-switch-project-by-name
              :before
              #'projectile-switch-switch-tab))

(when my/puppet-p
  (define-key *root-map* (kbd "C-x C-s") #'org-save-all-org-buffers)
  (define-key *root-map* (kbd "C-f") #'next-buffer))

(provide 'my-keymap)
