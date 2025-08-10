;;; my-ledger.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2025-08-10 13:52]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package ledger-mode
  :mode "\\.dat\\'"
  :config
  (setq ledger-narrow-on-reconcile nil)

  (setq ledger-reports
        `(("account" "%(binary) -f %(ledger-file) reg %(account)")
          ("credit card" "%(binary) -f %(ledger-file) reg %(account) --aux-date --sort -d")
          ("bal" "%(binary) -f %(ledger-file) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("equity" "%(binary) -f %(ledger-file) bal ^Exp ^RE ^Rev")
          ("uncleared" "%(binary) -f %(ledger-file) reg --uncleared --limit=\"payee!='Texas Instruments Income'\"")
          ("last-superfluous" "%(binary) -f %(ledger-file) bal --limit='account =~ /^Exp:(Food|Luxury|NewTech|People)/ && date >= [this month]'")
          ("superfluous" "%(binary) -f %(ledger-file) reg --limit='account =~ /^Exp:(Food|Luxury|NewTech|People)/'")
          ("recurring" "%(binary) -f %(ledger-file) reg --limit='has_tag(\"RECURRING\")' ^Exp")
          ("expmonth" "%(binary) -f %(ledger-file) -M reg Expenses")
          ("owedmom" "%(binary) -f %(ledger-file) reg Liabilities")
          ("progress" "%(binary) -f %(ledger-file) reg Assets Equity Liabilities")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("lia1" "%(binary) -f %(ledger-file) bal ^Lia --cleared")
          ("lia2" "%(binary) -f %(ledger-file) reg ^Lia --uncleared")
          ("Ast:AR" "%(binary) -f %(ledger-file) bal ^Ast:AR")
          ("earned-money" "%(binary) -f %(ledger-file) bal ^Rev:TI ^Exp:Necessary:Tax ^Exp:Necessary:Insurance ^Exp:Necessary:GroupLife")))

  (setq dynamic-reports
        '(("budgetcal" "%(binary) -f ~/MEGA/org/entries/food.ledger --daily --add-budget reg Expenses")))

  (use-package stripes)

  (add-hook 'ledger-report-after-report-hook
            #'(lambda ()
                (stripes-mode 2)))

  (require 'parse-time)

  (defun ledger-narrow-to-date-range ()
    (interactive)
    (goto-char (line-beginning-position))
    (when (looking-at
           (rx (and
                (separated-list " - "
                                (group (= 2 digit)) "-" (group (= 3 alpha))
                                "-" (= 2 digit)))))
      (let ((year (match-string 1))
            (month-start (cdr (assoc (downcase (match-string 2)) parse-time-months))))
        (setq ledger-report-cmd
              (--> ledger-report-cmd
                   (string-replace " -M" "" it)
                   (string-replace " -n" "" it)
                   (string-replace " -A" "" it)
                   (concat it
                           " "
                           (format " -b 20%s-%d"
                                   year
                                   month-start)
                           (format " -e 20%s-%d" year (1+ month-start)))))
        (ledger-report-redo))))

  (define-key ledger-report-mode-map (kbd "n")
              #'ledger-narrow-to-date-range)

  (defun ledger-accounts-expand-includes (orig)
    (let (includes)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx line-start "include "
                                      (group (+ nonl)))
                                  nil t)
          (push (match-string 1) includes)))
      (append
       (cl-mapcan #'(lambda (file)
                      (with-current-buffer (find-file-noselect
                                            (expand-file-name file))
                        (ledger-accounts-in-buffer)))
                  includes)
       (funcall orig))))

  (advice-add #'ledger-accounts-in-buffer
              :around
              #'ledger-accounts-expand-includes)

  (defun check-account-in-buffer (account)
    (member (list account) (ledger-accounts-in-buffer)))

  (advice-add #'ledger-reconcile-check-valid-account
              :override
              #'check-account-in-buffer)

  ;; TODO there has to be a better way to do this
  (defun save-after-reconcile-toggle (&rest args)
    (save-buffer))

  ;; (advice-add #'ledger-toggle-current
  ;;             :after
  ;;             #'save-after-reconcile-toggle)

  (defun ledger-dynamic-report ()
    (interactive)
    (let* ((ledger-reports dynamic-reports)
           (report-name (ledger-report-read-name)))
      (ledger-report report-name nil)))

  (setq ledger-reconcile-buffer-line-format
        "%(date)s %-4(code)s %-30(payee)s %-30(account)s %15(amount)s\n")

  (defun ledger-account-check-dont-include-regexp (orig account)
    (when (= (aref account 0)
             ?^)
      (setq account
            (substring account 1))))

  (defun ledger-report-show-monthly-average ()
    (interactive)
    (let ((average-string "-A -M -n"))
      (unless (string-match-p average-string ledger-report-cmd)
        (setq ledger-report-cmd
              (--> ledger-report-cmd
                   (replace-regexp-in-string
                    (rx " -b " (+ (not " "))) "" it)
                   (replace-regexp-in-string
                    (rx " -e " (+ (not " "))) "" it)
                   (concat it " " average-string)))
        (ledger-report-redo))))

  (setq ledger-amount-regexp
        (concat
         "\\(  \\|\t\\| \t\\)[ \t]*-?"
         "\\(?:" "?-" ledger-commodity-regexp " *\\)?"
         ;; We either match just a number after the commodity with no
         ;; decimal or thousand separators or a number with thousand
         ;; separators.  If we have a decimal part starting with `,'
         ;; or `.', because the match is non-greedy, it must leave at
         ;; least one of those symbols for the following capture
         ;; group, which then finishes the decimal part.
         "\\(-?\\(?:[0-9]+\\|[0-9,.]+?\\)\\)"
         "\\([,.][0-9)]+\\)?"
         "\\(?: *" ledger-commodity-regexp "\\)?"
         "\\([ \t]*[@={]@?[^\n;]+?\\)?"
         "\\([ \t]+;.+?\\|[ \t]*\\)?$"))

  (define-key ledger-report-mode-map (kbd "M") #'ledger-report-show-monthly-average)

  (defun my/ledger-complete-xact--remove-stars ()
    (interactive)
    (let* ((date-regexp (rx (and line-start (= 4 digit) "/" (= 2 digit) "/" (= 2 digit))))
           (start (save-excursion
                    (re-search-backward date-regexp)
                    (point)))
           (end (save-excursion
                  (or (re-search-forward date-regexp nil t)
                      (end-of-buffer))
                  (beginning-of-line)
                  (point))))
      (save-window-excursion
        (save-restriction
          (narrow-to-region start end)
          (beginning-of-buffer)
          (save-excursion
            (replace-regexp (rx "    "
                                (or "*" "!")
                                " "
                                (group (+ (not (any " " "\n")))))
                            "    \\1  "))
          (save-excursion
            (replace-regexp (rx (and " " (+ " ")
                                     ";; [" (+ (any digit "-" "=" "/")) "]"
                                     line-end))
                            ""))
          (save-excursion
            (replace-regexp (rx line-start (group (+ (any "/" digit)) " ")
                                " ")
                            "\\1"))))))

  (advice-add #'ledger-fully-complete-xact
              :after
              #'my/ledger-complete-xact--remove-stars)

  (defun my/ledger-clean-commodity ()
    (save-excursion
      (beginning-of-buffer)
      (replace-regexp (rx "  -$") "  $-")))

  (advice-add #'ledger-mode-clean-buffer
              :after
              #'my/ledger-clean-commodity)

  (defun my/ledger-convert-alias (account)
    (save-excursion
      (goto-char (point-min))
      (let ((regexp
             (rx line-start
                 "alias " (literal account) "="
                 (group (+ (or alphanumeric ":" "_")))
                 (* space)
                 line-end)))
        (or (and (re-search-forward regexp nil t)
                 (aprog1 (match-string 1)
                   (set-text-properties 0 (length it) nil it)))
            account))))

  (advice-add #'ledger-read-account-with-prompt
              :filter-return
              #'my/ledger-convert-alias)

  (defun my/ledger-field (orig context field)
    (let ((res (funcall orig context field)))
      (if (or (not (eq field 'account))
              (null res)
              (not (string-match (rx (group (separated-list ":" (separated-list " " (+ alphanumeric)))) "  ") res)) )
          res
        (match-string 1 res))))

  ;; (advice-add #'ledger-context-field-value
  ;;             :around
  ;;             #'my/ledger-field)

  (defun my/ledger-reconcile-switch-to-master (&rest args)
    (interactive)
    (switch-to-buffer (find-file-noselect ledger-master-file)))

  ;; (advice-add #'ledger-reconcile
  ;;             :before
  ;;             #'my/ledger-reconcile-switch-to-master)

  (defface ledger-starting-monthly-face
    `((t ,(list
           :background "gray25"
           :extend t
           :inherit font-lock-comment-face
           :box `(:line-width 1 :color "gray30" :style ,(if (>= emacs-major-version 30) 'released-button 'raised)))))
    nil)

  (defun ledger-apply-month-separator ()
    (interactive)
    (remove-overlays nil nil 'face 'ledger-starting-monthly-face)
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (when (looking-at-p (rx line-start
                                (separated " - "
                                           (separated "-" (= 2 digit) (= 3 alpha) (= 2 digit))
                                           (separated "-" (= 2 digit) (= 3 alpha) (= 2 digit)))
                                (+ nonl)))
          (let ((ol (make-overlay (point) (line-end-position))))
            (overlay-put ol 'face 'ledger-starting-monthly-face)
            (overlay-put ol 'priority 5)))
        (next-line))))

  ;; ;; Need some other way to do this
  ;; (add-hook 'ledger-report-mode-hook
  ;;           'ledger-apply-month-separator)
  )

(fset 'credit_card_statement
      [?\M-x ?o ?r ?g ?- ?m ?o ?d ?e return ?\M-x ?q backspace ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?\C-q tab return ?  ?  ?  ?  return ?\M-< ?\C-  ?\C-f ?\C-f ?\C-f ?\C-f ?\C-c ?m ?a ?\C-w ?- ?  ?\[ ?  ?\] ?  ?\C-e ?\C-k ?\C-c ?m ?  ?\C-q tab ?\C-q tab ?\C-e ?\C-j ?y ?\C-a ?_ ?_ ?_ ?_ backspace backspace backspace backspace ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?= ?\C-p ?\C-p ?\C-k ?\C-c ?m ?  ?\C-q tab ?\C-q tab ?\C-d ?\C-d return ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n ?\C-n])

(provide 'my-ledger)
;;; my-ledger.el ends here
