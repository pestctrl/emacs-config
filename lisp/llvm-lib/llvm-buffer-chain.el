;;; llvm-chain.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-10-23 10:12]

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

(defclass llvm-source-buffer-chain ()
  ((producing-act-buffer :initarg :prev   :type buffer :initform nil)
   ;; TODO: Should be a list of buffers
   (next-act-buffer     :initarg :next   :type buffer :initform nil))
  :allow-nil-initform t)

(defvar lbc/source-buffer-info nil)
(make-variable-buffer-local 'lbc/source-buffer-info)

(defun lbc/source-jump-to-next-act-buffer ()
  (interactive)
  ;; TODO: Window layout management
  (pop-to-buffer (slot-value lbc/source-buffer-info 'next-act-buffer)))

(defun lbc/source-jump-to-producing-act-buffer ()
  (interactive)
  ;; TODO: Window layout management
  (aif (slot-value lbc/source-buffer-info 'producing-act-buffer)
      (pop-to-buffer it)
    (user-error "No producing buffer")))

(defvar lbc/source-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-e") #'lbc/source-jump-to-next-act-buffer)
    (define-key map (kbd "M-a") #'lbc/source-jump-to-producing-act-buffer)
    map))

(define-minor-mode lbc/source-buffer-mode ""
  :keymap lbc/source-buffer-mode-map
  :global nil
  (when lbc/source-buffer-mode
    (unless lbc/source-buffer-info
      (setq lbc/source-buffer-info
            (make-instance 'llvm-source-buffer-chain)))))

(defclass llvm-act-buffer-chain ()
  ((previous-act-buffer :initarg :prev   :type buffer :initform nil)
   ;; TODO: Should be a list of buffers
   (next-act-buffer     :initarg :next   :type buffer :initform nil)
   (source-file         :initarg :source :type string)
   (dest-file           :initarg :dest   :type string))
  :allow-nil-initform t)

(defvar lbc/act-buffer-info nil)
(make-variable-buffer-local 'lbc/act-buffer-info)

(defun lbc/act-jump-to-source-file-buffer ()
  (interactive)
  (pop-to-buffer (find-file-noselect (slot-value lbc/act-buffer-info 'source-file)))
  (delete-other-windows))

(defun lbc/act-jump-to-dest-file-buffer ()
  (interactive)
  (let ((dest-file (slot-value lbc/act-buffer-info 'dest-file))
        (buff (current-buffer)))
    (if (not dest-file)
        (user-error "No destination file")
      (let ((buffer (find-file-noselect dest-file)))
        (with-current-buffer buffer
          (lbc/source-buffer-mode 1)
          (setf (slot-value lbc/source-buffer-info 'producing-act-buffer)
                buff))
        (pop-to-buffer buffer)
        (delete-other-windows)))))

(defun lbc/act-jump-to-previous-act-buffer ()
  (interactive)
  (aif (slot-value lbc/act-buffer-info 'previous-act-buffer)
      (pop-to-buffer it)
    (user-error "No previous act buffer")))

(defun lbc/act-jump-to-next-act-buffer ()
  (interactive)
  (aif (slot-value lbc/act-buffer-info 'next-act-buffer)
      (pop-to-buffer it)
    (user-error "No next act buffer")))

(defun lbc/act-again ()
  (interactive)
  (call-interactively #'lbc/act-jump-to-dest-file-buffer)
  (call-interactively #'ll/act-on-file))

(defvar lbc/act-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-s") #'lbc/act-jump-to-source-file-buffer)
    (define-key map (kbd "M-d") #'lbc/act-jump-to-dest-file-buffer)
    (define-key map (kbd "M-a") #'lbc/act-jump-to-previous-act-buffer)
    (define-key map (kbd "M-e") #'lbc/act-jump-to-next-act-buffer)
    (define-key map (kbd "a")   #'lbc/act-again)
    map))

(define-minor-mode lbc/act-buffer-mode ""
  :keymap lbc/act-buffer-mode-map
  :global nil
  (when lbc/act-buffer-mode
    (unless lbc/act-buffer-info
      (setq lbc/act-buffer-info
            (make-instance 'llvm-act-buffer-chain)))))

(defun lbc/dump ()
  (interactive)
  (when lbc/act-buffer-info
    (with-slots (previous-act-buffer next-act-buffer source-file dest-file) lbc/act-buffer-info
      (message "previous: %s\nnext: %s\nsource: %s\ndest: %s"
               previous-act-buffer next-act-buffer source-file dest-file))))

(defun lbc/around-act (orig &rest args)
  (let ((buff-orig (current-buffer))
        (previous-act (slot-value lbc/source-buffer-info 'producing-act-buffer))
        (act-buffer (apply orig args)))
    (with-current-buffer buff-orig
      ;; Make sure info is initialized
      (lbc/source-buffer-mode 1)

      (setf (slot-value lbc/source-buffer-info 'next-act-buffer)
            act-buffer))

    ;; Setup links in the act buffer
    (with-current-buffer act-buffer
      (lbc/act-buffer-mode 1)

      (setf (slot-value lbc/act-buffer-info 'source-file)
            (buffer-file-name buff-orig))
      (setf (slot-value lbc/act-buffer-info 'dest-file)
            ll/act-on-file-output))

    (when previous-act
      (with-current-buffer act-buffer
        (setf (slot-value lbc/act-buffer-info 'previous-act-buffer)
              previous-act))
      (with-current-buffer previous-act
        (setf (slot-value lbc/act-buffer-info 'next-act-buffer)
              act-buffer)))

    ;; TODO: every act should produce a file by default
    ;; (setf (slot-value lbc/act-buffer-info 'dest-file)
    ;;       "")
    ))

(advice-add #'ll/act-on-c-file
            :around
            #'lbc/around-act)

(advice-add #'ll/act-on-ll-file
            :around
            #'lbc/around-act)

(advice-add #'recompile
            :after
            #'lbc/act-buffer-mode)

(provide 'llvm-buffer-chain)
;;; llvm-chain.el ends here
