;;; org-capture-emacs-exit-warn.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-08-02 10:50]

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

(defvar capture-count 0)

(defun warn-active-capture-template ()
  ;; (or (zerop capture-count)
  ;;     (progn (pop-to-buffer
  ;;             (get-buffer
  ;;              (car
  ;;               (remove-if-not (lambda (b)
  ;;                                (let ((case-fold-search nil))
  ;;                                  (and b
  ;;                                       (buffer-name b)
  ;;                                       (string-match-p "CAPTURE-.*" (buffer-name b)))))
  ;;                              (buffer-list)))))
  ;;            (yes-or-no-p "Active capture templates exist; exit anyway? ")))
  t
  )

(add-hook 'kill-emacs-query-functions
          #'warn-active-capture-template)

(add-hook 'org-capture-mode-hook
          #'(lambda ()
              (incf capture-count)))

(add-hook 'org-capture-prepare-finalize-hook
          #'(lambda ()
              (decf capture-count)))

(provide 'org-capture-emacs-exit-warn)
;;; org-capture-emacs-exit-warn.el ends here
