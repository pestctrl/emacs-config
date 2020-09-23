;;; work-commentor.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-09-15 16:13]

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

(global-set-key (kbd "C-c h") #'banner-comment)

(setq banner-comment-char ?-)
(setq banner-comment-start "/*")
(setq banner-comment-end "*/")

(defun banner-comment (&optional end-column)
  "Turn line at point into a banner comment.

Called on an existing banner comment, will reformat it.

Final column will be (or END-COLUMN comment-fill-column fill-column)."
  (interactive "P")
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (forward-to-indentation 0)
      (let ((banner-width (- (or end-column
                                 banner-comment-width
                                 comment-fill-column
                                 fill-column)
                             (current-column)))
            (comment-start (or banner-comment-start comment-start))
            (comment-end (or banner-comment-end comment-end)))
        (narrow-to-region (point) (line-end-position))
        ;; re search to extract existing into: pre(97), text(98), post(99)
        (if (re-search-forward
             (format
              "\\(?97:^\\(%s\\|\\)%s\\)\\(?98:.*?\\)\\(?99:%s\\(%s\\|%s\\|\\)\\)$"
              (or comment-start-skip (regexp-quote (string-trim comment-start)))
              banner-comment-char-match
              banner-comment-char-match
              (regexp-quote (string-trim comment-start))
              (or comment-end-skip (regexp-quote (string-trim comment-start)))))
            (let* ((text (match-string 98))
                   (remaining-width (- banner-width
                                      (length comment-start)
                                      (if (string-empty-p text)
                                          (length text)
                                        (1+ (length text)))
                                      (length comment-end))))
              (if (< remaining-width 0)
                  (error "Text too wide for banner comment"))
              (replace-match ;; replace everything before
               (concat
                comment-start
                (when (not (string-empty-p text)) " "))
               nil nil nil 97)
              (replace-match ;; replace everything after
               (concat
                (make-string remaining-width
                             (if (string-empty-p text)
                                 banner-comment-char
                               ? ))
                comment-end)
               nil nil nil 99)))))))

(provide 'work-commentor)
;;; work-commentor.el ends here
