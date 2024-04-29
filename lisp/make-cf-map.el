;;; make-cf-map.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-04-26 08:11]

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

(defvar cfmap-width nil)
(make-variable-buffer-local 'cfmap-width)

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

(defun cfmap-transform (cfmap)
  (let ((new-list))
    (dolist (l cfmap)
      (let ((a (car l))
            (b (cdr l)))
        (push (if (< a b)
                  (cons 'down
                        (cons a b))
                (cons 'up
                      (cons b a)))
              new-list)))
    (sort new-list
          (lambda (x y)
            (let ((size1 (/ (abs (- (cadr x) (cddr x))) 5))
                  (size2 (/ (abs (- (cadr y) (cddr y))) 5)))
              (if (not (= size1 size2))
                  (< size1 size2)
                (< (cadr x) (cadr y))))))))

;; (defvar cfmap-transformed (cfmap-transform cfmap-test))

(defface cfmap-face1
  `((t ,(list
         :inherit 'default
         :foreground "#9E1CB2")))
  nil)

(defface cfmap-face2
  `((t ,(list
         :inherit 'default
         :foreground "#47B04B")))
  nil)

(defface cfmap-face3
  `((t ,(list
         :inherit 'default
         :foreground "#1194f6")))
  nil)

(defface cfmap-face4
  `((t ,(list
         :inherit 'default
         :foreground "#C90067")))
  nil)

(defface cfmap-face5
  `((t ,(list
         :inherit 'default
         :foreground "#FFED18")))
  nil)

(defface cfmap-face6
  `((t ,(list
         :inherit 'default
         :foreground "white")))
  nil)

(defvar cfmap-faces
  '(cfmap-face1
    cfmap-face2
    cfmap-face3
    cfmap-face4
    cfmap-face5
    cfmap-face6))

(defun cfmap-draw-arrow (dir start end arrow-length)
  (let ((arrow-face (nth (mod (/ (1- arrow-length) 2)
                              (length cfmap-faces))
                         cfmap-faces)))
    (cl-labels ((my-insert
                  (str)
                  (insert (propertize str 'face arrow-face)))
                (insert-arrow-part (type num &optional end)
                  (beginning-of-line)
                  (if (eq type 'line)
                      (progn
                        (unless (looking-at-p "\n")
                          (delete-forward-char 1))
                        (my-insert (if (= num 0)
                                       (if (eq dir 'up)
                                           "▲"
                                         "▼")
                                     "│")))
                    (let ((intersect (if end "└" "┌")))
                      (cond
                       ((eq type 'ingress)
                        (if (eq (point) (line-end-position))
                            (my-insert
                             (concat intersect
                                     (make-string (1- arrow-length) ?─)
                                     "►"))
                          (delete-forward-char 1)
                          (my-insert intersect)
                          (dotimes (i (1- arrow-length))
                            (let ((char
                                   (cond ((looking-at-p (rx (or "│" "┼" "▲" "▼"))) "┼")
                                         ((looking-at-p (rx (or "┌" "┬"))) "┬")
                                         ((looking-at-p (rx (or "└" "┴"))) "┴")
                                         (t "─"))))
                              (unless (looking-at-p "\n")
                                (delete-forward-char 1))
                              (my-insert char)))
                          (unless (looking-at-p "\n")
                            (delete-forward-char 1))
                          (my-insert "►")))
                       ((eq type 'egress)
                        (if (eq (point) (line-end-position))
                            (my-insert
                             (concat intersect
                                     (make-string arrow-length ?─)))
                          (delete-forward-char 1)
                          (my-insert intersect)
                          (dotimes (i arrow-length)
                            (let ((char
                                   (cond ((looking-at-p (rx (or "│" "┼" "▲" "▼"))) "┼")
                                         ((looking-at-p (rx (or "┌" "┬"))) "┬")
                                         ((looking-at-p (rx (or "└" "┴"))) "┴")
                                         (t "─"))))
                              (unless (looking-at-p "\n")
                                (delete-forward-char 1))
                              (my-insert char)))))))))
                (my-next-line ()
                  (forward-line 1)))
      (goto-line start)
      (insert-arrow-part
       (if (eq dir 'up)
           'ingress
         'egress) 1)
      (let ((i 0)
            (end (1- (abs (- start end)))))
        (while (< i end)
          (my-next-line)
          (insert-arrow-part 'line (mod i 5))
          (cl-incf i))
        (my-next-line))
      (insert-arrow-part
       (if (eq dir 'up)
           'egress
         'ingress)
       1
       t))))

(defun cfmap-render-buffer (buffer cfmap)
  (with-current-buffer buffer
    (erase-buffer)
    (dotimes (i 4000)
      (insert "\n"))
    (let* ((remaining (cfmap-transform cfmap))
           (arrow-length 3)
           previous-end
           new-list

           super-start super-end)
      (while (not (zerop (length remaining)))
        (overwrite-mode 1)
        (setq previous-end nil
              first-start nil
              new-list nil)

        (dolist (l remaining)
          (if (and previous-end
                   (< (cadr l) previous-end))
              (push l new-list)
            (cfmap-draw-arrow (car l) (cadr l) (cddr l) arrow-length))

          (when (or (not super-start )
                    (< (cadr l) super-start))
            (setq super-start (cadr l)))

          (when (or (not super-end )
                    (< super-end (cddr l)))
            (setq super-end (cddr l)))

          (let ((end (cddr l)))
            (when (or (not previous-end)
                      (< previous-end end))
              (setq previous-end end))))

        (overwrite-mode -1)

        (let ((start (save-excursion
                       (goto-line super-start)
                       (line-beginning-position)))
              (end (save-excursion
                     (goto-line super-end)
                     (line-beginning-position))))
          (string-rectangle start end "  "))

        (setq remaining (reverse new-list)
              arrow-length (+ 2 arrow-length)))
      (setq cfmap-width arrow-length))))

(provide 'make-cf-map)
;;; make-cf-map.el ends here
