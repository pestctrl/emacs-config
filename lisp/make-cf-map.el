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

(defvar cfmap-test
  '((5 . 11)
    (7 . 3)
    (15 . 18)
    (2 . 22)
    (24 . 30)
    ;; (22 . 25)
    ))

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
            (< (cadr x)
               (cadr y))))))

(defvar cfmap-transformed (cfmap-transform cfmap-test))

(defun cfmap-draw-arrow (dir start end arrow-length)
  (cl-labels ((insert-arrow-part (type)
                (beginning-of-line)
                (cond
                 ((eq type 'line)
                  (unless (looking-at-p "\n")
                    (delete-forward-char 1))
                  (insert "|"))
                 ((eq type 'ingress)
                  (if (eq (point) (line-end-position))
                      (insert
                       (concat "+"
                               (make-string (1- arrow-length) ?-)
                               ">"))
                    (delete-forward-char 1)
                    (insert "+")
                    (dotimes (i (1- arrow-length))
                      (let ((char
                             (if (looking-at-p "|") "+" "-")))
                        (unless (looking-at-p "\n")
                          (delete-forward-char 1))
                        (insert char)))
                    (insert ">")))
                 ((eq type 'egress)
                  (if (eq (point) (line-end-position))
                      (insert
                       (concat "+"
                               (make-string arrow-length ?-)))
                    (delete-forward-char 1)
                    (insert "+")
                    (dotimes (i arrow-length)
                      (let ((char
                             (if (looking-at-p "|") "+" "-")))
                        (unless (looking-at-p "\n")
                          (delete-forward-char 1))
                        (insert char)))))))
              (my-next-line ()
                (forward-line 1)))
    (goto-line start)
    (insert-arrow-part
     (if (eq dir 'up)
         'ingress
       'egress))
    (let ((i 0)
          (end (1- (abs (- start end)))))
      (while (< i end)
        (my-next-line)
        (insert-arrow-part 'line)
        (cl-incf i))
      (my-next-line))
    (insert-arrow-part
     (if (eq dir 'up)
         'egress
       'ingress))))

(progn
  (with-current-buffer (get-buffer "*scratch0*")
    (erase-buffer)
    (dotimes (i 637)
      (insert "\n"))
    (let* ((remaining cfmap-transformed)
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
              arrow-length (+ 2 arrow-length))))))

(provide 'make-cf-map)
;;; make-cf-map.el ends here
