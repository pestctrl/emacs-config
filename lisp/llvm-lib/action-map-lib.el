;;; action-map-lib.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-12-16 18:55]

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

(defun aml/map-to-char-prompt (map separator)
  (mapconcat #'(lambda (x) (plist-get (cdr x) :description))
             map
             (or separator " ")))

(defun aml/get-map-prop (map action prop)
  (plist-get (alist-get action map)
             prop))

(defun aml/read-action-map (map &optional separator)
  (if (= 1 (length map))
      (car (first map))
    (let ((lookup-table (mapcar #'(lambda (x)
                                    (cons (plist-get (cdr x)
                                                     :key)
                                          (car x)))
                                map))
          key)
      (while (not (alist-get key lookup-table))
        (setq key (read-char (aml/map-to-char-prompt map separator))))
      (alist-get key lookup-table))))

(provide 'action-map-lib)
;;; action-map-lib.el ends here
