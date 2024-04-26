;;; cf-map-test.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-04-26 07:05]

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
(require 'cf-map)

(ert-deftest inside-test ()
  (should-not (cfmap--inside '(1 . 10) '(2 . 5)))
  (should-not (cfmap--inside '(1 . 10) '(8 . 12)))
  (should-not (cfmap--inside '(5 . 10) '(2 . 7)))
  (should     (cfmap--inside '(2 . 5)  '(1 . 10))))

(ert-deftest overlapping-test ()
  (should (cfmap-overlapping '(1 . 10) '(2 . 5)))
  (should (cfmap-overlapping '(1 . 10) '(8 . 12)))
  (should (cfmap-overlapping '(5 . 10) '(2 . 7)))
  (should (cfmap-overlapping '(2 . 5)  '(1 . 10)))

  (should-not (cfmap-overlapping '(1 . 2) '(3 . 4)))
  (should-not (cfmap-overlapping '(5 . 6) '(3 . 4))))

(ert-deftest regionify-one-region ()
  (should (= 1 (length (cfmap-regionify '((1 . 10) (2 . 5))))))
  (should (= 1 (length (cfmap-regionify '((1 . 10) (8 . 12))))))
  (should (= 1 (length (cfmap-regionify '((5 . 10) (2 . 7))))))
  (should (= 1 (length (cfmap-regionify '((2 . 5)  (1 . 10))))))

  (should (= 2 (length (cfmap-regionify '((1 . 2)  (3 . 4)))))))

(ert-deftest test-subregions ()
  ;; This is one region with 2 sub-regions
  (should (= 2
             (length
              (plist-get (car (cfmap-regionify '((1 . 10) (2 . 3) (4 . 5))))
                         :subregions))))
  ;; One subregion, since things overlap
  (should (= 1
             (length
              (plist-get (car (cfmap-regionify '((1 . 10) (2 . 4) (3 . 5))))
                         :subregions)))))

;; (ert-run-tests-interactively t)

(provide 'cf-map-test)
;;; cf-map-test.el ends here
