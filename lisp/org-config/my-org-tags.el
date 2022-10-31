;;; my-org-tags.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-05-06 18:48]

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
(setq org-tag-alist
      '((:startgrouptag)
        ("all" . nil)
        (:grouptags)
        ("time" . nil)
        ("nontime" . nil)
        (:endgrouptag)
        (:startgrouptag)
        ("time" . nil)
        (:grouptags)
        ("prod" . ?1)
        (:endgrouptag)
        (:startgrouptag)
        ("nontime" . nil)
        (:grouptags)
        ("sandbox" . ?3)
        (:endgrouptag)
        (:startgrouptag)
        ("sandbox" . ?3)
        (:grouptags)
        ("dev" . ?2)
        ("people" . nil)
        (:endgrouptag)
        (:startgroup . nil)
        ("short" . ?s)
        ("long" . ?l)
        (:endgroup . nil)
        (:startgroup . nil)
        ("watch" . ?w)
        ("read" . ?r)
        (:endgroup . nil)
        (:startgroup . nil)
        ("grow" . ?g)
        ("rest" . ?R)
        (:endgroup . nil)
        (:startgroup . nil)
        ("active" . ?a)
        ("idle" . ?i)
        (:endgroup . nil)
        ;; (:startgrouptag)
        ;; ("online")
        ;; (:grouptags)
        ;; ("article")
        ;; (:endgrouptag)
        ;; (:startgrouptag)
        ;; ("read")
        ;; (:grouptags)
        ;; ("article")
        ;; (:endgrouptag)
        (:startgrouptag)
        ("active")
        (:grouptags)
        ("prog")
        (:endgrouptag)
        (:startgrouptag)
        ("people" . nil)
        (:grouptags)
        ("family" . nil)
        (:endgrouptag)
        ))


(setq org-agenda-hide-tags-regexp
      (rx (or "time" "nontime" "prod" "dev" "sandbox"
              "refile"
              "short" "long" "watch" "read" "grow" "rest" "active" "idle"
              (and symbol-start "_" (+ nonl) "_" symbol-end))))

(defconst category-tags '("computers"))

(provide 'my-org-tags)
;;; my-org-tags.el ends here
