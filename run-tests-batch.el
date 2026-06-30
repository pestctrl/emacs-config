;;; run-tests-batch.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>

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

;; Minimal, non-interactive entry point for running the test suite under
;; CI.  Rather than booting the full configuration (init.el), it puts only
;; `lisp/' and `elpa/' on the load-path and requires just what the tests
;; need, then runs ERT in batch mode so the process exits non-zero when a
;; test fails.  The interactive runner in `run-tests.el' calls
;; `ert-run-tests-interactively', which is unsuitable for CI.

;;; Code:

(setq user-emacs-directory (expand-file-name "./"))

(dolist (dir (list (expand-file-name "lisp/")
                   (expand-file-name "elpa/")))
  (add-to-list 'load-path dir)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'org)
(require 'org-project)

;; The agenda skip predicates live in `my-org-agenda-commands', which pulls
;; in much of the configuration.  The tests only exercise these two tag
;; checks, so define them here rather than loading that whole module.
(defun my/org-agenda-skip-unless-prod-tag ()
  (unless (member "prod" (org-get-tags))
    (outline-next-heading)
    (point)))

(defun my/org-agenda-skip-unless-dev-tag ()
  (unless (member "dev" (org-get-tags))
    (outline-next-heading)
    (point)))

;; The tests file ends with `(ert-run-tests-interactively t)'; suppress that
;; interactive call so loading only defines the tests.
(cl-letf (((symbol-function 'ert-run-tests-interactively) #'ignore))
  (load (expand-file-name "tests/my-org-tests.el")))

(ert-run-tests-batch-and-exit t)

;;; run-tests-batch.el ends here
