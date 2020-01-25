;;; workspace-prompter.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2020-01-25 13:43]

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


  (defun prompt-workspace (&optional prompt)
    "Prompt for a workspace, returning the workspace frame."
    (exwm-workspace--update-switch-history)
    (let* ((current-idx (exwm-workspace--position exwm-workspace--current))
           (history-add-new-input nil)  ;prevent modifying history
           (history-idx (read-from-minibuffer
                         (or prompt "Workspace: ")
                         (elt exwm-workspace--switch-history current-idx)
                         exwm-workspace--switch-map nil
                         `(exwm-workspace--switch-history . ,current-idx)))
           (workspace-idx (mod (1- (cl-position history-idx exwm-workspace--switch-history
                                                :test #'equal)) 
                               10)))
      (elt exwm-workspace--list workspace-idx)))

  (advice-add 'exwm-workspace--prompt-for-workspace
              :override
              #'prompt-workspace)

(provide 'workspace-prompter)
;;; workspace-prompter.el ends here
