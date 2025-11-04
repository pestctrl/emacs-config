(defun my/org-ql-stuck-projects (tag)
  (let (narrow-p old-beg old-end)
    (when-let* ((from (pcase org-agenda-overriding-restriction
                        ('nil (org-agenda-files nil 'ifmode))
                        ('file (get 'org-agenda-files 'org-restrict))
                        ('subtree (prog1 org-agenda-restrict
                                    (with-current-buffer org-agenda-restrict
                                      ;; Narrow the buffer; remember to widen it later.
                                      (setf old-beg (point-min) old-end (point-max)
                                            narrow-p t)
                                      (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end)))))))
      (let* ((org-todo-keywords-1 '("EMPTY" "ONE" "META" "META1" "TODO" "TASK"))
             (items (mapcan #'my/get-project-stuck-displayables
                            (org-ql-select from
                              `(and ,@(when (and tag
                                                 (not (zerop (length tag))))
                                        `((tags ,tag)))
                                    (todo "TODO" "TASK" "ONE" "META" "META1" "EMPTY" "SEQ")
                                    (my/top-level)
                                    (not (property "DELAYED"))
                                    (or (eq 'stuck (opr/type-of-task))
                                        (eq 'stuck (opr/type-of-project))))
                              :action 'element-with-markers
                              :narrow narrow-p))))
        (when narrow-p
          ;; Restore buffer's previous restrictions.
          (with-current-buffer from
            (narrow-to-region old-beg old-end)))
        (org-agenda-prepare)
        ;; FIXME: `org-agenda--insert-overriding-header' is from an Org version newer than
        ;; I'm using.  Should probably declare it as a minimum Org version after upgrading.
        ;;  (org-agenda--insert-overriding-header (or org-ql-block-header (org-ql-agenda--header-line-format from query)))
        (insert (org-add-props org-ql-block-header
                    nil 'face 'org-agenda-structure 'org-agenda-type 'search) "\n")
        ;; Calling `org-agenda-finalize' should be unnecessary, because in a "series" agenda,
        ;; `org-agenda-multi' is bound non-nil, in which case `org-agenda-finalize' does nothing.
        ;; But we do call `org-agenda-finalize-entries', which allows `org-super-agenda' to work.
        (let ((org-agenda-sorting-strategy-selected '(category-keep) ))
          (->> items
               (-map #'org-ql-view--format-element)
               org-agenda-finalize-entries
               insert))
        (insert "\n")))))

(provide 'work-org-ql)
