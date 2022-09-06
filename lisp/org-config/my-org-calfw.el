;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun mocfw/split-element-into-3 (element)
  (cl-flet ((timestamp-to-str
             (str)
             (save-match-data
               (string-match (rx (and "<"
                                      (group (= 4 digit)) "-"
                                      (group (= 2 digit)) "-"
                                      (group (= 2 digit))
                                      (optional " " (+ (not ">")))
                                      ">"))
                             str)
               (list (string-to-number (match-string 2 str))
                     (string-to-number (match-string 3 str))
                     (string-to-number (match-string 1 str)))))
            (timestamp-from-element
             (element)
             (--> element
                  (plist-get it 'timestamp)
                  (plist-get it :raw-value))))
    (let* ((marker (org-element-property :org-marker element))
           (heading (with-current-buffer (marker-buffer marker)
                      (goto-char (marker-position marker))
                      (org-get-heading)))
           result)
      (when-let (scheduled (org-element-property :scheduled element))
        (push (list (timestamp-to-str (timestamp-from-element scheduled))
                    heading)
              result))
      (when-let (deadline (org-element-property :deadline element))
        (push (list (timestamp-to-str (timestamp-from-element deadline))
                    heading)
              result))
      (when-let (delayed (with-current-buffer (marker-buffer marker)
                           (goto-char (marker-position marker))
                           (org-entry-get (point) "DELAYED")))
        (push (list (timestamp-to-str delayed)
                    heading)
              result))
      result)))

(defun my/cfw:org-schedule-period-to-calendar (begin end)
  (let* ((start-str (format "%04d-%02d-%02d"
                            (caddr begin)
                            (car begin)
                            (cadr begin)))
         (end-str (format "%04d-%02d-%02d"
                          (caddr end)
                          (car end)
                          (cadr end)))
         (results (org-ql-query :select 'element-with-markers
                                :from org-agenda-files
                                :where `(and (todo)
                                             (or (scheduled :from ,start-str :to ,end-str)
                                                 (deadline :from ,start-str :to ,end-str)
                                                 (and (property "DELAYED")
                                                      (when-let (d (org-entry-get (point) "DELAYED"))
                                                        (and (org-time> d (org-matcher-time ,start-str))
                                                             (org-time< d (org-matcher-time ,end-str)))) )))
                                :order-by '(date priority todo)))
         (aug-results (mapcan #'mocfw/split-element-into-3
                              results))
         alist)
    (dolist (i (reverse aug-results))
      (let ((date (car i))
            (element-str (cadr i)))
        (if-let (value (alist-get date alist))
            (setf (alist-get date alist)
                  (cons element-str value))
          (setf (alist-get date alist)
                (list element-str)))))
    alist))

;; (let* ((str (--> debug/result
;;                  (plist-get it 'headline)
;;                  (plist-get it :org-marker)))
;;        (element-str (org-ql-view--format-element i))
;;        (date (save-match-data
;;                (string-match (rx (and "<"
;;                                       (group (= 4 digit)) "-"
;;                                       (group (= 2 digit)) "-"
;;                                       (group (= 2 digit))
;;                                       (optional " " (+ (not ">")))
;;                                       ">"))
;;                              str)
;;                (list (string-to-number (match-string 2 str))
;;                      (string-to-number (match-string 3 str))
;;                      (string-to-number (match-string 1 str))))))
;;   (if-let (value (alist-get date alist))
;;       (setf (alist-get date alist)
;;             (cons element-str value))
;;     (setf (alist-get date alist)
;;           (list element-str))))

;; (my/cfw:org-schedule-period-to-calendar '(8 28 2022) '(9 10 2022))
;; (my/cfw:org-schedule-period-to-calendar '(9 01 2022) '(9 02 2022))

;; (message "%s" )



;; (plist-get (plist-get (org-element-property :scheduled debug/result) 'timestamp)
;;               :raw-value)

;; (get-text-property 0  debug/result)

(provide 'my-org-calfw)
