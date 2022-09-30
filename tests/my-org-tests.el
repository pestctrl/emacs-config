(require 'org)
(require 'org-project)

(defvar org-tests-directory "~/.emacs.d/tests/files/")
(defvar org-disabled-tests '())

(defmacro org-test/parents-should (file &rest body)
  (declare (indent defun))
  (let ((file (expand-file-name file org-tests-directory))
        (buffer-gensym (gensym "buffer")))
    `(progn
       ,@(save-window-excursion
           (let ((buffer (find-file-noselect file)))
             (prog1 (with-current-buffer buffer
                      (beginning-of-buffer)
                      (when (not (org-at-heading-p))
                        (outline-next-heading))
                      (cl-loop until (eobp)
                               for test-name = (intern (replace-regexp-in-string " " "-" (org-get-heading t nil t t)))
                               if (member "disabled" (org-get-tags))
                               do (add-to-list 'org-disabled-tests
                                               test-name)
                               else
                               when (not (member test-name org-disabled-tests))
                               collect `(ert-deftest ,test-name ()
                                          (let ((,buffer-gensym (find-file-noselect ,file)))
                                            (with-current-buffer ,buffer-gensym
                                              (goto-char ,(point))
                                              ,@body)))
                               do (org-end-of-subtree t t)))
               (kill-buffer buffer)))))))

(defmacro org-test/test-each (file)
  (declare (indent defun))
  (let ((file (expand-file-name file org-tests-directory))
        (buffer-gensym (gensym "buffer")))
    `(progn
       ,@(save-window-excursion
           (let ((buffer (find-file-noselect file)))
             (prog1 (with-current-buffer buffer
                      (beginning-of-buffer)
                      (when (not (org-at-heading-p))
                        (outline-next-heading))
                      (let ((end-of-tree (save-excursion (org-end-of-subtree t t))))
                        (cl-loop until (eobp)
                                 for test-name = (intern (replace-regexp-in-string " " "-" (org-get-heading t nil t t)))
                                 if (member "disabled" (org-get-tags))
                                 do (add-to-list 'org-disabled-tests
                                                 test-name)
                                 else
                                 when (not (member test-name org-disabled-tests))
                                 collect `(ert-deftest ,test-name ()
                                            (let ((,buffer-gensym (find-file-noselect ,file)))
                                              (with-current-buffer ,buffer-gensym
                                                (goto-char ,(point))
                                                ,@(save-excursion
                                                    (while (progn (next-line)
                                                                  (beginning-of-line)
                                                                  (not (and (let ((context (org-element-context)))
                                                                              (and (eq 'src-block
                                                                                       (car context))
                                                                                   (string= "emacs-lisp"
                                                                                            (plist-get (cadr context) :language))))))))
                                                    (let ((body (org-babel--expand-body (org-babel-get-src-block-info))))
                                                      (read (concat "(" body ")")))))))
                                 do (org-end-of-subtree t t))))))))))

(ert-deftest canary-test ()
  (should t))

(org-test/test-each "canary.org")

(org-test/parents-should "seq-stuck.org"
  (should (eq 'stuck (opr/type-of-project))))

(org-test/parents-should "seq-active.org"
  (should (eq 'active (opr/type-of-project))))

(org-test/parents-should "seq-invis.org"
  (should (eq 'invis (opr/type-of-project))))

(org-test/parents-should "meta-stuck-base.org"
  (should (eq 'stuck (opr/type-of-project))))

(org-test/parents-should "meta-active-base.org"
  (should (eq 'active (opr/type-of-project))))

(org-test/parents-should "meta-ambiguous.org"
  (should
   (let ((opr/meta-active-if-one-active nil))
     (eq 'stuck (opr/type-of-project))))
  (should
   (let ((opr/meta-active-if-one-active t))
     (eq 'active (opr/type-of-project)))))

(org-test/parents-should "empty-stuck.org"
  (should (eq 'stuck (opr/type-of-project))))

(org-test/parents-should "empty-invis.org"
  (should (eq 'invis (opr/type-of-project))))

(org-test/parents-should "skip-functions.org"
  (should (null (my/org-agenda-skip-unless-prod-tag)))
  (should (not (null (my/org-agenda-skip-unless-dev-tag)))))

(org-test/test-each "wait-behavior.org")

(ert-run-tests-interactively t)

;; (progn (setq org-disabled-tests nil) (mapcar (lambda (sym) (put sym 'ert--test nil)) (apropos-internal "" #'ert-test-boundp)))
