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
                                              ,@body)
                                            (kill-buffer ,buffer-gensym)))
                               do (org-end-of-subtree t t)))
               (kill-buffer buffer)))))))

(ert-deftest canary-test ()
  (should t))

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

(ert-run-tests-interactively t)

;; (progn (setq org-disabled-tests nil) (mapcar (lambda (sym) (put sym 'ert--test nil)) (apropos-internal "" #'ert-test-boundp)))
