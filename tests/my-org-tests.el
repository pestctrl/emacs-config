
(ert-deftest canary-test ()
  (should t))

(defvar tests-directory "~/.emacs.d/tests/files/")

(defmacro define-org-file-point-test (test-name file point func)
  `(ert-deftest ,test-name ()
     (let ((to-be-removed (find-file-noselect ,file)))
       (with-current-buffer to-be-removed
         (org-cycle '(64))
         (goto-char ,point)
         (let ((r (org-entry-get (point) "RESULT")))
           (should (eq (and r (intern r))
                       (funcall ,func))))))))

(defmacro org-test-function-on-file-individual (func file)
  (declare (indent defun))
  (let ((file (expand-file-name file tests-directory)))
    `(let ((count 0)
           (buf (find-file-noselect ,file)))
       (with-current-buffer buf
         (goto-char (point-min))
         (while (not (eobp))
           (when (org-entry-get (point) "TEST")
             (let ((test-name (intern (format "test%d-%s" count (symbol-name ,func))))
                   (p (point)))
               (define-org-file-point-test test-name ,file p ,func))
             (incf count))
           (outline-next-heading))))))

(defmacro org-test-function-on-file-individual (func file)
  (declare (indent defun))
  (let ((file (expand-file-name file tests-directory)))
    `(progn
       ,@(mapcar
          (lambda (p)
            (let ((test-name (intern (format "test%d-%s" p (symbol-name (cadr func))))))
              `(define-org-file-point-test ,test-name ,file ,p ,func)))
          (get-all-points-that-require-tests file)))))

(defun get-all-points-that-require-tests (file)
  (let ((visited? (get-file-buffer file))
        (buff (find-file-noselect file))
        (res '()))
    (with-current-buffer buff
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-entry-get (point) "TEST")
          (push (point) res))
        (outline-next-heading)))
    (unless visited?
      (kill-buffer buff))
    res))


(org-test-function-on-file-individual #'my/no-children "children.org")

(defun my/get-project-type-ambiguous-stuck ()
  (my/get-project-type buffer-file-name (point) t))

(org-test-function-on-file-individual #'my/get-project-type-ambiguous-stuck
  "~/.emacs.d/tests/files/projects.org")
