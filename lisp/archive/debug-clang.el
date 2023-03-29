;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defvar clang-hash-table (make-hash-table))

(defun attempt-compile (j)
  (interactive)
  (aprog1
      (compilation-start
       (string-join
        (list
         "cd ~/workspace/llvm-project.git/machine-outliner/build/Release/"
         "ninja clean"
         (format "ninja -j %d" j))
        " && "))
    (with-current-buffer it
      (add-hook 'compilation-finish-local-sticky
                (compile-finish-fun j)))))

(defun compile-finish-fun (j)
  `(lambda (buffer event)
     (if (not (string-match-p "exited abnormally" event))
         (message "Compile succeeeded at j=%d" ,j)

       (debug-clang/record-errors ,j buffer)
       (attempt-compile (- ,j 1)))))

(defun debug-clang/record-errors (j buffer)
  (let ((buff (find-file-noselect
               "/home/benson/plaintext/org/org-roam/20230328162512-llvm_won_t_compile.org"))
        (count 0))
    (with-current-buffer buff
      (save-excursion
        (goto-char (point-max))
        (insert (format "* Compilation with -j=%d\n\n" j))
        (dolist (err (debug-clang/get-errors buffer))
          (insert (format "** Failure %d\n\n" count))
          (insert "#+begin_quote\n")
          (insert err)
          (insert "#+end_quote\n\n")
          (incf count))))))

(defun debug-clang/get-errors (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((regexp (rx (group "FAILED: " (+ nonl) "\n"
                               (+ (not "[") (+ nonl) "\n"))))
            l)
        (while (re-search-forward regexp nil t)
          (push (match-string 1) l))
        l)))))

(attempt-compile 40)
