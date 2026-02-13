;;; test-paren-diagnostics.el --- Test file with various paren issues

;;; Test case 1: Missing closing paren
(defun test-missing-close (x)
  (if (> x 0)
      (message "positive")
  ;; Missing closing paren here!

;;; Test case 2: Extra closing paren
(defun test-extra-close (x)
  (if (> x 0)
      (message "positive")))
  ) ;; Extra paren!

;;; Test case 3: Mismatched brackets
(defun test-mismatched (x)
  (let ((result [1 2 3}))  ;; [ closed with }
    result))

;;; Test case 4: Wrong nesting
(defun test-wrong-nesting (x)
  (if (> x 0))
      (message "positive")
   (message "negative"))

;;; Test case 5: Multiple errors
(defun test-multiple (x y)
  (let ((a (+ x y))
    (if (> a 0)
        (message "result: %s" a)  ;; Missing let closing
  (message "done")

;;; test-paren-diagnostics.el ends here
