;;; test-all-bracket-files.el --- Test all bracket diagnostic tools

;; Test script to run all diagnostic methods on all test files

(defun count-brackets-by-type (file)
  "Count each bracket type separately in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((paren-open 0) (paren-close 0)
          (square-open 0) (square-close 0)
          (curly-open 0) (curly-close 0))
      (goto-char (point-min))
      (while (re-search-forward "[][(){}]" nil t)
        (let ((char (char-before)))
          (cond ((eq char ?\() (setq paren-open (1+ paren-open)))
                ((eq char ?\)) (setq paren-close (1+ paren-close)))
                ((eq char ?\[) (setq square-open (1+ square-open)))
                ((eq char ?\]) (setq square-close (1+ square-close)))
                ((eq char ?\{) (setq curly-open (1+ curly-open)))
                ((eq char ?\}) (setq curly-close (1+ curly-close))))))
      (list :paren-open paren-open
            :paren-close paren-close
            :paren-diff (- paren-open paren-close)
            :square-open square-open
            :square-close square-close
            :square-diff (- square-open square-close)
            :curly-open curly-open
            :curly-close curly-close
            :curly-diff (- curly-open curly-close)))))

(defun test-file (filename)
  "Test FILENAME and print results."
  (princ (format "\n========================================\n"))
  (princ (format "Testing: %s\n" filename))
  (princ (format "========================================\n\n"))

  (let ((counts (count-brackets-by-type filename)))
    (princ "Bracket Counts by Type:\n")
    (princ (format "  () Parentheses:   %d open, %d close (diff: %+d)\n"
                   (plist-get counts :paren-open)
                   (plist-get counts :paren-close)
                   (plist-get counts :paren-diff)))
    (princ (format "  [] Square brackets: %d open, %d close (diff: %+d)\n"
                   (plist-get counts :square-open)
                   (plist-get counts :square-close)
                   (plist-get counts :square-diff)))
    (princ (format "  {} Curly braces:  %d open, %d close (diff: %+d)\n"
                   (plist-get counts :curly-open)
                   (plist-get counts :curly-close)
                   (plist-get counts :curly-diff)))

    (let ((total-diff (+ (abs (plist-get counts :paren-diff))
                        (abs (plist-get counts :square-diff))
                        (abs (plist-get counts :curly-diff)))))
      (princ (format "\nTotal imbalance: %d brackets\n" total-diff))
      (if (zerop total-diff)
          (princ "⚠️  BALANCED - May have type mismatches!\n")
        (princ (format "✓ Detected %d bracket imbalances\n" total-diff))))))

;; Test all files
(test-file "test-square-brackets.cljc")
(test-file "test-curly-braces.cljc")
(test-file "test-type-mismatches.cljc")
(test-file "test-all-brackets.cljc")

(princ "\n========================================\n")
(princ "All tests complete!\n")
(princ "========================================\n")
