;;; test-enhanced-diagnostics.el --- Test enhanced bracket type detection

(load-file "claude-paren-diagnostics.el")

(defun format-type-mismatch (mismatch)
  "Format a single type mismatch for display."
  (format "  Line %d col %d: Opened '%c', expected '%c', found '%c'\n    Context: %s"
          (plist-get mismatch :open-line)
          (plist-get mismatch :open-col)
          (plist-get mismatch :opened)
          (plist-get mismatch :expected)
          (plist-get mismatch :closed)
          (substring (plist-get mismatch :open-context) 0
                    (min 60 (length (plist-get mismatch :open-context))))))

(defun test-file-diagnostics (filename)
  "Run enhanced diagnostics on FILENAME."
  (princ (format "\n========================================\n"))
  (princ (format "Testing: %s\n" filename))
  (princ (format "========================================\n\n"))

  ;; Test 1: Count by type
  (princ "1. Bracket Counts by Type:\n")
  (let ((counts (claude/count-by-type filename)))
    (princ (format "   () Parens:  %d open, %d close (diff: %+d)\n"
                   (plist-get counts :paren-open)
                   (plist-get counts :paren-close)
                   (plist-get counts :paren-diff)))
    (princ (format "   [] Squares: %d open, %d close (diff: %+d)\n"
                   (plist-get counts :square-open)
                   (plist-get counts :square-close)
                   (plist-get counts :square-diff)))
    (princ (format "   {} Curlies: %d open, %d close (diff: %+d)\n"
                   (plist-get counts :curly-open)
                   (plist-get counts :curly-close)
                   (plist-get counts :curly-diff)))
    (princ (format "   Total imbalance: %d\n\n"
                   (plist-get counts :total-diff))))

  ;; Test 2: Type mismatch detection
  (princ "2. Type Mismatch Detection:\n")
  (let ((type-check (claude/find-type-mismatches filename)))
    (if (string= (plist-get type-check :status) "ok")
        (princ (format "   ✓ %s\n\n" (plist-get type-check :message)))
      (progn
        (princ (format "   ✗ Found %d type mismatch(es):\n\n"
                       (plist-get type-check :count)))
        (dolist (mismatch (plist-get type-check :type-mismatches))
          (when (eq (plist-get mismatch :error) 'type-mismatch)
            (princ (format-type-mismatch mismatch))
            (princ "\n\n"))))))

  ;; Test 3: Comprehensive summary
  (princ "3. Overall Assessment:\n")
  (let ((summary (claude/bracket-summary filename)))
    (princ (format "   %s\n\n" (plist-get summary :recommendation)))))

;; Run tests on all test files
(test-file-diagnostics "test-square-brackets.cljc")
(test-file-diagnostics "test-curly-braces.cljc")
(test-file-diagnostics "test-type-mismatches.cljc")

(princ "========================================\n")
(princ "All diagnostic tests complete!\n")
(princ "========================================\n")
