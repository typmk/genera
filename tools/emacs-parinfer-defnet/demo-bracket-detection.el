;;; demo-bracket-detection.el --- Demonstration of bracket type detection

;; This file demonstrates the three bracket type detection functions

(require 'bracket-type-detection)

;; Demo 1: Count brackets by type in a test file
(defun demo-count-by-type ()
  "Show bracket counts separated by type."
  (interactive)
  (let ((result (claude/count-by-type "C:/Users/Apollo/em/test-type-mismatches.cljc")))
    (message "Bracket Counts:\n  Parens (): %d open, %d close (diff: %d)\n  Squares []: %d open, %d close (diff: %d)\n  Curlies {}: %d open, %d close (diff: %d)\n  Total imbalance: %d"
             (plist-get result :paren-open)
             (plist-get result :paren-close)
             (plist-get result :paren-diff)
             (plist-get result :square-open)
             (plist-get result :square-close)
             (plist-get result :square-diff)
             (plist-get result :curly-open)
             (plist-get result :curly-close)
             (plist-get result :curly-diff)
             (plist-get result :total-diff))))

;; Demo 2: Find type mismatches
(defun demo-type-mismatches ()
  "Show first few type mismatches found."
  (interactive)
  (let* ((result (claude/find-type-mismatches "C:/Users/Apollo/em/test-type-mismatches.cljc"))
         (status (plist-get result :status))
         (count (plist-get result :count))
         (mismatches (plist-get result :type-mismatches)))
    (if (string= status "ok")
        (message "✓ No type mismatches found!")
      (message "✗ Found %d type mismatches\n\nFirst mismatch:\n  Line %d: Opened with '%c' but closed with '%c' (expected '%c')\n  Context: %s"
               count
               (plist-get (car mismatches) :open-line)
               (plist-get (car mismatches) :opened)
               (plist-get (car mismatches) :closed)
               (plist-get (car mismatches) :expected)
               (plist-get (car mismatches) :open-context)))))

;; Demo 3: Comprehensive summary
(defun demo-bracket-summary ()
  "Show comprehensive bracket analysis."
  (interactive)
  (let* ((result (claude/bracket-summary "C:/Users/Apollo/em/test-type-mismatches.cljc"))
         (recommendation (plist-get result :recommendation)))
    (message "Bracket Summary:\n  %s" recommendation)))

;; Demo 4: Test on a balanced file
(defun demo-balanced-file ()
  "Test on bracket-type-detection-clean.el (should be balanced)."
  (interactive)
  (let* ((result (claude/bracket-summary "C:/Users/Apollo/em/bracket-type-detection-clean.el"))
         (recommendation (plist-get result :recommendation)))
    (message "Testing bracket-type-detection-clean.el:\n  %s" recommendation)))

(provide 'demo-bracket-detection)
;;; demo-bracket-detection.el ends here
