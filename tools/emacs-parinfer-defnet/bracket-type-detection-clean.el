;;; bracket-type-detection-clean.el --- Type-aware bracket diagnostics

;; Standalone file with type mismatch detection functions
;; Can be loaded independently or integrated into claude-paren-diagnostics.el

(defun claude/find-type-mismatches (buffer-or-file)
  "Find bracket TYPE mismatches (e.g., [ closed with }).
Stack-based detection that tracks which bracket type was opened
and validates the closing bracket matches.

Returns list of mismatches with:
- :opened - the opening bracket character
- :closed - the actual closing bracket found
- :expected - what closing bracket was expected
- :open-line - line number where bracket was opened
- :open-col - column where opened
- :close-line - line number where closed
- :close-col - column where closed
- :context - text around the error

This is the ONLY Emacs-native tool that can detect type mismatches.
Example: [1 2 3} has balanced count but wrong types."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (stack '())  ; Stack of (char line col)
        (mismatches '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[][(){}]" nil t)
          (let* ((pos (1- (point)))
                 (char (char-after pos))
                 (line (line-number-at-pos pos))
                 (col (save-excursion (goto-char pos) (current-column))))
            (cond
             ;; Opening bracket - push to stack
             ((memq char '(?\( ?\[ ?\{))
              (push (list :char char :line line :col col :pos pos) stack))

             ;; Closing bracket - check type match
             ((memq char '(?\) ?\] ?\}))
              (if (null stack)
                  ;; Unmatched closing bracket
                  (push (list :error 'unmatched-close
                             :closed char
                             :line line
                             :col col
                             :context (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                        mismatches)
                ;; Check if types match
                (let* ((opener (pop stack))
                       (open-char (plist-get opener :char))
                       (expected-close (cdr (assq open-char
                                                   '((?\( . ?\))
                                                     (?\[ . ?\])
                                                     (?\{ . ?\}))))))
                  (unless (eq char expected-close)
                    ;; TYPE MISMATCH!
                    (push (list :error 'type-mismatch
                               :opened open-char
                               :closed char
                               :expected expected-close
                               :open-line (plist-get opener :line)
                               :open-col (plist-get opener :col)
                               :close-line line
                               :close-col col
                               :open-context (save-excursion
                                              (goto-char (plist-get opener :pos))
                                              (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)))
                               :close-context (buffer-substring-no-properties
                                              (line-beginning-position)
                                              (line-end-position)))
                          mismatches))))))))))

    ;; Return results
    (if mismatches
        (list :status "error"
              :count (length mismatches)
              :type-mismatches (nreverse mismatches)
              :unmatched-opens (length stack))
      (list :status "ok"
            :message (if stack
                        (format "No type mismatches, but %d unclosed brackets"
                               (length stack))
                      "All brackets have correct types and are balanced")))))

(defun claude/count-by-type (buffer-or-file)
  "Count each bracket type separately.
Returns counts for (), [], {} independently.
Useful for identifying which bracket type has imbalance."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer)))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((paren-open 0) (paren-close 0)
              (square-open 0) (square-close 0)
              (curly-open 0) (curly-close 0))
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
                :curly-diff (- curly-open curly-close)
                :total-open (+ paren-open square-open curly-open)
                :total-close (+ paren-close square-close curly-close)
                :total-diff (+ (abs (- paren-open paren-close))
                              (abs (- square-open square-close))
                              (abs (- curly-open curly-close)))))))))

(defun claude/bracket-summary (buffer-or-file)
  "Comprehensive bracket analysis with type awareness.
Combines counting, type mismatch detection, and balance checking.
This is the RECOMMENDED function for complete bracket diagnostics."
  (let ((counts (claude/count-by-type buffer-or-file))
        (type-check (claude/find-type-mismatches buffer-or-file)))
    (list :counts counts
          :type-check type-check
          :recommendation
          (cond
           ;; Perfect: balanced and correct types
           ((and (zerop (plist-get counts :total-diff))
                 (string= (plist-get type-check :status) "ok"))
            "✓ All brackets balanced and correctly typed")

           ;; Type errors present
           ((string= (plist-get type-check :status) "error")
            (format "✗ %d type mismatch(es) detected - opened with one type, closed with another"
                   (plist-get type-check :count)))

           ;; Imbalanced but types correct
           ((> (plist-get counts :total-diff) 0)
            (format "✗ %d bracket(s) imbalanced but types are correct"
                   (plist-get counts :total-diff)))

           (t "✓ Brackets appear balanced")))))

(provide 'bracket-type-detection)
;;; bracket-type-detection-clean.el ends here
