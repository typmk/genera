;;; claude-paren-diagnostics.el --- Diagnostic helpers for LLM to quickly identify paren issues

;;; Commentary:
;; This file provides functions optimized for LLM consumption.
;; Goals:
;; 1. Precise error location (line + column)
;; 2. Context around error
;; 3. Minimal token usage
;; 4. Structured output (easy to parse)
;; 5. Multiple diagnostic methods

;;; Code:

(defun claude/diagnose-parens (&optional buffer-or-file)
  "Comprehensive paren diagnostics for BUFFER-OR-FILE.
Returns a plist with detailed error information optimized for LLM analysis.
Includes: error location, context, suggested fixes."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer)))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (condition-case err
            (progn
              (check-parens)
              (list :status "ok"
                    :message "All parentheses balanced"
                    :buffer (buffer-name)
                    :size (buffer-size)))
          (error
           (let* ((error-pos (point))
                  (error-line (line-number-at-pos error-pos))
                  (error-col (save-excursion
                              (goto-char error-pos)
                              (current-column)))
                  (line-start (line-beginning-position))
                  (line-end (line-end-position))
                  (line-text (buffer-substring-no-properties line-start line-end))
                  (context-start (max (point-min) (- error-pos 100)))
                  (context-end (min (point-max) (+ error-pos 100)))
                  (context (buffer-substring-no-properties context-start context-end)))
             (list :status "error"
                   :error-type (car err)
                   :message (error-message-string err)
                   :buffer (buffer-name)
                   :position error-pos
                   :line error-line
                   :column error-col
                   :line-text line-text
                   :context context
                   :char-at-error (char-to-string (char-after error-pos))))))))))

(defun claude/scan-parens (&optional buffer-or-file)
  "Scan for paren mismatches using forward-sexp.
More detailed than check-parens - finds ALL issues, not just first."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (issues '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case err
              (progn
                (forward-sexp 1)
                ;; Successfully moved, continue
                )
            (error
             ;; Found an issue
             (let* ((error-pos (point))
                    (error-line (line-number-at-pos error-pos))
                    (error-col (current-column))
                    (line-text (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
               (push (list :position error-pos
                          :line error-line
                          :column error-col
                          :error (error-message-string err)
                          :line-text line-text)
                     issues)
               ;; Skip past error to continue scanning
               (forward-char 1))))))
      (if issues
          (list :status "error"
                :count (length issues)
                :issues (nreverse issues))
        (list :status "ok"
              :message "No scan-parse errors found")))))

(defun claude/show-paren-depth (buffer-or-file)
  "Show parenthesis depth at each line.
Useful for identifying where nesting goes wrong."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (depth-info '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((current-line 1)
              (depths '()))
          (while (not (eobp))
            (let ((depth (car (syntax-ppss))))
              (when (or (zerop (length depths))
                       (/= depth (car (last depths))))
                (push (list :line current-line
                           :depth depth
                           :text (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (min (+ (line-beginning-position) 60)
                                      (line-end-position))))
                      depth-info)))
            (forward-line 1)
            (setq current-line (1+ current-line)))
          (list :status "ok"
                :lines (line-number-at-pos (point-max))
                :depth-changes (nreverse depth-info)))))))

(defun claude/find-unmatched-opener (buffer-or-file)
  "Find unmatched opening parens/brackets/braces.
Uses syntax table to identify unclosed delimiters."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (unmatched '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[][(){}]" nil t)
          (let* ((pos (1- (point)))
                 (char (char-after pos))
                 (syntax (char-syntax char)))
            (when (eq syntax ?\()  ;; Opening paren
              (save-excursion
                (goto-char pos)
                (condition-case nil
                    (forward-sexp 1)
                  (error
                   ;; This opener has no matching closer
                   (push (list :position pos
                              :line (line-number-at-pos pos)
                              :column (save-excursion
                                       (goto-char pos)
                                       (current-column))
                              :char (char-to-string char)
                              :context (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))
                         unmatched))))))))
      (if unmatched
          (list :status "error"
                :count (length unmatched)
                :unmatched (nreverse unmatched))
        (list :status "ok"
              :message "All opening delimiters matched")))))

(defun claude/validate-by-indentation (buffer-or-file)
  "Use indentation as a heuristic for structure problems.
Identifies lines with suspicious indentation that might indicate paren issues.
Enhanced version that tracks indentation stack and detects multiple anomaly types."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (suspicious '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((prev-indent 0)
              (indent-stack '(0)))  ; Track expected indentation levels
          (while (not (eobp))
            (let* ((current-indent (current-indentation))
                   (line-num (line-number-at-pos))
                   (line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))
                   (indent-jump (- current-indent prev-indent))
                   (is-blank (string-match-p "^[[:space:]]*$" line-text))
                   (is-comment (string-match-p "^[[:space:]]*;" line-text)))

              ;; Skip blank lines and comments
              (unless (or is-blank is-comment)

                ;; Detection 1: Suspicious indentation jumps (more than 4 spaces)
                (when (> indent-jump 4)
                  (push (list :line line-num
                             :type "suspicious-jump"
                             :from prev-indent
                             :to current-indent
                             :jump indent-jump
                             :text (substring line-text 0 (min 60 (length line-text)))
                             :warning "Large indentation increase - possible missing paren above")
                        suspicious))

                ;; Detection 2: Unexpected large drops (more than 6 spaces)
                (let ((drop (- prev-indent current-indent)))
                  (when (> drop 6)
                    (push (list :line line-num
                               :type "suspicious-drop"
                               :from prev-indent
                               :to current-indent
                               :drop drop
                               :text (substring line-text 0 (min 60 (length line-text)))
                               :warning "Large indentation decrease - possible missing closers above")
                          suspicious)))

                ;; Detection 3: Indentation not matching any known level
                (unless (or (= current-indent 0)
                           (member current-indent indent-stack)
                           (< (length indent-stack) 2))  ; Allow flexibility early on
                  (push (list :line line-num
                             :type "unexpected-level"
                             :indent current-indent
                             :expected indent-stack
                             :text (substring line-text 0 (min 60 (length line-text)))
                             :warning "Indentation doesn't match any open level")
                        suspicious))

                ;; Detection 4: Sudden return to column 0 from deep nesting
                (when (and (zerop current-indent)
                          (> prev-indent 4))
                  (push (list :line line-num
                             :type "sudden-dedent"
                             :from prev-indent
                             :to 0
                             :text (substring line-text 0 (min 60 (length line-text)))
                             :warning "Sudden return to column 0 from deep nesting - possible unclosed forms")
                        suspicious))

                ;; Update tracking state
                (setq prev-indent current-indent)

                ;; Update indent stack
                (cond
                 ;; Going deeper - add new level
                 ((> current-indent (or (car indent-stack) 0))
                  (push current-indent indent-stack))
                 ;; Going back - pop levels we've left
                 ((< current-indent (or (car indent-stack) 0))
                  (while (and indent-stack
                             (< current-indent (car indent-stack)))
                    (pop indent-stack))))))

            (forward-line 1))))

      (if suspicious
          (list :status "warning"
                :count (length suspicious)
                :anomalies (nreverse suspicious))
        (list :status "ok"
              :message "No suspicious indentation patterns")))))

(defun claude/quick-paren-summary (buffer-or-file)
  "Quick summary of paren health - optimized for minimal tokens.
Returns compact diagnostic info."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer)))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((open-count 0)
              (close-count 0)
              (depth-at-end 0))
          ;; Count parens
          (while (re-search-forward "[][(){}]" nil t)
            (let ((char (char-before)))
              (cond
               ((memq char '(?\( ?\[ ?\{)) (setq open-count (1+ open-count)))
               ((memq char '(?\) ?\] ?\})) (setq close-count (1+ close-count))))))
          ;; Check final depth
          (goto-char (point-max))
          (setq depth-at-end (car (syntax-ppss)))

          (list :open-count open-count
                :close-count close-count
                :balance (- open-count close-count)
                :depth-at-end depth-at-end
                :balanced (zerop depth-at-end)
                :likely-issue (cond
                              ((> depth-at-end 0) "missing-closers")
                              ((< depth-at-end 0) "extra-closers")
                              (t nil))))))))

(defun claude/all-diagnostics (buffer-or-file)
  "Run ALL diagnostic methods and return combined results.
Single function for comprehensive analysis."
  (list :check-parens (claude/diagnose-parens buffer-or-file)
        :scan-parse (claude/scan-parens buffer-or-file)
        :depth-analysis (claude/show-paren-depth buffer-or-file)
        :unmatched-openers (claude/find-unmatched-opener buffer-or-file)
        :indentation-hints (claude/validate-by-indentation buffer-or-file)
        :quick-summary (claude/quick-paren-summary buffer-or-file)))

(defun claude/minimal-diagnostic (buffer-or-file)
  "Minimal diagnostic for token efficiency.
Returns only essential error info."
  (let ((result (claude/diagnose-parens buffer-or-file)))
    (if (string= (plist-get result :status) "ok")
        (list :ok t)
      (list :ok nil
            :line (plist-get result :line)
            :col (plist-get result :column)
            :msg (plist-get result :message)
            :text (plist-get result :line-text)))))

(defun claude/scan-indentation-pattern (buffer-or-file)
  "Scan indentation patterns to detect likely paren issues.
Mimics human visual scanning technique - looking for anomalies in indentation flow.
This is the programmatic version of scanning with rainbow delimiters."
  (claude/validate-by-indentation buffer-or-file))

(defun claude/indentation-map (buffer-or-file)
  "Create efficient structure map using syntax-ppss (NO visual bars to count!).
Shows depth as direct numbers - parseable and verifiable.
This is the NEW efficient version - no counting needed!"
  (claude/efficient-diagnosis buffer-or-file))

(defun claude/combined-diagnosis (buffer-or-file)
  "Combine indentation scan with syntax check for best results.
Two-stage approach:
  1. Indentation heuristics (fast, finds likely problem areas)
  2. Syntax validation (accurate, confirms actual errors)
This gives both speed and accuracy with minimal token usage."
  (let ((indent-result (claude/scan-indentation-pattern buffer-or-file))
        (syntax-result (claude/minimal-diagnostic buffer-or-file)))
    (list :status "combined"
          :indentation indent-result
          :syntax syntax-result
          :recommendation
          (cond
           ;; Both find issues - high confidence
           ((and (not (plist-get syntax-result :ok))
                (string= (plist-get indent-result :status) "warning"))
            (format "High confidence: Syntax error at line %d, indentation anomalies detected"
                   (plist-get syntax-result :line)))

           ;; Syntax error but clean indentation - might be quote/string issue
           ((not (plist-get syntax-result :ok))
            (format "Syntax error at line %d - check quotes/strings (indentation looks ok)"
                   (plist-get syntax-result :line)))

           ;; Indentation suspicious but syntax ok - possible style issue or false positive
           ((string= (plist-get indent-result :status) "warning")
            "Indentation anomalies detected but syntax is valid - may be style choice")

           ;; All good
           (t "No issues detected")))))

(defun claude/structure-checkpoints (buffer-or-file &optional interval)
  "Get structural depth checkpoints using syntax-ppss.
INTERVAL determines sampling frequency (default: every 10 lines).
Returns parseable depth values - no counting needed!"
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (checkpoints '())
        (interval (or interval 10)))

    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((total-lines (line-number-at-pos (point-max))))
          (while (not (eobp))
            (let ((line-num (line-number-at-pos)))
              (when (or (= line-num 1)
                       (zerop (% line-num interval))
                       (= line-num total-lines))
                (let* ((state (syntax-ppss))
                       (depth (nth 0 state))
                       (innermost (nth 1 state))
                       (stack (nth 9 state)))
                  (push (list :line line-num
                             :depth depth
                             :innermost innermost
                             :stack-size (length stack))
                        checkpoints))))
            (forward-line 1))))

      ;; Get final depth
      (goto-char (point-max))
      (let* ((final-state (syntax-ppss))
             (final-depth (nth 0 final-state)))

        (list :status (if (zerop final-depth) "balanced" "unbalanced")
              :total-lines (line-number-at-pos (point-max))
              :checkpoints (nreverse checkpoints)
              :final-depth final-depth
              :balanced (zerop final-depth))))))

(defun claude/structure-deltas (buffer-or-file)
  "Report only depth changes - extremely compact.
Shows when and how depth changes, ignoring constant regions."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (deltas '())
        (prev-depth 0))

    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-num (line-number-at-pos))
                 (state (syntax-ppss))
                 (depth (nth 0 state))
                 (delta (- depth prev-depth)))

            (when (not (zerop delta))
              (push (list :line line-num
                         :depth depth
                         :delta delta
                         :change (cond
                                 ((> delta 0) (format "+%d" delta))
                                 (t (format "%d" delta))))
                    deltas)
              (setq prev-depth depth)))

          (forward-line 1))))

    (list :status "ok"
          :change-count (length deltas)
          :deltas (nreverse deltas))))

(defun claude/stack-at-error (buffer-or-file &optional position)
  "Get exact paren stack at error POSITION (or current error).
Returns positions of all unclosed opening parens - directly verifiable!"
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer)))))

    (with-current-buffer buf
      (save-excursion
        ;; If no position given, find error position
        (unless position
          (goto-char (point-min))
          (condition-case err
              (check-parens)
            (error
             (setq position (point)))))

        (when position
          (goto-char position)
          (let* ((state (syntax-ppss))
                 (depth (nth 0 state))
                 (stack (nth 9 state))
                 (stack-info '()))

            ;; Build detailed stack info
            (dolist (opener-pos stack)
              (save-excursion
                (goto-char opener-pos)
                (push (list :position opener-pos
                           :line (line-number-at-pos)
                           :column (current-column)
                           :char (char-to-string (char-after))
                           :context (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (min (+ (line-beginning-position) 60)
                                         (line-end-position))))
                      stack-info)))

            (list :status (if (zerop depth) "ok" "error")
                  :position position
                  :line (line-number-at-pos position)
                  :depth depth
                  :unclosed-count (length stack)
                  :stack (nreverse stack-info))))))))

(defun claude/efficient-diagnosis (buffer-or-file)
  "Efficient structure-based diagnosis using syntax-ppss.
This replaces visual bars with parseable numerical data.
Returns: checkpoints + deltas + error details in ~100-150 tokens."
  (let ((buf (cond
              ((bufferp buffer-or-file) buffer-or-file)
              ((stringp buffer-or-file) (find-file-noselect buffer-or-file))
              (t (current-buffer))))
        (syntax-result (claude/minimal-diagnostic buffer-or-file)))

    (with-current-buffer buf
      (let ((checkpoints (claude/structure-checkpoints buf 10))
            (deltas (claude/structure-deltas buf))
            (has-error (not (plist-get syntax-result :ok))))

        (list :status (if has-error "error" "ok")
              :file (buffer-name buf)
              :lines (line-number-at-pos (point-max))
              :checkpoints (plist-get checkpoints :checkpoints)
              :final-depth (plist-get checkpoints :final-depth)
              :balanced (plist-get checkpoints :balanced)
              :deltas (plist-get deltas :deltas)
              :syntax-check syntax-result
              :stack (when has-error
                      (claude/stack-at-error buf))
              :recommendation
              (cond
               ((not has-error)
                "Structure balanced - no errors detected")
               ((plist-get (claude/stack-at-error buf) :unclosed-count)
                (format "%d unclosed paren(s) - check lines in stack"
                       (plist-get (claude/stack-at-error buf) :unclosed-count)))
               (t "Syntax error detected - see details"))))))))

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
                          mismatches))))))))

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

(provide 'claude-paren-diagnostics)
;;; claude-paren-diagnostics.el ends here
