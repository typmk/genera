;;; parinfer-diagnose.el --- Use parinfer for structural diagnostics

(add-to-list 'load-path "C:/Users/Apollo/em/parinfer-elisp")
(require 'parinferlib)

(defun claude/parinfer-diagnose (file)
  "Use Parinfer to diagnose structural issues in FILE.

Parinfer is uniquely good at finding missing opening parens
by inferring structure from indentation."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((text (buffer-string))
           ;; Try indent mode (infers parens from indentation)
           (indent-result (parinferlib-indent-mode text nil))
           ;; Try paren mode (infers indentation from parens)
           (paren-result (parinferlib-paren-mode text nil))

           (indent-success (plist-get indent-result :success))
           (paren-success (plist-get paren-result :success))
           (indent-error (plist-get indent-result :error))
           (paren-error (plist-get paren-result :error))
           (indent-changes (plist-get indent-result :changed-lines))
           (paren-changes (plist-get paren-result :changed-lines)))

      (princ "=== PARINFER DIAGNOSTIC RESULTS ===\n\n")

      ;; Report indent mode results
      (princ "--- Indent Mode (infer parens from indentation) ---\n")
      (if indent-success
          (progn
            (princ (format "✓ Success: File structure is valid\n"))
            (when indent-changes
              (princ (format "  Changes suggested: %d lines\n" (length indent-changes)))
              (princ "  (These are parens that should be added/removed)\n\n")
              (dotimes (i (min 10 (length indent-changes)))
                (let* ((change (aref indent-changes i))
                       (line-no (plist-get change :line-no))
                       (line-text (plist-get change :line)))
                  (princ (format "  Line %d: %s\n" (1+ line-no) line-text))))))
        (progn
          (princ "✗ Error detected:\n")
          (let ((err-name (plist-get indent-error :name))
                (err-msg (plist-get indent-error :message))
                (err-line (plist-get indent-error :line-no))
                (err-col (plist-get indent-error :x)))
            (princ (format "  %s at line %d, col %d\n" err-name (1+ err-line) err-col))
            (princ (format "  Message: %s\n" err-msg)))))

      (princ "\n--- Paren Mode (infer indentation from parens) ---\n")
      (if paren-success
          (progn
            (princ (format "✓ Success: Parens are balanced\n"))
            (when paren-changes
              (princ (format "  Indentation changes suggested: %d lines\n" (length paren-changes)))))
        (progn
          (princ "✗ Error detected:\n")
          (let ((err-name (plist-get paren-error :name))
                (err-msg (plist-get paren-error :message))
                (err-line (plist-get paren-error :line-no))
                (err-col (plist-get paren-error :x)))
            (princ (format "  %s at line %d, col %d\n" err-name (1+ err-line) err-col))
            (princ (format "  Message: %s\n" err-msg)))))

      (princ "\n--- Summary ---\n")
      (cond
       ((and indent-success paren-success)
        (princ "✓ File is structurally valid!\n"))
       ((not paren-success)
        (princ "✗ Parenthesis imbalance detected\n")
        (princ "  Fix the paren errors first\n"))
       ((not indent-success)
        (princ "✗ Indentation suggests missing/extra parens\n")
        (princ "  Parinfer can infer where they should be!\n")))

      ;; Return structured result
      (list :indent-success indent-success
            :paren-success paren-success
            :indent-changes (when indent-changes (length indent-changes))
            :paren-changes (when paren-changes (length paren-changes))
            :indent-error indent-error
            :paren-error paren-error))))

;; Main entry point
(let ((file (car command-line-args-left)))
  (when file
    (claude/parinfer-diagnose file)))
