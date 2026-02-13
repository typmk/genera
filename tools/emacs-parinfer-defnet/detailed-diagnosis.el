;;; detailed-diagnosis.el --- Detailed paren error diagnosis with context

(defun diagnose-paren-errors-detailed (file)
  "Provide detailed diagnosis of paren errors with surrounding context."
  (with-temp-buffer
    (insert-file-contents file)
    (lisp-mode)  ; Enable Lisp mode for syntax-ppss
    (goto-char (point-min))

    (princ "=== DETAILED PAREN DIAGNOSIS ===\n\n")

    ;; Use syntax-ppss to find errors
    (let ((errors '())
          (line-num 1))

      (while (not (eobp))
        (let* ((state (syntax-ppss))
               (depth (car state)))  ; Current paren depth

          ;; If we see a closing paren but depth is 0, it's unmatched
          (when (and (eq (char-after) ?\)) (= depth 0))
            (let ((context-start (save-excursion
                                   (forward-line -2)
                                   (point)))
                  (context-end (save-excursion
                                 (forward-line 2)
                                 (point))))
              (push (list :type 'unmatched-close
                          :line line-num
                          :column (current-column)
                          :context (buffer-substring context-start context-end))
                    errors)))

          (when (eq (char-after) ?\n)
            (setq line-num (1+ line-num)))
          (forward-char 1)))

      ;; Check final depth for unmatched opens
      (let ((final-depth (car (syntax-ppss))))
        (when (> final-depth 0)
          (princ (format "CRITICAL: %d unmatched opening paren(s) remain\n\n" final-depth))))

      ;; Report each error with context
      (when errors
        (princ (format "Found %d unmatched closing parens:\n\n" (length errors)))
        (dolist (err (reverse errors))
          (princ (format "Line %d, Column %d: Unmatched ')'\n"
                         (plist-get err :line)
                         (plist-get err :column)))
          (princ "Context:\n")
          (princ (plist-get err :context))
          (princ "\n---\n\n")))

      ;; Now scan for likely locations where closing parens are needed
      (goto-char (point-min))
      (princ "\n=== LIKELY FIX LOCATIONS ===\n\n")

      (let ((fix-candidates '())
            (line-num 1))
        (while (not (eobp))
          (let* ((line-start (point))
                 (line-end (line-end-position))
                 (line-text (buffer-substring line-start line-end))
                 (state (syntax-ppss line-end))
                 (depth-at-eol (car state)))

            ;; If depth increases during this line and isn't closed
            (when (and (> depth-at-eol 0)
                       (or (string-match-p "(atom \\[\\]$" line-text)
                           (string-match-p "(atom {}$" line-text)
                           (string-match-p "(reset! [^ ]+ #{}$" line-text)
                           (string-match-p "(reset! [^ ]+ \\[\\]$" line-text)
                           (string-match-p "(reset! [^ ]+ {}$" line-text)
                           (string-match-p ":\\w+$" line-text)
                           (string-match-p "false$" line-text)
                           (string-match-p "true$" line-text)))

              (push (list :line line-num
                          :depth depth-at-eol
                          :text line-text)
                    fix-candidates))

            (setq line-num (1+ line-num))
            (forward-line 1)))

        ;; Report candidates
        (when fix-candidates
          (princ (format "Found %d lines that likely need closing parens:\n\n" (length fix-candidates)))
          (dolist (candidate (reverse fix-candidates))
            (princ (format "Line %d (depth %d): %s\n"
                           (plist-get candidate :line)
                           (plist-get candidate :depth)
                           (plist-get candidate :text)))))

        (unless fix-candidates
          (princ "No obvious fix candidates found via heuristics.\n"))))))

;; Main entry point
(let ((file (car command-line-args-left)))
  (when file
    (diagnose-paren-errors-detailed file)))
