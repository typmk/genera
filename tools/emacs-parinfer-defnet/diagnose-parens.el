;;; diagnose-parens.el --- Batch mode paren diagnostics

(defun find-first-paren-error (file)
  "Find the first paren error in FILE and return details."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case err
        (progn
          ;; Scan through file
          (let ((depth 0)
                (errors '()))
            (while (not (eobp))
              (let ((char (char-after)))
                (cond
                 ((eq char ?\()
                  (setq depth (1+ depth)))
                 ((eq char ?\))
                  (if (= depth 0)
                      ;; Unmatched closer
                      (push (list :type 'unmatched-close
                                  :line (line-number-at-pos)
                                  :column (current-column)
                                  :char ")")
                            errors)
                    (setq depth (1- depth))))))
              (forward-char 1))
            ;; Check for unmatched openers
            (when (> depth 0)
              (push (list :type 'unmatched-open
                          :count depth
                          :message (format "%d unmatched opening paren(s)" depth))
                    errors))
            (if errors
                (prin1 (list :status 'error
                             :errors (reverse errors)
                             :total (length errors)))
              (prin1 '(:status ok :message "No errors")))))
      (error
       (prin1 (list :status 'error
                    :message (error-message-string err)
                    :at-point (point)
                    :line (line-number-at-pos)
                    :column (current-column)))))))

(defun count-all-parens (file)
  "Count opening and closing parens in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((open-count 0)
          (close-count 0))
      (while (not (eobp))
        (let ((char (char-after)))
          (cond
           ((eq char ?\() (setq open-count (1+ open-count)))
           ((eq char ?\)) (setq close-count (1+ close-count)))))
        (forward-char 1))
      (prin1 (list :open open-count
                   :close close-count
                   :diff (- open-count close-count))))))

;; Main entry point
(let ((file (car command-line-args-left)))
  (when file
    (princ "=== Paren Count ===\n")
    (count-all-parens file)
    (princ "\n\n=== Error Analysis ===\n")
    (find-first-paren-error file)))
