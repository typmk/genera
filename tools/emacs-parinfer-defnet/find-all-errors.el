;;; find-all-errors.el --- Find all paren mismatches with details

(defun find-all-paren-errors (file)
  "Find ALL paren errors in FILE with precise locations."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((stack '())  ; Stack of (line col) for opens
          (errors '())
          (line 1)
          (col 0))

      ;; Scan character by character
      (while (not (eobp))
        (let ((char (char-after)))
          (cond
           ;; Opening paren - push location
           ((eq char ?\()
            (push (list :line line :col col) stack))

           ;; Closing paren - pop or record error
           ((eq char ?\))
            (if stack
                (pop stack)  ; Matched - remove from stack
              ;; Unmatched closer
              (push (list :type 'unmatched-close
                          :line line
                          :col col
                          :char ")"
                          :context (buffer-substring (max 1 (- (point) 20))
                                                    (min (point-max) (+ (point) 20))))
                    errors)))

           ;; Newline - increment line counter
           ((eq char ?\n)
            (setq line (1+ line))
            (setq col -1)))  ; Will be 0 after increment below

          (setq col (1+ col))
          (forward-char 1)))

      ;; Any remaining stack items are unmatched opens
      (dolist (open stack)
        (push (list :type 'unmatched-open
                    :line (plist-get open :line)
                    :col (plist-get open :col)
                    :char "(")
              errors))

      ;; Count error types
      (let ((close-count 0)
            (open-count 0))
        (dolist (e errors)
          (if (eq (plist-get e :type) 'unmatched-close)
              (setq close-count (1+ close-count))
            (setq open-count (1+ open-count))))
        (prin1 (list :status (if errors 'error 'ok)
                     :errors (reverse errors)
                     :total (length errors)
                     :unmatched-closes close-count
                     :unmatched-opens open-count))))))

;; Main entry point
(let ((file (car command-line-args-left)))
  (when file
    (find-all-paren-errors file)))
