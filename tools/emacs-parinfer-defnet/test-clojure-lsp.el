;;; test-clojure-lsp.el --- Test clojure-lsp diagnostics for paren errors

(defun claude/lsp-diagnose (file)
  "Use clojure-lsp to diagnose structural errors in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file)

    ;; Enable Clojure mode
    (when (fboundp 'clojure-mode)
      (clojure-mode))

    ;; Start LSP if available
    (when (fboundp 'lsp-deferred)
      (lsp-deferred)
      (sleep-for 2))  ; Give LSP time to analyze

    ;; Get diagnostics from LSP
    (when (and (boundp 'lsp-diagnostics)
               (fboundp 'lsp-diagnostics))
      (let ((diags (lsp-diagnostics)))
        (prin1 (list :status (if diags 'found-errors 'no-errors)
                     :diagnostics diags
                     :count (length diags)))))))

;; Test if clojure-lsp is available
(defun claude/check-lsp-available ()
  "Check if clojure-lsp is installed and configured."
  (let ((lsp-available (executable-find "clojure-lsp"))
        (eglot-available (fboundp 'eglot))
        (lsp-mode-available (fboundp 'lsp-deferred)))
    (prin1 (list :clojure-lsp-binary lsp-available
                 :eglot-mode eglot-available
                 :lsp-mode lsp-mode-available))))

;; Main entry point
(let ((action (car command-line-args-left))
      (file (cadr command-line-args-left)))
  (cond
   ((string= action "check")
    (claude/check-lsp-available))
   ((string= action "diagnose")
    (when file
      (claude/lsp-diagnose file)))
   (t
    (princ "Usage: emacs --batch --load test-clojure-lsp.el check|diagnose [file]\n"))))
