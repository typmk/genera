;;; eglot-diagnose.el --- Use eglot + clojure-lsp for paren diagnostics

(require 'eglot)
(require 'jsonrpc)

(defun claude/eglot-diagnose-sync (file)
  "Use eglot + clojure-lsp to get diagnostics for FILE synchronously."
  (with-temp-buffer
    (insert-file-contents file)
    (setq buffer-file-name file)
    (clojure-mode)

    (princ "Starting eglot...\n")

    ;; Start eglot
    (eglot-ensure)

    ;; Wait for server to be ready
    (princ "Waiting for LSP server...\n")
    (sleep-for 3)

    ;; Request diagnostics
    (princ "Getting diagnostics...\n")
    (let ((diagnostics (flymake-diagnostics)))
      (if diagnostics
          (progn
            (princ (format "\n=== Found %d diagnostics ===\n\n" (length diagnostics)))
            (dolist (diag diagnostics)
              (let* ((beg (flymake-diagnostic-beg diag))
                     (end (flymake-diagnostic-end diag))
                     (text (flymake-diagnostic-text diag))
                     (type (flymake-diagnostic-type diag))
                     (line (line-number-at-pos beg))
                     (col (save-excursion (goto-char beg) (current-column))))
                (princ (format "Line %d, Col %d [%s]: %s\n"
                               line col type text)))))
        (princ "No diagnostics found (file may be valid or LSP not responding)\n")))

    ;; Try to get raw LSP diagnostics too
    (when (eglot-current-server)
      (princ "\n=== Raw LSP Server Info ===\n")
      (princ (format "Server: %s\n" (eglot-current-server)))
      (princ (format "Capabilities: %s\n" (eglot--server-capable :diagnosticProvider))))))

;; Simpler non-interactive version
(defun claude/clojure-lsp-direct (file)
  "Call clojure-lsp directly on FILE (no emacs integration)."
  (princ "\n=== Direct clojure-lsp call ===\n\n")
  (let* ((default-directory (file-name-directory file))
         (cmd (format "clojure-lsp diagnostics-range --filename \"%s\" --row 1 --col 1 --end-row 9999 --end-col 9999"
                      file))
         (output (shell-command-to-string cmd)))
    (princ output)))

;; Main entry point
(let ((file (car command-line-args-left)))
  (when file
    ;; Try direct LSP call first (simpler)
    (claude/clojure-lsp-direct file)

    ;; Then try eglot integration
    ;; (claude/eglot-diagnose-sync file)  ; Commented out - harder in batch mode
    ))
