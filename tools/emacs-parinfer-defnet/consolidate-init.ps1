# Script to consolidate and properly format init.el
$initFile = "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el"

$newContent = @'
;;; init.el --- Emacs configuration with Claude Code integration

;;; Commentary:
;; Basic Emacs configuration with claude-code.el setup using straight.el

;;; Code:

;; Bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install inheritenv (required dependency)
(use-package inheritenv)

;; Install eat terminal emulator (recommended for Windows)
(use-package eat
  :straight (:host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el"))))

;; Install mcp.el - Model Context Protocol integration
(use-package mcp
  :straight (:host github
             :repo "lizqwerscott/mcp.el"
             :depth 1)
  :config
  ;; Configure MCP servers (example configuration)
  ;; Uncomment and customize the servers you want to use
  ;; (setq mcp-hub-servers
  ;;       '(("filesystem" . (:command "npx"
  ;;                          :args ("-y" "@modelcontextprotocol/server-filesystem")
  ;;                          :roots ("C:/Users/Apollo/")))
  ;;         ("fetch" . (:command "uvx"
  ;;                     :args ("mcp-server-fetch")))))

  ;; Optional: Auto-start all configured servers on Emacs startup
  ;; (add-hook 'after-init-hook #'mcp-hub-start-all-server)
  )

;; Install claude-code.el from GitHub
(use-package claude-code
  :straight (:host github
             :repo "stevemolitor/claude-code.el"
             :depth 1
             :files (:defaults (:exclude "*.png" "*.gif" "*.jpg")))
  :after (inheritenv eat)
  :custom
  ;; Use eat as the terminal backend (better Windows support)
  (claude-code-terminal-backend 'eat)
  ;; Set the correct command name for your system
  (claude-code-program "claude")
  :bind-keymap
  ;; Bind all claude-code commands to C-c c prefix
  ("C-c c" . claude-code-command-map)
  :bind
  ;; Optional: Set up repeat map for mode cycling
  (:repeat-map my-claude-code-map
   ("M" . claude-code-cycle-mode))
  :config
  ;; Enable claude-code-mode globally
  (claude-code-mode 1))

;; Optional: Configure repeat-mode for easier mode cycling
(use-package repeat
  :config
  (repeat-mode 1))

;;; Language-Specific Development Configurations

;; Clojure Development Configuration
(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.edn\\'")
  :config
  ;; Enable prettify symbols for lambda, etc.
  (setq clojure-enable-fancify-symbols t))

(use-package cider
  :after clojure-mode
  :config
  ;; Disable the help banner in REPL
  (setq cider-repl-display-help-banner nil)
  ;; Enable auto-completion in REPL
  (setq cider-repl-use-pretty-printing t)
  ;; Show documentation on mouse hover
  (setq cider-auto-select-error-buffer t)
  ;; Eldoc for function signatures
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;; Enable smartparens in Clojure modes
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :after (flycheck clojure-mode))

;; Common Lisp Development Configuration (SLIME + SBCL)
(use-package slime
  :config
  ;; Set the path to SBCL
  (setq inferior-lisp-program "sbcl")
  ;; Set up SLIME contribs for enhanced features
  ;; slime-fancy includes: slime-repl, slime-autodoc, slime-c-p-c, slime-editing-commands,
  ;;                       slime-fancy-inspector, slime-fancy-trace, slime-fuzzy,
  ;;                       slime-presentations, slime-scratch, slime-references, slime-package-fu
  (setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf))
  ;; Optional: Enable fuzzy completion
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

;;; Additional Features and Helper Functions

;; Start Emacs server for remote control via emacsclient
(require 'server)
(unless (server-running-p)
  (server-start)
  (message "Emacs server started - you can now use emacsclient"))

;; Load Claude Code helper functions for programmatic access
(let ((claude-helpers (expand-file-name "~/em/claude-helpers.el")))
  (when (file-exists-p claude-helpers)
    (load-file claude-helpers)
    (message "Claude helper functions loaded")))

;; Load Claude paren diagnostics (optimized for LLM token efficiency)
(let ((claude-diagnostics (expand-file-name "~/em/claude-paren-diagnostics.el")))
  (when (file-exists-p claude-diagnostics)
    (load-file claude-diagnostics)
    (message "Claude paren diagnostics loaded")))

(provide 'init)
;;; init.el ends here
'@

Set-Content -Path $initFile -Value $newContent
Write-Host "Successfully consolidated and formatted init.el"
