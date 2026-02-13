;;; defnet.el --- Defnet IDE integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Heisenberg Technologies
;; Author: Apollo Nicolson <apollo.nicolson@heisenbergtech.com.au>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (request "0.3.3") (transient "0.5.0"))
;; Keywords: tools, languages, clojure
;; URL: https://github.com/hbtweb/defnet

;;; Commentary:

;; Defnet IDE integration for Emacs.  Provides code intelligence,
;; call graph analysis, trust scoring, and refactoring capabilities
;; through the Defnet MCP server.
;;
;; Features:
;; - Find callers/callees of functions
;; - Search code semantically
;; - Trust score visualization
;; - Impact analysis before refactoring
;; - Clerk notebook integration for visualizations
;;
;; Usage:
;;   (require 'defnet)
;;   (defnet-start)  ; Start the defnet server
;;   M-x defnet-find-callers  ; Find who calls current function
;;   M-x defnet-dashboard     ; Open Clerk dashboard in browser

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

;;; Customization

(defgroup defnet nil
  "Defnet IDE integration."
  :group 'tools
  :prefix "defnet-")

(defcustom defnet-server-host "localhost"
  "Hostname for Defnet server."
  :type 'string
  :group 'defnet)

(defcustom defnet-server-port 9876
  "Port for Defnet HTTP server."
  :type 'integer
  :group 'defnet)

(defcustom defnet-clerk-port 7777
  "Port for Clerk notebook server."
  :type 'integer
  :group 'defnet)

(defcustom defnet-nrepl-port 7888
  "Port for nREPL server."
  :type 'integer
  :group 'defnet)

(defcustom defnet-project-root nil
  "Root directory of the project to analyze.
If nil, uses `project-root' or `default-directory'."
  :type '(choice (const nil) directory)
  :group 'defnet)

(defcustom defnet-auto-index t
  "If non-nil, automatically index project on server start."
  :type 'boolean
  :group 'defnet)

;;; Internal Variables

(defvar defnet--server-process nil
  "Process object for the Defnet server.")

(defvar defnet--request-id 0
  "Counter for JSON-RPC request IDs.")

(defvar defnet--connected nil
  "Non-nil if connected to Defnet server.")

;;; JSON-RPC Client

(defun defnet--next-id ()
  "Generate next request ID."
  (cl-incf defnet--request-id))

(defun defnet--server-url (&optional path)
  "Return the server URL, optionally with PATH appended."
  (format "http://%s:%d%s"
          defnet-server-host
          defnet-server-port
          (or path "/rpc")))

(defun defnet--make-request (method params)
  "Create a JSON-RPC request for METHOD with PARAMS."
  (json-encode
   `((jsonrpc . "2.0")
     (id . ,(defnet--next-id))
     (method . ,method)
     (params . ,params))))

(defun defnet--call (method params &optional callback)
  "Call METHOD with PARAMS on the Defnet server.
If CALLBACK is provided, call asynchronously."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (defnet--make-request method params)))
    (if callback
        (url-retrieve (defnet--server-url)
                      (lambda (status)
                        (goto-char (point-min))
                        (re-search-forward "\n\n" nil t)
                        (let ((response (json-read)))
                          (funcall callback response))))
      ;; Synchronous call
      (with-current-buffer
          (url-retrieve-synchronously (defnet--server-url))
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (json-read)))))

(defun defnet--call-tool (tool-name args &optional callback)
  "Call a Defnet TOOL-NAME with ARGS.
Wraps the MCP tools/call method."
  (defnet--call "tools/call"
                `((name . ,tool-name)
                  (arguments . ,args))
                callback))

;;; Health Check

(defun defnet-ping ()
  "Check if Defnet server is running."
  (interactive)
  (condition-case err
      (let ((response (defnet--call "initialize"
                                    `((protocolVersion . "2024-11-05")
                                      (capabilities . ())
                                      (clientInfo . ((name . "defnet.el")
                                                    (version . "0.1.0")))))))
        (setq defnet--connected t)
        (message "Defnet: Connected to %s" (defnet--server-url)))
    (error
     (setq defnet--connected nil)
     (message "Defnet: Not connected - %s" (error-message-string err)))))

(defun defnet-health ()
  "Get Defnet server health status."
  (interactive)
  (condition-case err
      (with-current-buffer
          (url-retrieve-synchronously
           (defnet--server-url "/health"))
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (let ((health (json-read)))
          (message "Defnet: %s (indexed: %s functions, %s namespaces)"
                   (alist-get 'status health)
                   (alist-get 'functions (alist-get 'indexed health))
                   (alist-get 'namespaces (alist-get 'indexed health)))))
    (error
     (message "Defnet: Server not responding - %s" (error-message-string err)))))

;;; Server Management

(defun defnet--project-root ()
  "Get the project root directory."
  (or defnet-project-root
      (when (fboundp 'project-root)
        (when-let ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun defnet-start ()
  "Start the Defnet server."
  (interactive)
  (if (process-live-p defnet--server-process)
      (message "Defnet: Server already running")
    (let ((default-directory (defnet--project-root)))
      (setq defnet--server-process
            (start-process "defnet" "*defnet*"
                           "clj" "-M:http"))
      (message "Defnet: Starting server on port %d..." defnet-server-port)
      ;; Wait a bit for startup
      (run-with-timer 3 nil #'defnet-ping))))

(defun defnet-stop ()
  "Stop the Defnet server."
  (interactive)
  (when (process-live-p defnet--server-process)
    (kill-process defnet--server-process)
    (setq defnet--server-process nil)
    (setq defnet--connected nil)
    (message "Defnet: Server stopped")))

(defun defnet-restart ()
  "Restart the Defnet server."
  (interactive)
  (defnet-stop)
  (run-with-timer 1 nil #'defnet-start))

;;; Code Intelligence Commands

(defun defnet--function-at-point ()
  "Get the fully qualified function name at point.
Works with Clojure, Emacs Lisp, and other Lisps."
  (let ((sym (thing-at-point 'symbol t)))
    (when sym
      ;; For Clojure, try to get namespace
      (if (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojurec-mode)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "(ns \\([^ \n)]+\\)" nil t)
                (concat (match-string 1) "/" sym)
              sym))
        sym))))

(defun defnet-find-callers (&optional function-name)
  "Find all callers of FUNCTION-NAME.
If FUNCTION-NAME is nil, use function at point."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "find-callers"
                                      `((function-name . ,fn)))))
    (defnet--display-results "Callers" fn response)))

(defun defnet-called-by (&optional function-name)
  "Find all functions called by FUNCTION-NAME.
If FUNCTION-NAME is nil, use function at point."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "called-by"
                                      `((function-name . ,fn)))))
    (defnet--display-results "Called by" fn response)))

(defun defnet-locate-function (&optional function-name)
  "Locate the definition of FUNCTION-NAME."
  (interactive)
  (let* ((fn (or function-name
                 (read-string "Function: " (defnet--function-at-point))))
         (response (defnet--call-tool "locate-function"
                                      `((function-name . ,fn))))
         (content (alist-get 'content (aref (alist-get 'result response) 0))))
    ;; Parse the result and jump to location
    (when-let* ((text (alist-get 'text content))
                (path (and (string-match "file: \\([^\n]+\\)" text)
                          (match-string 1 text)))
                (line (and (string-match "line: \\([0-9]+\\)" text)
                          (string-to-number (match-string 1 text)))))
      (find-file path)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun defnet-search-code (query)
  "Search code for QUERY."
  (interactive "sSearch: ")
  (let ((response (defnet--call-tool "search-code"
                                     `((query . ,query)))))
    (defnet--display-results "Search" query response)))

(defun defnet-explain-function (&optional function-name)
  "Get AI-powered explanation of FUNCTION-NAME."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "explain-function"
                                      `((function-name . ,fn)))))
    (defnet--display-markdown "Explanation" fn response)))

(defun defnet-find-usages (&optional function-name)
  "Find all usages of FUNCTION-NAME across the codebase."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "find-usages"
                                      `((function-name . ,fn)))))
    (defnet--display-results "Usages" fn response)))

(defun defnet-get-context (&optional function-name)
  "Get full context for FUNCTION-NAME including callers, callees, tests."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "get-context"
                                      `((function-name . ,fn)))))
    (defnet--display-markdown "Context" fn response)))

(defun defnet-impact-analysis (&optional function-name)
  "Analyze the impact of changing FUNCTION-NAME."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "impact-analysis"
                                      `((function-name . ,fn)))))
    (defnet--display-markdown "Impact Analysis" fn response)))

(defun defnet-suggest-tests (&optional function-name)
  "Get AI-suggested tests for FUNCTION-NAME."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point)))
         (response (defnet--call-tool "suggest-tests"
                                      `((function-name . ,fn)))))
    (defnet--display-markdown "Suggested Tests" fn response)))

(defun defnet-get-stats ()
  "Get codebase statistics."
  (interactive)
  (let ((response (defnet--call-tool "get-stats" '())))
    (defnet--display-markdown "Codebase Stats" "project" response)))

(defun defnet-analyze-structure (&optional file-path)
  "Analyze bracket structure of FILE-PATH."
  (interactive)
  (let* ((path (or file-path (buffer-file-name)))
         (response (defnet--call-tool "analyze-structure"
                                      `((file-path . ,path)))))
    (defnet--display-results "Structure" path response)))

;;; Result Display

(defun defnet--display-results (title query response)
  "Display RESPONSE in a results buffer with TITLE and QUERY."
  (let ((buf (get-buffer-create "*defnet-results*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "# %s: %s\n\n" title query))
      (let* ((result (alist-get 'result response))
             (content (when result
                        (alist-get 'content (aref result 0)))))
        (if content
            (insert (alist-get 'text content))
          (insert "No results found.")))
      (goto-char (point-min))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (read-only-mode 1))
    (display-buffer buf)))

(defun defnet--display-markdown (title query response)
  "Display RESPONSE as markdown in a buffer with TITLE and QUERY."
  (defnet--display-results title query response))

;;; Clerk Integration

(defun defnet-clerk-start ()
  "Start Clerk notebook server."
  (interactive)
  (let ((default-directory (defnet--project-root)))
    (start-process "clerk" "*clerk*"
                   "clj" "-X" "nextjournal.clerk/serve!"
                   ":port" (number-to-string defnet-clerk-port)
                   ":browse" "true")
    (message "Clerk: Starting on port %d..." defnet-clerk-port)))

(defun defnet-clerk-show ()
  "Show current buffer in Clerk."
  (interactive)
  (when (buffer-file-name)
    (let ((file (buffer-file-name)))
      ;; Send to nREPL to evaluate
      (when (fboundp 'cider-nrepl-sync-request:eval)
        (cider-nrepl-sync-request:eval
         (format "(nextjournal.clerk/show! \"%s\")" file))))))

(defun defnet-dashboard ()
  "Open Defnet dashboard in Clerk."
  (interactive)
  (browse-url (format "http://localhost:%d" defnet-clerk-port)))

(defun defnet-call-graph (&optional function-name)
  "Show call graph for FUNCTION-NAME in Clerk."
  (interactive)
  (let* ((fn (or function-name (defnet--function-at-point))))
    ;; This would eval Clojure code to show the call graph in Clerk
    (when (fboundp 'cider-nrepl-sync-request:eval)
      (cider-nrepl-sync-request:eval
       (format "(defnet.clerk.views/show-call-graph \"%s\")" fn)))
    (defnet-dashboard)))

(defun defnet-trust-heatmap (&optional namespace)
  "Show trust score heatmap for NAMESPACE in Clerk."
  (interactive "sNamespace (blank for all): ")
  (when (fboundp 'cider-nrepl-sync-request:eval)
    (cider-nrepl-sync-request:eval
     (if (string-empty-p namespace)
         "(defnet.clerk.views/show-trust-heatmap)"
       (format "(defnet.clerk.views/show-trust-heatmap \"%s\")" namespace))))
  (defnet-dashboard))

;;; Transient Menu (if available)

(when (require 'transient nil t)
  (transient-define-prefix defnet-menu ()
    "Defnet IDE menu."
    ["Server"
     ("s" "Start server" defnet-start)
     ("S" "Stop server" defnet-stop)
     ("r" "Restart server" defnet-restart)
     ("h" "Health check" defnet-health)
     ("p" "Ping" defnet-ping)]
    ["Navigate"
     ("c" "Find callers" defnet-find-callers)
     ("C" "Called by" defnet-called-by)
     ("l" "Locate function" defnet-locate-function)
     ("u" "Find usages" defnet-find-usages)
     ("/" "Search code" defnet-search-code)]
    ["Analyze"
     ("e" "Explain function" defnet-explain-function)
     ("x" "Get context" defnet-get-context)
     ("i" "Impact analysis" defnet-impact-analysis)
     ("t" "Suggest tests" defnet-suggest-tests)
     ("a" "Analyze structure" defnet-analyze-structure)
     ("=" "Codebase stats" defnet-get-stats)]
    ["Clerk"
     ("d" "Dashboard" defnet-dashboard)
     ("g" "Call graph" defnet-call-graph)
     ("T" "Trust heatmap" defnet-trust-heatmap)
     ("v" "Show in Clerk" defnet-clerk-show)]))

;;; Keybindings

(defvar defnet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d c") #'defnet-find-callers)
    (define-key map (kbd "C-c d C") #'defnet-called-by)
    (define-key map (kbd "C-c d l") #'defnet-locate-function)
    (define-key map (kbd "C-c d u") #'defnet-find-usages)
    (define-key map (kbd "C-c d /") #'defnet-search-code)
    (define-key map (kbd "C-c d e") #'defnet-explain-function)
    (define-key map (kbd "C-c d x") #'defnet-get-context)
    (define-key map (kbd "C-c d i") #'defnet-impact-analysis)
    (define-key map (kbd "C-c d t") #'defnet-suggest-tests)
    (define-key map (kbd "C-c d a") #'defnet-analyze-structure)
    (define-key map (kbd "C-c d =") #'defnet-get-stats)
    (define-key map (kbd "C-c d d") #'defnet-dashboard)
    (define-key map (kbd "C-c d g") #'defnet-call-graph)
    (define-key map (kbd "C-c d T") #'defnet-trust-heatmap)
    (when (fboundp 'defnet-menu)
      (define-key map (kbd "C-c d m") #'defnet-menu))
    map)
  "Keymap for `defnet-mode'.")

;;;###autoload
(define-minor-mode defnet-mode
  "Minor mode for Defnet IDE integration.

\\{defnet-mode-map}"
  :lighter " Defnet"
  :keymap defnet-mode-map
  :group 'defnet
  (if defnet-mode
      (defnet-ping)
    nil))

;;;###autoload
(defun defnet-mode-maybe ()
  "Enable `defnet-mode' if in a Defnet project."
  (when (or (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojurec-mode)
            (file-exists-p (expand-file-name ".defnet" (defnet--project-root))))
    (defnet-mode 1)))

;; Auto-enable in Clojure buffers
;;;###autoload
(add-hook 'clojure-mode-hook #'defnet-mode-maybe)
;;;###autoload
(add-hook 'clojurescript-mode-hook #'defnet-mode-maybe)
;;;###autoload
(add-hook 'clojurec-mode-hook #'defnet-mode-maybe)

(provide 'defnet)
;;; defnet.el ends here
