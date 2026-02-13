;;; claude-helpers.el --- Helper functions for Claude Code to interact with Emacs

;;; Commentary:
;; This file provides optimized functions for Claude Code to programmatically
;; interact with Emacs through emacsclient.
;;
;; These functions return structured data that's easy to parse and provide
;; better error handling and feedback.

;;; Code:

(defun claude/ping ()
  "Simple ping function to test connectivity.
Returns a plist with status and timestamp."
  (list :status "ok"
        :timestamp (current-time-string)
        :emacs-version emacs-version))

(defun claude/get-config (var-name)
  "Get the value of a configuration variable VAR-NAME.
Returns a plist with the variable name and value."
  (let ((sym (intern var-name)))
    (list :variable var-name
          :exists (boundp sym)
          :value (if (boundp sym) (symbol-value sym) nil))))

(defun claude/list-buffers ()
  "List all buffers with useful information.
Returns a list of plists with buffer info."
  (mapcar (lambda (buf)
            (with-current-buffer buf
              (list :name (buffer-name)
                    :file (buffer-file-name)
                    :modified (buffer-modified-p)
                    :mode (symbol-name major-mode)
                    :size (buffer-size))))
          (buffer-list)))

(defun claude/buffer-info (buffer-name)
  "Get detailed information about BUFFER-NAME.
Returns a plist with buffer details."
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (list :status "ok"
              :name (buffer-name)
              :file (buffer-file-name)
              :modified (buffer-modified-p)
              :mode (symbol-name major-mode)
              :size (buffer-size)
              :point (point)
              :line (line-number-at-pos)
              :column (current-column)
              :readonly buffer-read-only))
    (list :status "error"
          :message (format "Buffer '%s' not found" buffer-name))))

(defun claude/read-buffer-region (buffer-name start end)
  "Read region from START to END in BUFFER-NAME.
Returns a plist with status and content."
  (if-let ((buf (get-buffer buffer-name)))
      (with-current-buffer buf
        (list :status "ok"
              :content (buffer-substring-no-properties start end)
              :start start
              :end end
              :length (- end start)))
    (list :status "error"
          :message (format "Buffer '%s' not found" buffer-name))))

(defun claude/get-messages (&optional n)
  "Get the last N messages from the *Messages* buffer.
Returns a plist with messages as a list."
  (let ((n (or n 20)))
    (with-current-buffer "*Messages*"
      (save-excursion
        (goto-char (point-max))
        (forward-line (- n))
        (list :status "ok"
              :count n
              :messages (buffer-substring-no-properties (point) (point-max)))))))

(defun claude/eval-safe (code-string)
  "Safely evaluate CODE-STRING and return result with error handling.
Returns a plist with status and result or error."
  (condition-case err
      (let ((result (eval (read code-string))))
        (list :status "ok"
              :result result
              :printed (prin1-to-string result)))
    (error
     (list :status "error"
           :error-type (car err)
           :error-message (error-message-string err)))))

(defun claude/package-info (package-name)
  "Get information about installed PACKAGE-NAME.
Returns a plist with package details."
  (let ((pkg (intern package-name)))
    (list :package package-name
          :loaded (featurep pkg)
          :available (package-installed-p pkg)
          :location (locate-library package-name))))

(defun claude/list-features (prefix)
  "List all loaded features matching PREFIX.
Returns a list of feature names."
  (let ((features-list '()))
    (dolist (feature features)
      (when (string-prefix-p prefix (symbol-name feature))
        (push (symbol-name feature) features-list)))
    (list :status "ok"
          :prefix prefix
          :count (length features-list)
          :features (sort features-list #'string<))))

(defun claude/check-function (func-name)
  "Check if function FUNC-NAME exists and get its info.
Returns a plist with function details."
  (let ((sym (intern func-name)))
    (list :function func-name
          :exists (fboundp sym)
          :type (cond
                 ((commandp sym) "interactive")
                 ((fboundp sym) "function")
                 (t nil))
          :interactive (commandp sym)
          :doc (if (fboundp sym)
                   (documentation sym)
                 nil))))

(defun claude/minor-modes ()
  "List all active minor modes in current buffer.
Returns a list of minor mode names."
  (let ((modes '()))
    (dolist (mode minor-mode-list)
      (when (and (boundp mode) (symbol-value mode))
        (push (symbol-name mode) modes)))
    (list :status "ok"
          :count (length modes)
          :modes (sort modes #'string<))))

(defun claude/file-info (filepath)
  "Get information about FILEPATH.
Returns a plist with file details."
  (if (file-exists-p filepath)
      (list :status "ok"
            :path filepath
            :exists t
            :readable (file-readable-p filepath)
            :writable (file-writable-p filepath)
            :directory (file-directory-p filepath)
            :size (file-attribute-size (file-attributes filepath))
            :modified (format-time-string
                      "%Y-%m-%d %H:%M:%S"
                      (file-attribute-modification-time (file-attributes filepath))))
    (list :status "error"
          :path filepath
          :exists nil
          :message "File does not exist")))

(defun claude/server-info ()
  "Get information about the Emacs server.
Returns a plist with server details."
  (list :status "ok"
        :running (server-running-p)
        :name server-name
        :use-tcp server-use-tcp
        :port (when server-use-tcp server-port)
        :clients (length server-clients)))

(defun claude/call-json (func-name &rest args)
  "Call FUNC-NAME with ARGS and return result as JSON string.
This is optimized for machine parsing."
  (require 'json)
  (condition-case err
      (let* ((func (intern func-name))
             (result (apply func args)))
        (json-encode (list :status "ok"
                          :function func-name
                          :result result)))
    (error
     (json-encode (list :status "error"
                       :function func-name
                       :error (error-message-string err))))))

(provide 'claude-helpers)
;;; claude-helpers.el ends here
