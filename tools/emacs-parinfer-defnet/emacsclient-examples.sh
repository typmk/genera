#!/bin/bash
# Examples of using emacsclient for testing and debugging
# These commands work while Emacs GUI is running with server started

EMACSCLIENT="C:/Program Files/Emacs/emacs-30.2/bin/emacsclient.exe"

echo "==================================="
echo "emacsclient Examples for Debugging"
echo "==================================="
echo ""
echo "Make sure Emacs is running first!"
echo ""

# Example 1: Simple evaluation
echo "Example 1: Evaluate a simple expression"
echo "Command: emacsclient --eval \"(+ 1 2)\""
"$EMACSCLIENT" --eval "(+ 1 2)"
echo ""

# Example 2: Check feature
echo "Example 2: Check if a feature is loaded"
echo "Command: emacsclient --eval \"(featurep 'claude-code)\""
"$EMACSCLIENT" --eval "(featurep 'claude-code)"
echo ""

# Example 3: Get variable value
echo "Example 3: Get a variable value"
echo "Command: emacsclient --eval \"claude-code-program\""
"$EMACSCLIENT" --eval "claude-code-program"
echo ""

# Example 4: Check function exists
echo "Example 4: Check if a function exists"
echo "Command: emacsclient --eval \"(fboundp 'claude-code-start)\""
"$EMACSCLIENT" --eval "(fboundp 'claude-code-start)"
echo ""

# Example 5: List buffers
echo "Example 5: List all buffers"
echo "Command: emacsclient --eval \"(mapcar 'buffer-name (buffer-list))\""
"$EMACSCLIENT" --eval "(mapcar 'buffer-name (buffer-list))"
echo ""

# Example 6: Get current buffer name
echo "Example 6: Get current buffer name"
echo "Command: emacsclient --eval \"(buffer-name)\""
"$EMACSCLIENT" --eval "(buffer-name)"
echo ""

# Example 7: Send a message (appears in *Messages* buffer)
echo "Example 7: Send a debug message"
echo "Command: emacsclient --eval \"(message \\\"Debug: Testing from emacsclient\\\")\""
"$EMACSCLIENT" --eval "(message \"Debug: Testing from emacsclient\")"
echo ""

# Example 8: Evaluate multiple expressions
echo "Example 8: Evaluate multiple expressions with progn"
echo "Command: emacsclient --eval \"(progn (setq test-var 42) test-var)\""
"$EMACSCLIENT" --eval "(progn (setq test-var 42) test-var)"
echo ""

# Example 9: Load or reload a package
echo "Example 9: Reload claude-code package"
echo "Command: emacsclient --eval \"(require 'claude-code t)\""
"$EMACSCLIENT" --eval "(require 'claude-code t)"
echo ""

# Example 10: Get all claude-code related variables
echo "Example 10: List claude-code customizable variables"
echo "Command: emacsclient --eval \"(apropos-variable \\\"^claude-code-\\\")\""
"$EMACSCLIENT" --eval "(let ((vars '())) (mapatoms (lambda (s) (when (and (boundp s) (string-prefix-p \"claude-code-\" (symbol-name s))) (push (cons (symbol-name s) (symbol-value s)) vars)))) vars)"
echo ""

# Example 11: Check keybinding
echo "Example 11: Check what C-c c is bound to"
echo "Command: emacsclient --eval \"(key-binding (kbd \\\"C-c c\\\"))\""
"$EMACSCLIENT" --eval "(key-binding (kbd \"C-c c\"))"
echo ""

# Example 12: Interactive debugging - set a variable
echo "Example 12: Temporarily change a setting"
echo "Command: emacsclient --eval \"(setq claude-code-program \\\"test-claude\\\")\""
"$EMACSCLIENT" --eval "(setq claude-code-program \"test-claude\")"
echo "Changed claude-code-program to: test-claude"
echo ""
echo "Changing it back..."
"$EMACSCLIENT" --eval "(setq claude-code-program \"claude\")"
echo ""

echo "==================================="
echo "Advanced Usage"
echo "==================================="
echo ""
echo "For more complex debugging, you can:"
echo "  1. Use IELM inside Emacs: M-x ielm"
echo "  2. Use *scratch* buffer: Type expression, then C-j"
echo "  3. Use edebug: C-u C-M-x on a function to instrument it"
echo "  4. Check *Messages*: C-h e"
echo "  5. Describe function: C-h f <function-name>"
echo "  6. Describe variable: C-h v <variable-name>"
echo ""
