#!/bin/bash
# Test script for claude-code.el integration
# Run this after starting Emacs GUI

EMACSCLIENT="C:/Program Files/Emacs/emacs-30.2/bin/emacsclient.exe"

echo "==================================="
echo "Claude Code Emacs Integration Test"
echo "==================================="
echo ""

# Check if server is running
echo -n "Checking if Emacs server is running... "
if ! "$EMACSCLIENT" --eval "(server-running-p)" 2>/dev/null | grep -q "t"; then
    echo "FAILED"
    echo ""
    echo "Error: Emacs server not running."
    echo "Please start Emacs GUI first. The server will start automatically."
    exit 1
fi
echo "OK"

# Test 1: Check if claude-code is loaded
echo -n "Test 1: Checking if claude-code is loaded... "
RESULT=$("$EMACSCLIENT" --eval "(featurep 'claude-code)" 2>&1)
if echo "$RESULT" | grep -q "t"; then
    echo "OK"
else
    echo "FAILED"
    echo "  Result: $RESULT"
fi

# Test 2: Check program name
echo -n "Test 2: Checking claude-code-program setting... "
RESULT=$("$EMACSCLIENT" --eval "claude-code-program" 2>&1)
if echo "$RESULT" | grep -q '"claude"'; then
    echo "OK (set to: $RESULT)"
else
    echo "FAILED"
    echo "  Expected: \"claude\""
    echo "  Got: $RESULT"
fi

# Test 3: Check terminal backend
echo -n "Test 3: Checking terminal backend... "
RESULT=$("$EMACSCLIENT" --eval "claude-code-terminal-backend" 2>&1)
if echo "$RESULT" | grep -q "eat"; then
    echo "OK (using: $RESULT)"
else
    echo "WARNING"
    echo "  Expected: eat"
    echo "  Got: $RESULT"
fi

# Test 4: Check if mode is enabled
echo -n "Test 4: Checking if claude-code-mode is active... "
RESULT=$("$EMACSCLIENT" --eval "claude-code-mode" 2>&1)
if echo "$RESULT" | grep -q "t"; then
    echo "OK"
else
    echo "FAILED"
    echo "  Mode not active: $RESULT"
fi

# Test 5: Check if command map exists
echo -n "Test 5: Checking if command map exists... "
RESULT=$("$EMACSCLIENT" --eval "(boundp 'claude-code-command-map)" 2>&1)
if echo "$RESULT" | grep -q "t"; then
    echo "OK"
else
    echo "FAILED"
    echo "  Command map not found: $RESULT"
fi

# Test 6: List available claude-code functions
echo ""
echo "Available claude-code functions:"
"$EMACSCLIENT" --eval "(let ((funcs '())) (mapatoms (lambda (s) (when (and (fboundp s) (string-prefix-p \"claude-code-\" (symbol-name s))) (push (symbol-name s) funcs)))) (sort funcs 'string<))" 2>&1 | sed 's/^/  /'

# Test 7: Check if claude command is accessible
echo ""
echo -n "Test 7: Checking if 'claude' command is accessible... "
if command -v claude &> /dev/null; then
    VERSION=$(claude --version 2>&1)
    echo "OK ($VERSION)"
else
    echo "FAILED"
    echo "  The 'claude' command is not in PATH"
fi

echo ""
echo "==================================="
echo "Test Summary"
echo "==================================="
echo "If all tests passed, you can use C-c c in Emacs to access Claude Code."
echo "If any tests failed, check the output above for details."
echo ""
echo "Quick test in Emacs:"
echo "  1. Press C-c c c to start Claude"
echo "  2. Or press C-c c m to see the menu"
