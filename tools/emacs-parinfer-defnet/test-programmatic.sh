#!/bin/bash
# Test script for programmatic Emacs interaction
# Demonstrates the difference between raw eval and helper functions

EMACSCLIENT="C:/Program Files/Emacs/emacs-30.2/bin/emacsclient.exe"

echo "=========================================="
echo "Programmatic Emacs Interaction Demo"
echo "=========================================="
echo ""

# Check if server is running
if ! "$EMACSCLIENT" -e "(server-running-p)" 2>/dev/null | grep -q "t"; then
    echo "ERROR: Emacs server not running"
    echo "Please start Emacs first"
    exit 1
fi

echo "✓ Emacs server is running"
echo ""

# Test 1: Ping
echo "Test 1: Connectivity Test"
echo "Command: (claude/ping)"
"$EMACSCLIENT" -e "(claude/ping)"
echo ""

# Test 2: Get Config (structured data)
echo "Test 2: Get Configuration Variable"
echo "Command: (claude/get-config \"claude-code-program\")"
"$EMACSCLIENT" -e "(claude/get-config \"claude-code-program\")"
echo ""

# Compare with raw eval
echo "Compare with raw eval:"
echo "Command: claude-code-program"
"$EMACSCLIENT" -e "claude-code-program"
echo ""
echo "Notice: Helper function returns structured data with context!"
echo ""

# Test 3: List Buffers
echo "Test 3: List All Buffers"
echo "Command: (claude/list-buffers)"
"$EMACSCLIENT" -e "(claude/list-buffers)" | head -20
echo "... (truncated)"
echo ""

# Test 4: Check if package exists
echo "Test 4: Check Package Information"
echo "Command: (claude/package-info \"mcp\")"
"$EMACSCLIENT" -e "(claude/package-info \"mcp\")"
echo ""

# Test 5: Safe evaluation
echo "Test 5: Safe Evaluation (with error handling)"
echo "Command: (claude/eval-safe \"(+ 1 2 3)\")"
"$EMACSCLIENT" -e "(claude/eval-safe \"(+ 1 2 3)\")"
echo ""

echo "Test 5b: Safe Evaluation (error case)"
echo "Command: (claude/eval-safe \"(undefined-function)\")"
"$EMACSCLIENT" -e "(claude/eval-safe \"(undefined-function)\")"
echo ""

# Test 6: Get recent messages
echo "Test 6: Get Recent Messages"
echo "Command: (claude/get-messages 5)"
"$EMACSCLIENT" -e "(claude/get-messages 5)"
echo ""

# Test 7: Check function
echo "Test 7: Check if Function Exists"
echo "Command: (claude/check-function \"claude-code-start\")"
"$EMACSCLIENT" -e "(claude/check-function \"claude-code-start\")"
echo ""

# Test 8: Server info
echo "Test 8: Server Information"
echo "Command: (claude/server-info)"
"$EMACSCLIENT" -e "(claude/server-info)"
echo ""

# Test 9: Minor modes
echo "Test 9: Active Minor Modes"
echo "Command: (claude/minor-modes)"
"$EMACSCLIENT" -e "(claude/minor-modes)"
echo ""

echo "=========================================="
echo "All tests completed!"
echo ""
echo "Key Advantages of Helper Functions:"
echo "  ✓ Structured data (plists)"
echo "  ✓ Error handling built-in"
echo "  ✓ Consistent return format"
echo "  ✓ Easy to parse"
echo "  ✓ Self-documenting"
echo ""
echo "See PROGRAMMATIC-EMACS.md for full documentation"
echo "=========================================="
