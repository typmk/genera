---
description: Run parenthesis diagnostics on a Lisp file
args:
  - name: file
    description: Path to the .el, .clj, or .lisp file to diagnose
    required: true
---

# Diagnose Lisp File for Parenthesis Errors

Run comprehensive diagnostics on a Lisp file to find parenthesis/bracket errors.

**Usage:** `/emacs-diag path/to/file.el`

```bash
# Convert path to forward slashes for Emacs
FILE_PATH=$(echo "$1" | sed 's/\\/\//g')

echo "Running quick summary..."
emacsclient -e "(claude/quick-paren-summary \"$FILE_PATH\")"

echo ""
echo "Running detailed diagnostic..."
emacsclient -e "(claude/minimal-diagnostic \"$FILE_PATH\")"

echo ""
echo "Running combined multi-method analysis..."
emacsclient -e "(claude/combined-diagnosis \"$FILE_PATH\")"
```

This runs three levels of diagnostics:
1. **Quick summary** - Overall balance check
2. **Minimal diagnostic** - Precise error location
3. **Combined diagnosis** - Cross-validated multi-method analysis

**Examples:**
- `/emacs-diag init.el` - Check init file
- `/emacs-diag C:/Users/Apollo/project/core.clj` - Check Clojure file
- `/emacs-diag src/main.lisp` - Check Common Lisp file
