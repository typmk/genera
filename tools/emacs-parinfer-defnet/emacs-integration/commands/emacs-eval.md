---
description: Safely evaluate an Emacs Lisp expression
args:
  - name: expression
    description: The Elisp expression to evaluate
    required: true
---

# Safely Evaluate Emacs Lisp Expression

Evaluate an Emacs Lisp expression with error handling.

**Usage:** `/emacs-eval (+ 1 2 3)`

```bash
emacsclient -e "(claude/eval-safe \"$ARGUMENTS\")"
```

This safely evaluates the expression and returns structured output with status and result or error message.

**Examples:**
- `/emacs-eval (emacs-version)` - Get Emacs version
- `/emacs-eval (+ 1 2 3)` - Simple arithmetic
- `/emacs-eval (buffer-list)` - List all buffers
- `/emacs-eval (featurep 'cider)` - Check if CIDER is loaded
