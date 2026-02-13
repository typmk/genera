---
description: Show recent REPL output from Clojure (CIDER), Common Lisp (SLIME), or Emacs Lisp
args:
  - name: repl-type
    description: Type of REPL (cider, slime, or elisp)
    required: false
---

# Show REPL Output

Display recent output from active REPL sessions.

**Usage:** `/emacs-repl [cider|slime|elisp]`

If no argument provided, shows all active REPLs.

```bash
if [ -z "$ARGUMENTS" ]; then
  echo "Active REPLs:"
  echo ""

  # Check CIDER
  if emacsclient -e '(get-buffer "*cider-repl*")' 2>/dev/null | grep -q '#<buffer'; then
    echo "=== CIDER REPL (Clojure) ==="
    emacsclient -e '(with-current-buffer "*cider-repl*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
    echo ""
  fi

  # Check SLIME
  if emacsclient -e '(get-buffer "*slime-repl sbcl*")' 2>/dev/null | grep -q '#<buffer'; then
    echo "=== SLIME REPL (Common Lisp) ==="
    emacsclient -e '(with-current-buffer "*slime-repl sbcl*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
    echo ""
  fi

  # Check ielm
  if emacsclient -e '(get-buffer "*ielm*")' 2>/dev/null | grep -q '#<buffer'; then
    echo "=== ielm (Emacs Lisp) ==="
    emacsclient -e '(with-current-buffer "*ielm*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
  fi
else
  case "$ARGUMENTS" in
    cider)
      echo "=== CIDER REPL (Clojure) ==="
      emacsclient -e '(with-current-buffer "*cider-repl*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
      ;;
    slime)
      echo "=== SLIME REPL (Common Lisp) ==="
      emacsclient -e '(with-current-buffer "*slime-repl sbcl*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
      ;;
    elisp)
      echo "=== ielm (Emacs Lisp) ==="
      emacsclient -e '(with-current-buffer "*ielm*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
      ;;
    *)
      echo "Unknown REPL type: $ARGUMENTS"
      echo "Valid options: cider, slime, elisp"
      ;;
  esac
fi
```

Shows the last ~1000 characters of REPL output.

**Examples:**
- `/emacs-repl` - Show all active REPLs
- `/emacs-repl cider` - Show only CIDER (Clojure) REPL
- `/emacs-repl slime` - Show only SLIME (Common Lisp) REPL
- `/emacs-repl elisp` - Show only ielm (Emacs Lisp) REPL
