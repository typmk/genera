---
name: emacs-config-query
description: Query Emacs configuration variables, check package status, and retrieve settings using structured helper functions. Invoked when user asks about Emacs configuration, package installation status, or variable values.
allowed-tools:
  - Bash
---

# Emacs Configuration Query Skill

You are an expert at querying Emacs configuration programmatically using emacsclient.

## When to Use This Skill

Automatically activate when:
- User asks "Is package X installed?"
- User asks "What is the value of variable Y?"
- User asks about Emacs configuration settings
- User asks "Is feature Z loaded?"
- User wants to check Emacs state before making changes

## Available Query Functions

### Connectivity Test (~15 tokens)
```bash
emacsclient -e '(claude/ping)'
```
Returns: (:status "ok" :emacs-version "30.2" :uptime SECONDS)

### Get Configuration Variable (~20 tokens)
```bash
emacsclient -e '(claude/get-config "variable-name")'
```
Returns: (:variable NAME :value VALUE :type TYPE :documentation DOC)

### Check Package/Feature Loaded (~15 tokens)
```bash
emacsclient -e '(featurep '\''package-name)'
```
Returns: t or nil

### Get Recent Messages (~30-50 tokens)
```bash
emacsclient -e '(claude/get-messages 20)'
```
Returns: Last N messages from *Messages* buffer

### List All Buffers (~30-50 tokens)
```bash
emacsclient -e '(claude/list-buffers)'
```
Returns: List of all buffers with metadata

### Get Specific Buffer Info (~40 tokens)
```bash
emacsclient -e '(claude/buffer-info "buffer-name")'
```
Returns: Detailed information about specific buffer

### Safe Evaluation (~30-50 tokens)
```bash
emacsclient -e '(claude/eval-safe "(expression)")'
```
Returns: (:status "ok"|"error" :result RESULT :error-message MSG)

## Query Strategy

**Always check before acting:**

1. **Verify Connectivity** - Use `claude/ping` before any operation
2. **Check Prerequisites** - Use `featurep` to check if packages loaded
3. **Get Current State** - Use `claude/get-config` to see existing values
4. **Safe Testing** - Use `claude/eval-safe` for potentially risky operations

## Example Usage Patterns

### Check if Package Installed
```bash
# User asks: "Is CIDER installed?"

emacsclient -e '(featurep '\''cider)'
# Returns: t (installed) or nil (not installed)

# If installed, get more details:
emacsclient -e '(claude/get-config "cider-version")'
```

### Get Configuration Value
```bash
# User asks: "What's my LSP server configuration?"

emacsclient -e '(claude/get-config "eglot-server-programs")'
# Returns: structured plist with value and documentation
```

### Check Multiple Features
```bash
# User asks: "What Lisp environments are set up?"

# Check each feature
emacsclient -e '(list :clojure (featurep '\''clojure-mode) :slime (featurep '\''slime) :elisp (featurep '\''elisp-mode))'
```

### Safe Experimental Evaluation
```bash
# User wants to test something risky

emacsclient -e '(claude/eval-safe "(require '\''potentially-missing-package)")'
# Returns: (:status "error" :error-message "Cannot open load file...")
```

## Path Handling for Windows

When querying path-related variables:
- Emacs uses forward slashes internally: `C:/Users/Apollo/`
- Convert if needed for display to user

## Structured Output Parsing

All helper functions return plists for easy parsing:

```elisp
# claude/get-config returns:
(:variable "variable-name"
 :value "current-value"
 :type 'symbol|'string|'number|'list
 :documentation "Variable documentation..."
 :source "file-where-defined.el")

# claude/ping returns:
(:status "ok"
 :emacs-version "30.2"
 :uptime 3600
 :server-running t)

# claude/eval-safe returns:
(:status "ok"|"error"
 :result "evaluation-result"
 :error-message "error description if failed")
```

## Common Queries Reference

| User Question | Command |
|---------------|---------|
| "Is X loaded?" | `(featurep '\''X)` |
| "What is variable Y?" | `(claude/get-config "Y")` |
| "Is Emacs running?" | `(claude/ping)` |
| "What buffers are open?" | `(claude/list-buffers)` |
| "What's in *Messages*?" | `(claude/get-messages N)` |
| "Test if X works" | `(claude/eval-safe "(X)")` |

## Error Handling

If queries fail:
1. Check connectivity first with `claude/ping`
2. Verify helper functions loaded
3. Try safe evaluation wrapper if direct query fails
4. Report specific error to user with context

## Best Practices

- **Always verify before modifying** - Check current state first
- **Use structured functions** - Never parse raw elisp unless necessary
- **Batch related queries** - Combine multiple checks when possible
- **Handle nil gracefully** - Many queries return nil for "not found"
- **Quote symbols properly** - Use `'\''symbol` in bash for Emacs symbols
