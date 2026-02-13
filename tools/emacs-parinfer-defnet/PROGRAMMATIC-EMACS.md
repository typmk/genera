# Optimal Programmatic Interaction with Emacs

## Overview

This guide covers the best ways for Claude Code to interact programmatically with your Emacs instance, ranked from most to least optimal.

## Ranking: Best to Worst

1. **emacsclient with helper functions** ⭐⭐⭐⭐⭐ (BEST)
2. **emacsclient with direct --eval** ⭐⭐⭐⭐
3. **Batch mode evaluation** ⭐⭐⭐
4. **File-based communication** ⭐⭐
5. **Manual user interaction** ⭐

---

## 1. emacsclient with Helper Functions (BEST ⭐⭐⭐⭐⭐)

### Why This is Optimal

**Pros:**
- ✅ Real-time interaction with running Emacs
- ✅ Structured data returns (plists, JSON)
- ✅ Better error handling
- ✅ No parsing of complex Elisp output
- ✅ Reusable functions
- ✅ Fast (no process startup)
- ✅ Type-safe interfaces
- ✅ Can inspect live state

**Cons:**
- Requires initial setup (loading helper functions)

### Setup

#### Step 1: Load Helper Functions

Add to your `init.el`:
```elisp
;; Load Claude helper functions
(load-file (expand-file-name "~/em/claude-helpers.el"))
```

Or load dynamically when needed:
```bash
emacsclient -e '(load-file "C:/Users/Apollo/em/claude-helpers.el")'
```

#### Step 2: Use Helper Functions

**Basic Examples:**

```bash
# Ping test
emacsclient -e '(claude/ping)'
# => (:status "ok" :timestamp "..." :emacs-version "30.2")

# Get config variable
emacsclient -e '(claude/get-config "claude-code-program")'
# => (:variable "claude-code-program" :exists t :value "claude")

# List buffers
emacsclient -e '(claude/list-buffers)'
# => ((:name "*scratch*" :file nil :modified nil ...) ...)

# Get buffer info
emacsclient -e '(claude/buffer-info "*scratch*")'
# => (:status "ok" :name "*scratch*" :mode "lisp-interaction-mode" ...)

# Check if function exists
emacsclient -e '(claude/check-function "claude-code-start")'
# => (:function "claude-code-start" :exists t :type "interactive" ...)

# Get recent messages
emacsclient -e '(claude/get-messages 10)'
# => (:status "ok" :count 10 :messages "...")

# Safe evaluation
emacsclient -e '(claude/eval-safe "(+ 1 2 3)")'
# => (:status "ok" :result 6 :printed "6")
```

### Available Helper Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `claude/ping` | Test connectivity | Status plist |
| `claude/get-config` | Get variable value | Variable info plist |
| `claude/list-buffers` | List all buffers | List of buffer plists |
| `claude/buffer-info` | Get buffer details | Buffer info plist |
| `claude/read-buffer-region` | Read buffer region | Content plist |
| `claude/get-messages` | Get recent messages | Messages plist |
| `claude/eval-safe` | Safe evaluation | Result/error plist |
| `claude/package-info` | Package information | Package info plist |
| `claude/list-features` | List loaded features | Features list |
| `claude/check-function` | Function info | Function details plist |
| `claude/minor-modes` | Active minor modes | Modes list |
| `claude/file-info` | File details | File info plist |
| `claude/server-info` | Server status | Server info plist |

### Advanced: JSON Output

For machine parsing:
```bash
emacsclient -e '(require '\''json) (json-encode (claude/ping))'
# => {"status":"ok","timestamp":"...","emacs-version":"30.2"}
```

### Why This is Better Than Raw --eval

**Raw --eval:**
```bash
emacsclient -e '(boundp '\''claude-code-program)'
# => t
# Problem: Just "t", no context
```

**With helpers:**
```bash
emacsclient -e '(claude/get-config "claude-code-program")'
# => (:variable "claude-code-program" :exists t :value "claude")
# Better: Structured data with context
```

---

## 2. emacsclient with Direct --eval (⭐⭐⭐⭐)

### When to Use

- Helper functions not loaded yet
- Quick one-off queries
- Simple expressions
- Already know the Elisp

### Best Practices

#### Use -u to Suppress Unwanted Output
```bash
# Without -u (may show extra messages)
emacsclient -e '(message "test")'

# With -u (clean output)
emacsclient -e '(message "test")' -u
```

#### Return Structured Data
```bash
# Bad: Returns raw value
emacsclient -e 'claude-code-program'
# => "claude"

# Good: Returns structure
emacsclient -e '(list :var "claude-code-program" :value claude-code-program)'
# => (:var "claude-code-program" :value "claude")
```

#### Handle Errors Gracefully
```bash
emacsclient -e '(condition-case err (do-something) (error (list :error (error-message-string err))))'
```

#### Use progn for Multiple Expressions
```bash
emacsclient -e '(progn (require '\''claude-code) (message "Loaded") (featurep '\''claude-code))'
```

### Advanced Options

#### -n: No Wait (Async)
```bash
# Don't wait for result (fire and forget)
emacsclient -n -e '(message "Starting long process...")'
```

#### -q: Quiet Mode
```bash
# Suppress success messages
emacsclient -q -e '(+ 1 2)'
```

#### -w: Timeout
```bash
# Wait max 5 seconds
emacsclient -w 5 -e '(some-long-operation)'
```

### Common Patterns

#### Check if Feature Loaded
```bash
emacsclient -e '(featurep '\''claude-code)'
```

#### Get Variable Value
```bash
emacsclient -e 'claude-code-program'
```

#### Check if Function Exists
```bash
emacsclient -e '(fboundp '\''claude-code-start)'
```

#### List Matching Symbols
```bash
emacsclient -e '(apropos-internal "^claude-code-" '\''commandp)'
```

#### Safe Require
```bash
emacsclient -e '(require '\''claude-code t)'
# Returns t if loaded, nil if not found
```

---

## 3. Batch Mode Evaluation (⭐⭐⭐)

### When to Use

- Clean slate needed
- Testing configuration loading
- No running Emacs instance
- Automated testing

### Usage

```bash
emacs --batch \
  --load "~/.emacs.d/init.el" \
  --eval '(message "Result: %s" (+ 1 2))'
```

### Pros/Cons

**Pros:**
- ✅ Clean environment
- ✅ No state pollution
- ✅ Good for testing config

**Cons:**
- ❌ Slow (starts new process)
- ❌ Can't inspect running state
- ❌ No interaction with GUI
- ❌ Startup overhead

### Best for

- Configuration validation
- Package installation
- Automated scripts
- CI/CD pipelines

---

## 4. File-Based Communication (⭐⭐)

### How It Works

Write commands to a file, have Emacs watch and execute them.

### Setup

```elisp
(defvar claude-command-file "~/em/claude-commands.txt")
(defvar claude-output-file "~/em/claude-output.txt")

(defun claude-watch-file ()
  (run-with-timer 1 1
    (lambda ()
      (when (file-exists-p claude-command-file)
        (with-temp-buffer
          (insert-file-contents claude-command-file)
          (let ((result (eval (read (buffer-string)))))
            (with-temp-file claude-output-file
              (prin1 result (current-buffer)))
            (delete-file claude-command-file)))))))
```

### Usage

```bash
# Write command
echo "(+ 1 2 3)" > ~/em/claude-commands.txt

# Wait and read result
sleep 2
cat ~/em/claude-output.txt
```

### Pros/Cons

**Pros:**
- ✅ Works without server
- ✅ Cross-platform
- ✅ Simple concept

**Cons:**
- ❌ Slow (file I/O)
- ❌ Polling overhead
- ❌ Race conditions
- ❌ Messy (temp files)
- ❌ Complex setup

**Use only when:** emacsclient not available

---

## 5. Manual User Interaction (⭐)

### What This Means

Asking user to run commands manually in Emacs.

### When to Use

- Absolute last resort
- User preference
- Learning/teaching

### Pros/Cons

**Pros:**
- ✅ User sees what's happening
- ✅ No technical requirements
- ✅ User has control

**Cons:**
- ❌ Slow
- ❌ Error-prone
- ❌ Not automated
- ❌ Interrupts workflow

---

## Comparison Matrix

| Method | Speed | Reliability | Setup | State Access | Best For |
|--------|-------|-------------|-------|--------------|----------|
| emacsclient + helpers | ⚡⚡⚡⚡⚡ | ✅✅✅✅✅ | Medium | Live state | Everything |
| emacsclient --eval | ⚡⚡⚡⚡ | ✅✅✅✅ | None | Live state | Quick queries |
| Batch mode | ⚡⚡ | ✅✅✅ | None | Clean slate | Testing |
| File-based | ⚡ | ✅✅ | Complex | Live state | Fallback |
| Manual | 🐌 | ✅ | None | User-driven | Learning |

---

## Recommended Workflow for Claude Code

### Phase 1: Initial Setup
```bash
# Load helper functions once
emacsclient -e '(load-file "C:/Users/Apollo/em/claude-helpers.el")'
```

### Phase 2: Regular Operations

**For queries:**
```bash
# Use helper functions
emacsclient -e '(claude/get-config "claude-code-program")'
emacsclient -e '(claude/list-buffers)'
emacsclient -e '(claude/package-info "claude-code")'
```

**For quick checks:**
```bash
# Direct eval for simple things
emacsclient -e '(featurep '\''mcp)'
```

**For modifications:**
```bash
# Direct eval for setq, require, etc.
emacsclient -e '(setq test-var 42)'
emacsclient -e '(require '\''mcp)'
```

### Phase 3: Error Handling

```bash
# Use safe eval
emacsclient -e '(claude/eval-safe "(risky-function)")'
# Returns: (:status "error" :error-type ... :error-message ...)
```

---

## Integration with Claude Code Workflow

### Scenario 1: Debugging Configuration Issue

```bash
# 1. Check if package loaded
emacsclient -e '(claude/package-info "claude-code")'

# 2. Check configuration
emacsclient -e '(claude/get-config "claude-code-program")'

# 3. Check if function exists
emacsclient -e '(claude/check-function "claude-code-start")'

# 4. Get recent errors
emacsclient -e '(claude/get-messages 20)'
```

### Scenario 2: Installing New Package

```bash
# 1. Load package
emacsclient -e '(straight-use-package '\''package-name)'

# 2. Verify installation
emacsclient -e '(claude/package-info "package-name")'

# 3. Check if loaded
emacsclient -e '(featurep '\''package-name)'
```

### Scenario 3: Inspecting Running State

```bash
# 1. What buffers are open?
emacsclient -e '(claude/list-buffers)'

# 2. What's in *Messages*?
emacsclient -e '(claude/get-messages)'

# 3. What minor modes active?
emacsclient -e '(claude/minor-modes)'

# 4. Server status?
emacsclient -e '(claude/server-info)'
```

---

## Advanced: TCP Server for Remote Access

### Enable TCP Server

In your `init.el`:
```elisp
(setq server-use-tcp t)
(setq server-host "localhost")
(setq server-port 9999)
(server-start)
```

### Connect Remotely

```bash
emacsclient -f "localhost:9999" -e '(message "Hello from remote")'
```

### Security Note

TCP server requires authentication. Auth file at:
`~/.emacs.d/server/server`

---

## Best Practices Summary

### DO ✅

1. **Use helper functions** for structured data
2. **Load helpers once** at session start
3. **Handle errors gracefully** with condition-case
4. **Return plists or JSON** for easy parsing
5. **Use -u flag** to suppress noise
6. **Test connectivity** with ping function
7. **Keep functions pure** (no side effects in queries)

### DON'T ❌

1. **Don't parse raw Elisp output** (use structured data)
2. **Don't use batch mode** for live state queries
3. **Don't ignore errors** (use safe wrappers)
4. **Don't make assumptions** (check before acting)
5. **Don't use file-based** unless necessary
6. **Don't start multiple servers** (check with server-running-p)

---

## Files Created

- **claude-helpers.el** - Helper functions for programmatic access
- **PROGRAMMATIC-EMACS.md** - This guide

---

## Next Steps

1. ✅ Load helper functions in your Emacs
2. ✅ Test connectivity with ping
3. ✅ Use structured helpers instead of raw --eval
4. ✅ Enjoy faster, more reliable automation!

---

## Quick Reference Card

```bash
# Setup (once per session)
emacsclient -e '(load-file "~/em/claude-helpers.el")'

# Test connectivity
emacsclient -e '(claude/ping)'

# Get config
emacsclient -e '(claude/get-config "VAR-NAME")'

# List buffers
emacsclient -e '(claude/list-buffers)'

# Check package
emacsclient -e '(claude/package-info "PACKAGE")'

# Safe eval
emacsclient -e '(claude/eval-safe "CODE")'

# Get messages
emacsclient -e '(claude/get-messages)'
```

## Conclusion

**Optimal approach:** emacsclient with helper functions

This provides:
- Fast real-time access ⚡
- Structured, parseable data 📊
- Error handling 🛡️
- Type safety ✅
- Reusability ♻️

Load `claude-helpers.el` and enjoy programmatic Emacs! 🚀
