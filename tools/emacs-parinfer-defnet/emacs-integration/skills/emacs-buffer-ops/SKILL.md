---
name: emacs-buffer-ops
description: Interact with Emacs buffers programmatically - list, inspect, read contents, and get buffer metadata. Invoked when user asks about open buffers, REPL state, or wants to interact with running Emacs sessions.
allowed-tools:
  - Bash
---

# Emacs Buffer Operations Skill

You are an expert at interacting with Emacs buffers using programmatic emacsclient commands.

## When to Use This Skill

Automatically activate when:
- User asks "What buffers are open?"
- User wants to see REPL state (e.g., "*cider-repl*", "*slime-repl*")
- User asks about specific buffer contents
- User wants to check compilation output or test results
- User needs to inspect *Messages*, *warnings*, or other special buffers

## Available Buffer Functions

### List All Buffers (~30-50 tokens)
```bash
emacsclient -e '(claude/list-buffers)'
```
Returns: List of all buffers with names, sizes, modes, and modified status

### Get Specific Buffer Info (~40 tokens)
```bash
emacsclient -e '(claude/buffer-info "buffer-name")'
```
Returns: Detailed buffer metadata (mode, size, point position, modified, file)

### Read Buffer Contents (variable, ~50-5000+ tokens)
```bash
emacsclient -e '(with-current-buffer "buffer-name" (buffer-substring-no-properties (point-min) (point-max)))'
```
Returns: Full buffer contents as string

### Read Buffer Excerpt (~50-500 tokens)
```bash
emacsclient -e '(with-current-buffer "buffer-name" (buffer-substring-no-properties (point-min) (min 1000 (point-max))))'
```
Returns: First N characters of buffer

### Check Buffer Exists (~15 tokens)
```bash
emacsclient -e '(get-buffer "buffer-name")'
```
Returns: buffer object or nil

### Get Buffer Major Mode (~20 tokens)
```bash
emacsclient -e '(with-current-buffer "buffer-name" major-mode)'
```
Returns: Symbol like 'clojure-mode, 'emacs-lisp-mode, etc.

## Buffer Operation Strategy

**Progressive information gathering:**

1. **List First** - Use `claude/list-buffers` to see what's available
2. **Get Metadata** - Use `claude/buffer-info` for specific buffer details
3. **Read Selectively** - Only read full contents if absolutely necessary
4. **Excerpt When Possible** - Read first/last N characters for large buffers

## Common Buffer Patterns

### REPL Interaction

```bash
# User asks: "What's in my Clojure REPL?"

# Step 1: Check if REPL buffer exists
emacsclient -e '(get-buffer "*cider-repl*")'

# Step 2: Get recent output (last 1000 chars)
emacsclient -e '(with-current-buffer "*cider-repl*" (buffer-substring-no-properties (max 1 (- (point-max) 1000)) (point-max)))'
```

### Check Compilation Results

```bash
# User asks: "Did my code compile successfully?"

# Check *compilation* buffer
emacsclient -e '(claude/buffer-info "*compilation*")'

# Read last 500 chars for errors/warnings
emacsclient -e '(with-current-buffer "*compilation*" (buffer-substring-no-properties (max 1 (- (point-max) 500)) (point-max)))'
```

### Inspect Messages Buffer

```bash
# User asks: "Any errors in Emacs?"

# Get recent messages
emacsclient -e '(claude/get-messages 20)'
```

### List Files Being Edited

```bash
# User asks: "What files do I have open?"

# List all buffers and filter for file buffers
emacsclient -e '(claude/list-buffers)'
# Parse output for buffers with :file property
```

## Structured Output Format

### claude/list-buffers
```elisp
((:name "*scratch*"
  :size 234
  :mode 'lisp-interaction-mode
  :modified nil
  :file nil)
 (:name "init.el"
  :size 5678
  :mode 'emacs-lisp-mode
  :modified t
  :file "C:/Users/Apollo/.emacs.d/init.el"))
```

### claude/buffer-info
```elisp
(:name "buffer-name"
 :size 1234
 :mode 'clojure-mode
 :modified nil
 :file "C:/path/to/file.clj"
 :point 567
 :point-min 1
 :point-max 1234
 :line-number 15
 :column 23)
```

## Special Buffer Names

Common Emacs special buffers:
- `*scratch*` - Elisp scratch buffer
- `*Messages*` - Log messages
- `*compilation*` - Compilation output
- `*cider-repl*` - Clojure REPL
- `*slime-repl sbcl*` - Common Lisp REPL
- `*Warnings*` - Warning messages
- `*Help*` - Help documentation
- `*Backtrace*` - Error backtraces

## Performance Considerations

**Token usage by operation:**
- List buffers: ~30-50 tokens (all buffers)
- Buffer info: ~40 tokens (one buffer)
- Read 100 chars: ~50-100 tokens
- Read full file: ~500-50000 tokens (avoid unless necessary)

**Best practices:**
- Always list or check metadata before reading contents
- Use excerpts (first/last N chars) for large buffers
- Filter to relevant buffers when possible
- Cache buffer lists if querying multiple times

## Common Use Cases

### REPL State Inspection
```bash
# Check what REPLs are running
emacsclient -e '(claude/list-buffers)'
# Filter for *-repl* buffers

# Get REPL output
emacsclient -e '(with-current-buffer "*cider-repl*" ...)'
```

### Check Test Results
```bash
# After running tests, check results buffer
emacsclient -e '(claude/buffer-info "*cider-test-report*")'
```

### Monitor Background Processes
```bash
# Check if compilation/linting finished
emacsclient -e '(with-current-buffer "*flycheck-errors*" ...)'
```

### Debugging Helper
```bash
# Get backtrace from error
emacsclient -e '(with-current-buffer "*Backtrace*" (buffer-string))'
```

## Error Handling

If buffer operations fail:
1. Verify buffer exists with `get-buffer`
2. Check buffer name is exact (case-sensitive, with asterisks)
3. Confirm Emacs server is running with `claude/ping`
4. Handle nil returns gracefully (buffer not found)

## Integration with Other Skills

This skill works well with:
- **emacs-diagnostics**: Check error buffers after diagnostic runs
- **emacs-config-query**: Verify package modes match buffer modes
- Combine buffer list with config checks for comprehensive state
