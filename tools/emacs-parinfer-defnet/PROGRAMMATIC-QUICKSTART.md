# Programmatic Emacs - Quick Start

## What Changed?

I've optimized how I interact with your Emacs programmatically!

## Before vs After

### Before (Raw emacsclient)
```bash
emacsclient -e 'claude-code-program'
# => "claude"
# Problem: No context, harder to parse
```

### After (Helper Functions)
```bash
emacsclient -e '(claude/get-config "claude-code-program")'
# => (:variable "claude-code-program" :exists t :value "claude")
# Better: Structured data with full context!
```

## What Was Added

### 1. Helper Functions (`claude-helpers.el`)

15+ specialized functions for programmatic Emacs interaction:

**Configuration:**
- `claude/get-config` - Get variable with metadata
- `claude/package-info` - Package details
- `claude/server-info` - Server status

**Inspection:**
- `claude/list-buffers` - All buffers with details
- `claude/buffer-info` - Specific buffer info
- `claude/minor-modes` - Active minor modes
- `claude/list-features` - Loaded features

**Execution:**
- `claude/eval-safe` - Safe evaluation with error handling
- `claude/check-function` - Function existence and details

**Monitoring:**
- `claude/get-messages` - Recent *Messages* content
- `claude/ping` - Connectivity test

**And more!** See `claude-helpers.el` for full list.

### 2. Auto-Loading

Helper functions now load automatically when Emacs starts!

Added to your `init.el` (lines 95-99):
```elisp
(let ((claude-helpers (expand-file-name "~/em/claude-helpers.el")))
  (when (file-exists-p claude-helpers)
    (load-file claude-helpers)
    (message "Claude helper functions loaded")))
```

### 3. Documentation

- **PROGRAMMATIC-EMACS.md** - Complete guide (ranking, comparison, examples)
- **PROGRAMMATIC-QUICKSTART.md** - This file
- **claude-helpers.el** - Well-documented helper functions

## Why This Matters

### Benefits for You
- ✅ Faster debugging
- ✅ Better error messages
- ✅ More accurate help
- ✅ Clearer communication

### Benefits for Claude Code
- ✅ Structured data (easy to parse)
- ✅ Error handling built-in
- ✅ Consistent interface
- ✅ Type safety
- ✅ Faster queries

## Quick Examples

### Check Configuration
```bash
emacsclient -e '(claude/get-config "claude-code-program")'
# Returns: (:variable "claude-code-program" :exists t :value "claude")
```

### List Buffers
```bash
emacsclient -e '(claude/list-buffers)'
# Returns: ((:name "*scratch*" :file nil :mode "lisp-interaction-mode") ...)
```

### Check Package
```bash
emacsclient -e '(claude/package-info "mcp")'
# Returns: (:package "mcp" :loaded t :available t :location "...")
```

### Safe Evaluation
```bash
emacsclient -e '(claude/eval-safe "(+ 1 2 3)")'
# Returns: (:status "ok" :result 6 :printed "6")

emacsclient -e '(claude/eval-safe "(undefined-function)")'
# Returns: (:status "error" :error-type void-function :error-message "...")
```

### Test Connectivity
```bash
emacsclient -e '(claude/ping)'
# Returns: (:status "ok" :timestamp "..." :emacs-version "30.2")
```

### Get Recent Messages
```bash
emacsclient -e '(claude/get-messages 10)'
# Returns: (:status "ok" :count 10 :messages "...")
```

## Testing

Run the test script:
```bash
bash test-programmatic.sh
```

This demonstrates all helper functions and shows the difference between raw eval and structured helpers.

## Usage in Practice

### Scenario: Debug Configuration Issue

**Old way:**
```bash
emacsclient -e 'claude-code-program'  # => "claude"
emacsclient -e '(boundp '\''claude-code-program)'  # => t
emacsclient -e '(featurep '\''claude-code)'  # => t
# Multiple queries, manual interpretation
```

**New way:**
```bash
emacsclient -e '(claude/get-config "claude-code-program")'
# => (:variable "claude-code-program" :exists t :value "claude")
# One query, all info, structured!
```

### Scenario: Check Package Status

**Old way:**
```bash
emacsclient -e '(featurep '\''mcp)'  # => t
emacsclient -e '(locate-library "mcp")'  # => "c:/Users/..."
# Separate queries
```

**New way:**
```bash
emacsclient -e '(claude/package-info "mcp")'
# => (:package "mcp" :loaded t :available t :location "c:/Users/...")
# Everything in one structured response!
```

### Scenario: Inspect Running State

**Old way:**
```bash
emacsclient -e '(mapcar '\''buffer-name (buffer-list))'  # => ("*scratch*" ...)
# Just names, no details
```

**New way:**
```bash
emacsclient -e '(claude/list-buffers)'
# => ((:name "*scratch*" :file nil :modified nil :mode "lisp-interaction-mode" :size 0) ...)
# Full details for each buffer!
```

## Ranking of Methods

1. **⭐⭐⭐⭐⭐ Helper functions** (NEW! - Best)
2. **⭐⭐⭐⭐ Raw emacsclient** (Still good for quick queries)
3. **⭐⭐⭐ Batch mode** (Clean slate testing)
4. **⭐⭐ File-based** (Fallback only)
5. **⭐ Manual** (Last resort)

See `PROGRAMMATIC-EMACS.md` for detailed comparison.

## What You Need to Do

### Option 1: Automatic (Recommended)
**Nothing!** The helpers are already configured to load automatically.

Just restart Emacs and they'll be available.

### Option 2: Manual Load
If Emacs is already running and you don't want to restart:

```bash
emacsclient -e '(load-file "C:/Users/Apollo/em/claude-helpers.el")'
```

## Verifying It Works

```bash
# Test ping
emacsclient -e '(claude/ping)'

# Should return something like:
# (:status "ok" :timestamp "Wed Nov 13 13:30:00 2024" :emacs-version "30.2")
```

If you see that, everything is working! 🎉

## Common Use Cases

| Task | Helper Function | Old Way |
|------|-----------------|---------|
| Check variable | `claude/get-config` | `boundp` + value lookup |
| List buffers | `claude/list-buffers` | `buffer-list` + manual mapping |
| Check package | `claude/package-info` | Multiple `featurep`/`locate-library` |
| Safe eval | `claude/eval-safe` | Raw `eval` with manual error handling |
| Get messages | `claude/get-messages` | Switch to buffer, read manually |
| Check function | `claude/check-function` | `fboundp` + `commandp` + `documentation` |

## Error Handling

### Before
```bash
emacsclient -e '(some-undefined-function)'
# ERROR: void-function some-undefined-function
# Process exits with error, hard to handle
```

### After
```bash
emacsclient -e '(claude/eval-safe "(some-undefined-function)")'
# (:status "error" :error-type void-function :error-message "...")
# Structured error, easy to handle!
```

## Integration with Claude Code

When you ask for Emacs help, I can now:

1. **Query faster** - Structured responses, no parsing needed
2. **Handle errors better** - Built-in error handling
3. **Get more context** - Single query returns complete info
4. **Validate changes** - Easy to check before/after state
5. **Debug efficiently** - Quick state inspection

## Files Summary

| File | Purpose | Location |
|------|---------|----------|
| `claude-helpers.el` | Helper functions | `~/em/` |
| `PROGRAMMATIC-EMACS.md` | Complete guide | `~/em/` |
| `PROGRAMMATIC-QUICKSTART.md` | This file | `~/em/` |
| `test-programmatic.sh` | Test script | `~/em/` |
| `init.el` | Auto-loads helpers | `~/.emacs.d/` |

## Next Steps

1. **Restart Emacs** (or load helpers manually)
2. **Run test**: `bash test-programmatic.sh`
3. **Ask for Emacs help** - I'll use the improved methods automatically!

## Questions?

- **Do I need to do anything?** No! It's already configured.
- **Will this slow down Emacs?** No, minimal overhead.
- **Can I use the old way?** Yes, both work fine.
- **How do I see all helpers?** Look at `claude-helpers.el`

## Summary

**What:** Added optimized helper functions for programmatic Emacs interaction

**Why:** Faster, more reliable, structured data, better error handling

**How:** Auto-loads in your init.el, available immediately

**Benefit:** Better Emacs help from Claude Code!

🚀 **Your Emacs is now optimized for programmatic interaction!** 🚀
