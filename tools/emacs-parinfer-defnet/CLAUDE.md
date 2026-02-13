# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs configuration and development environment repository focused on:
- Programmatic Emacs interaction via `emacsclient`
- LLM-optimized diagnostic tools for Lisp code
- Clojure, Common Lisp, and Emacs Lisp development
- Integration between Claude Code and Emacs
- Model Context Protocol (MCP) server management

## Critical Paths

### Emacs Configuration
- **Main config**: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el`
- **Emacs binary**: `C:/Program Files/Emacs/emacs-30.2/bin/emacs.exe`
- **Claude CLI**: `C:/Users/Apollo/.local/bin/claude.exe`

### Key Helper Libraries
- **claude-helpers.el** - Structured functions for programmatic Emacs queries (C:\Users\Apollo\em\claude-helpers.el:1)
- **claude-paren-diagnostics.el** - Token-efficient parenthesis diagnostics (C:\Users\Apollo\em\claude-paren-diagnostics.el:1)
- **bracket-type-detection-clean.el** - Type-aware bracket diagnostics for (), [], {} (C:\Users\Apollo\em\bracket-type-detection-clean.el:1)

## Programmatic Emacs Interaction

### Priority Method: emacsclient with Helper Functions

**Always prefer structured helper functions over raw eval:**

```bash
# Good: Structured, parseable output
emacsclient -e '(claude/get-config "variable-name")'

# Avoid: Raw eval without structure
emacsclient -e 'variable-name'
```

### Essential Helper Functions

| Function | Purpose | Token Cost |
|----------|---------|------------|
| `claude/ping` | Test connectivity | ~15 tokens |
| `claude/get-config` | Get variable with metadata | ~20 tokens |
| `claude/list-buffers` | List all buffers with details | ~30-50 tokens |
| `claude/buffer-info` | Specific buffer information | ~40 tokens |
| `claude/minimal-diagnostic` | Quick paren error check | ~20-30 tokens |
| `claude/eval-safe` | Safe evaluation with error handling | ~30-50 tokens |

### Always Check Before Acting

Before making changes to Emacs configuration:
1. Verify connectivity: `emacsclient -e '(claude/ping)'`
2. Check if feature loaded: `emacsclient -e '(featurep '\''package-name)'`
3. Get current config: `emacsclient -e '(claude/get-config "var-name")'`

## Parenthesis/Bracket Diagnostics

### For Lisp Code Debugging

When encountering paren errors, use token-efficient diagnostics:

```bash
# Step 1: Quick health check (15-20 tokens)
emacsclient -e '(claude/quick-paren-summary "file.el")'

# Step 2: Precise error location (20-30 tokens)
emacsclient -e '(claude/minimal-diagnostic "file.el")'

# Step 3: If multiple errors suspected (30-40 tokens per error)
emacsclient -e '(claude/find-unmatched-opener "file.el")'
```

**Never ask for entire file contents when debugging parens** - use diagnostics instead (95%+ token savings).

### Available Diagnostic Functions

From `claude-paren-diagnostics.el`:
- `claude/minimal-diagnostic` - Essential error info only
- `claude/quick-paren-summary` - Overall balance check
- `claude/diagnose-parens` - Detailed error with context
- `claude/find-unmatched-opener` - All unmatched delimiters
- `claude/validate-by-indentation` - Heuristic hints from indentation
- `claude/combined-diagnosis` - Multi-method validation
- `claude/efficient-diagnosis` - Structure-based analysis with checkpoints

From `bracket-type-detection-clean.el` (type-aware diagnostics):
- `claude/count-by-type` - Count (), [], {} separately (~30 tokens)
- `claude/find-type-mismatches` - Detect type errors like `[}` (~40-60 tokens per error)
- `claude/bracket-summary` - Comprehensive analysis with recommendations (~50-80 tokens)

### Bracket Type Detection

**New capability**: Type-aware bracket diagnostics can detect when brackets are opened with one type but closed with another (e.g., `[1 2 3}` or `{:key value]`).

```bash
# Count each bracket type separately
emacsclient -e '(claude/count-by-type "file.clj")'
# => (:paren-open 50 :paren-close 48 :paren-diff 2
#     :square-open 10 :square-close 10 :square-diff 0
#     :curly-open 5 :curly-close 5 :curly-diff 0)

# Find type mismatches (critical errors)
emacsclient -e '(claude/find-type-mismatches "file.clj")'
# => (:status "error" :count 3 :type-mismatches (list of errors))

# Comprehensive summary
emacsclient -e '(claude/bracket-summary "file.clj")'
# => Full analysis with counts, type checks, and recommendation
```

**Key insight**: Type mismatches can appear "balanced" to simple counters. Example: `[1 2 3}` has 1 open bracket and 1 close bracket (balanced count) but uses wrong types. Only stack-based type tracking can detect this.

## Development Environments

### Clojure Development

**Key packages**: clojure-mode, cider, smartparens, rainbow-delimiters, eglot
**LSP**: clojure-lsp (via eglot)
**REPL**: CIDER

Commands from init.el:
- `eglot-ensure` - Auto-starts LSP for Clojure files
- `C-c l r` - Rename (eglot-rename)
- `C-c l a` - Code actions (eglot-code-actions)
- `C-c l f` - Format buffer (eglot-format-buffer)

Configuration location: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:105-166`

### Common Lisp Development

**Key packages**: slime
**Implementation**: SBCL
**REPL**: SLIME

Configuration location: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:179-190`

### Emacs Lisp Development

**Key packages**: flycheck, rainbow-delimiters, smartparens

**Linting tools** (prefer these over manual debugging):
- `elint` (built-in) - Structural linting, finds missing parens/brackets
- `byte-compile` (built-in) - Full compilation with warnings
- `checkdoc` (built-in) - Documentation style checking
- `flycheck` (installed) - Real-time syntax checking

### Emacs Lisp Linting Workflow

When debugging Emacs Lisp files, **always use linting tools first**:

```bash
# Step 1: Use elint to find structural errors (RECOMMENDED)
emacs --batch --eval "(progn (require 'elint) (find-file \"file.el\") (elint-current-buffer))"
# Output: Precise error messages with exact locations

# Step 2: Byte compile to find semantic issues
emacs --batch -f batch-byte-compile "file.el"
# Output: Warnings about undefined functions, wrong argument counts, etc.

# Step 3: If still having issues, use diagnostic functions
emacsclient -e '(claude/minimal-diagnostic "file.el")'
```

**Example elint output**:
```
Missing `)' in top form: (defun my-function (arg)
```

**Time savings**: elint identifies the exact error in seconds vs. 10-30 minutes of manual bracket counting.

## Model Context Protocol (MCP)

### MCP Server Management

MCP is installed but not configured with active servers by default.

**Package**: mcp.el (via mcp-hub)
**Commands**:
- `M-x mcp-hub` - Main control panel
- `mcp-hub-start-server` - Start specific server
- `mcp-hub-view-log` - View server logs

**Configuration location**: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:56-72`

### Adding MCP Servers

To configure MCP servers, modify `init.el` around line 63:

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em")))))
```

See `MCP-GUIDE.md` for comprehensive server configuration examples.

## Claude Code Integration

### Configuration

claude-code.el is installed with:
- **Terminal backend**: eat (better Windows support)
- **Command**: `claude` (not default `claude-code`)
- **Key prefix**: `C-c c`

Configuration location: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:74-95`

### Mode Cycling

Use `C-c c M` then repeat `M` to cycle between modes (repeat-mode enabled).

## Windows-Specific Considerations

### Path Format
- Use forward slashes in Elisp: `"C:/Users/Apollo/em/file.el"`
- Backslashes work in bash: `"C:\Users\Apollo\em\file.el"`

### Terminal Emulator
- eat is configured for better Windows support vs eshell/vterm
- Configuration: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:46-53`

### Line Endings
- UTF-8 encoding configured globally: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:29-33`

## Common Development Commands

### Testing Emacs Configuration

```bash
# Test connectivity
emacsclient -e '(claude/ping)'

# Check if package loaded
emacsclient -e '(featurep '\''package-name)'

# Get recent messages (for debugging)
emacsclient -e '(claude/get-messages 20)'

# List all buffers
emacsclient -e '(claude/list-buffers)'
```

### Diagnosing Lisp Code

```bash
# Quick paren check
emacsclient -e '(claude/minimal-diagnostic "file.el")'

# Full diagnostic suite
emacsclient -e '(claude/all-diagnostics "file.el")'

# Efficient structure analysis
emacsclient -e '(claude/efficient-diagnosis "file.el")'
```

### PowerShell Scripts

Multiple PowerShell scripts exist for configuration management (*.ps1 files). These are typically one-off configuration helpers and should be reviewed before execution.

## Architecture Principles

### Token Efficiency
All helper functions return structured plists optimized for parsing with minimal token usage. Never parse raw Elisp output when structured alternatives exist.

### Programmatic First
The codebase prioritizes programmatic access over interactive GUI tools. Use `emacsclient` with helper functions rather than manual Emacs commands.

### Multiple Validation Methods
Diagnostics use multiple approaches (syntax-ppss, indentation heuristics, forward-sexp scanning, stack-based type tracking) for cross-validation and higher confidence.

### Linting Over Manual Debugging
**Always use automated linting tools before manual debugging**:
1. `elint` for structural errors (missing parens, malformed forms)
2. `byte-compile` for semantic issues (undefined functions, wrong arg counts)
3. Diagnostic functions for detailed analysis

This approach saves 10-30 minutes per debugging session and provides precise error locations.

### Emacs Server Always Running
The Emacs server starts automatically (init.el:196-198). All operations should use `emacsclient` to interact with the running instance.

## Performance Optimizations

### eglot-booster
Installed for LSP performance boost. Uses I/O-only mode on Emacs 30+ since native JSON parsing is already fast.
Configuration: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:170-177`

### straight.el Package Manager
Uses straight.el for deterministic package management with Git-based installation.
Bootstrap: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:8-23`

## Documentation Files

Key reference docs in this repository:
- **PROGRAMMATIC-EMACS.md** - Comprehensive guide to emacsclient interaction patterns
- **PROGRAMMATIC-QUICKSTART.md** - Quick reference for programmatic access
- **LLM-PAREN-DIAGNOSTICS.md** - Guide to token-efficient paren diagnostics
- **MCP-GUIDE.md** - Complete MCP server configuration guide
- **CLAUDE-CODE-EMACS-GUIDE.md** - Claude Code integration setup

### Bracket Type Detection Documentation
- **BRACKET-TYPE-SUCCESS.md** - Success summary with implementation details and testing results
- **BRACKET-TYPE-TEST-RESULTS.md** - Comprehensive tool comparison across all bracket types
- **BRACKET-TYPE-ENHANCEMENT-COMPLETE.md** - Original completion summary and coverage analysis
- **THREE-DOCUMENT-SYNTHESIS.md** - Unified analysis comparing Defnet, Parinfer, and clojure-mcp approaches
- **demo-bracket-detection.el** - Interactive demonstration functions for bracket type detection

### Test Files for Bracket Diagnostics
Comprehensive test suite with 79+ intentional errors across all bracket types:
- **test-square-brackets.cljc** - 14 errors in `[]` brackets only
- **test-curly-braces.cljc** - 17 errors in `{}` brackets only (includes reader sets `#{}`)
- **test-type-mismatches.cljc** - 24 type mismatch errors (e.g., `[}`, `{]`, `()`)
- **test-all-brackets.cljc** - Combined stress test with 24+ mixed errors

These test files are useful for:
1. Validating diagnostic tools detect all error types
2. Benchmarking tool performance and coverage
3. Testing edge cases and cascading errors
4. Understanding different bracket error patterns

## Error Handling Best Practices

### Safe Evaluation
Always use `claude/eval-safe` for potentially risky operations:

```bash
emacsclient -e '(claude/eval-safe "(risky-operation)")'
# Returns: (:status "error"|"ok" :result ... :error-message ...)
```

### Checking Prerequisites
Before installing packages or making changes:
1. Check if already installed
2. Verify dependencies exist
3. Test with safe wrappers
4. Validate results

### Multi-Stage Diagnostics
For complex issues, use progressive diagnostic levels:
1. Quick summary (is there a problem?)
2. Minimal diagnostic (where is it?)
3. Detailed diagnostic (what exactly is wrong?)
4. Cross-validate with alternative methods

## UI Configuration

- Tool bar disabled: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:34`
- Menu bar disabled: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:35`
- Scroll bar disabled: `C:/Users/Apollo/AppData/Roaming/.emacs.d/init.el:37-38`
- UX enhancements loaded from: `ux-config.el`
