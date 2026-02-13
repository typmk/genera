# Emacs Integration Plugin for Claude Code

Seamless Emacs integration for Claude Code with token-efficient diagnostics, REPL interaction, and programmatic control via `emacsclient`.

## Features

### 🎯 Auto-Invoked Skills
Claude automatically uses these capabilities when relevant:

- **emacs-diagnostics** - Detects and diagnoses Lisp paren/bracket errors with type-aware detection (95%+ token savings, supports (), [], {})
- **emacs-config-query** - Queries Emacs configuration and package status
- **emacs-buffer-ops** - Inspects buffers, REPLs, and Emacs state

### 🤖 Specialized Subagent
- **emacs-debugger** - Isolated context for complex multi-step debugging sessions (resumable)

### ⚡ Quick Commands
- `/emacs-ping` - Test Emacs server connectivity
- `/emacs-eval <expr>` - Safely evaluate Emacs Lisp expressions
- `/emacs-diag <file>` - Run comprehensive diagnostics on Lisp files
- `/emacs-config <var>` - Query configuration variables
- `/emacs-repl [type]` - Show REPL output (CIDER, SLIME, ielm)

### 🔒 Safety Hooks
- **PreToolUse** - Auto-check Lisp files for paren errors before editing
- **SessionStart** - Verify Emacs connectivity and helper availability

## Prerequisites

### Required
- **Emacs** ≥28.0 with server running
- **emacsclient** accessible in PATH
- **Claude Code** CLI

### Recommended Emacs Packages
- `claude-helpers.el` - Structured query functions
- `claude-paren-diagnostics.el` - Token-efficient diagnostic tools
- `bracket-type-detection-clean.el` - Type-aware bracket diagnostics
- `defnet.el` - **Defnet IDE integration** (code intelligence, call graphs, trust scores)

## Defnet IDE Integration

The `defnet.el` package provides deep integration with the Defnet code intelligence server:

### Features
- **Find callers/callees** of any function
- **Semantic code search** across codebase
- **Trust score visualization**
- **Impact analysis** before refactoring
- **Clerk notebook integration** for call graphs and heatmaps

### Setup

1. Start Defnet IDE: `clj -M:ide` (in defnet project)
2. Load defnet.el:
   ```elisp
   (load-file "C:/Users/Apollo/em/emacs-integration/defnet.el")
   ```
3. Enable mode: `M-x defnet-mode` (auto-enables in Clojure buffers)

### Key Bindings (C-c d prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `c` | find-callers | Who calls this function? |
| `C` | called-by | What does this function call? |
| `l` | locate-function | Jump to definition |
| `u` | find-usages | All usages in codebase |
| `/` | search-code | Semantic search |
| `e` | explain-function | AI explanation |
| `i` | impact-analysis | What breaks if changed? |
| `d` | dashboard | Open Clerk dashboard |
| `g` | call-graph | Interactive call graph |
| `T` | trust-heatmap | Trust score visualization |
| `m` | menu | Transient menu |

### Transient Menu

If `transient` is installed, `C-c d m` opens a hydra-like menu for all commands.

## Installation

### Option 1: Local Development Install

```bash
# Clone or copy the plugin to your project
cp -r emacs-integration /path/to/your/project/.claude/plugins/

# Or for user-wide installation
cp -r emacs-integration ~/.claude/plugins/
```

### Option 2: Add to Claude Code Plugin Marketplace (Future)

Once you publish this to a marketplace:

```bash
/plugin install emacs-integration
```

## Configuration

### 1. Ensure Emacs Server is Running

Add to your `init.el`:

```elisp
;; Start Emacs server automatically
(require 'server)
(unless (server-running-p)
  (server-start))
```

### 2. Load Helper Functions

Required for full functionality:

```elisp
;; Load helper libraries
(load-file "C:/Users/Apollo/em/claude-helpers.el")
(load-file "C:/Users/Apollo/em/claude-paren-diagnostics.el")
(load-file "C:/Users/Apollo/em/bracket-type-detection-clean.el")  ; NEW: Type-aware bracket detection
```

### 3. Enable Plugin in Claude Code

If installed locally, the plugin will be automatically available in your project.

To verify:
```bash
/plugin list
```

## Usage Examples

### Automatic Skill Invocation

Claude automatically uses skills when relevant:

```
User: "I'm getting unmatched paren error in my config"

Claude: [Automatically invokes emacs-diagnostics skill]
       Uses: emacsclient -e '(claude/minimal-diagnostic "init.el")'
       Result: Found unmatched ( at line 42, column 15
```

### Explicit Commands

```bash
# Test connectivity
/emacs-ping

# Evaluate Elisp
/emacs-eval (+ 1 2 3)
/emacs-eval (featurep 'cider)

# Diagnose file
/emacs-diag src/core.clj

# Query config
/emacs-config load-path
/emacs-config cider-version

# Check REPL
/emacs-repl cider
```

### Complex Debugging with Subagent

```
User: "Help me debug this complex multi-file Clojure issue"

Claude: I'll delegate to the emacs-debugger subagent for deep analysis.
       [Launches subagent with isolated context]
       [Subagent runs 20+ diagnostic commands]
       [Returns summary to main conversation]
```

### Hooks in Action

When you try to edit a Lisp file:

```
PreToolUse Hook: Checking parens in file.el...
⚠️  Warning: Parenthesis errors detected. Run /emacs-diag for details.
```

On session start:

```
✓ Emacs server connected
✓ Emacs diagnostic helpers loaded
```

## Architecture

```
emacs-integration/
├── .claude-plugin/
│   └── plugin.json              # Plugin metadata
│
├── skills/                      # Auto-invoked by Claude
│   ├── emacs-diagnostics/
│   │   └── SKILL.md            # Paren/bracket diagnostics
│   ├── emacs-config-query/
│   │   └── SKILL.md            # Config queries
│   └── emacs-buffer-ops/
│       └── SKILL.md            # Buffer operations
│
├── agents/                      # Specialized subagents
│   └── emacs-debugger.md       # Deep debugging specialist
│
├── commands/                    # Slash commands
│   ├── emacs-ping.md
│   ├── emacs-eval.md
│   ├── emacs-diag.md
│   ├── emacs-config.md
│   └── emacs-repl.md
│
└── hooks/                       # Event-driven automation
    └── hooks.json              # PreToolUse, SessionStart hooks
```

## How It Works

### Token Efficiency
Traditional approach to debugging parens:
```
Read entire file: ~5000 tokens
Claude analyzes: ~10000 tokens
Total: ~15000 tokens
```

With emacs-diagnostics skill:
```
Quick diagnostic: ~20 tokens
Minimal diagnostic: ~30 tokens
Total: ~50 tokens (99%+ savings!)
```

### Programmatic Control
All interactions use structured helper functions that return parseable plists:

```elisp
;; Instead of raw eval
(some-variable)  ; Returns: raw output, hard to parse

;; Use structured helpers
(claude/get-config "some-variable")
;; Returns: (:variable "some-variable" :value "..." :type 'string :documentation "...")
```

### Multi-Method Diagnostics
For high-confidence error detection, cross-validates using:
- `syntax-ppss` - Emacs's parser state
- Indentation heuristics - Structural pattern analysis
- `forward-sexp` scanning - Delimiter balancing
- Checkpoint-based analysis - Incremental validation

## Language Support

- **Emacs Lisp** (.el) - Full support with all diagnostics
- **Clojure** (.clj, .cljs, .cljc) - Full support + CIDER REPL
- **Common Lisp** (.lisp, .lsp) - Full support + SLIME REPL

## Windows Considerations

### Path Formats
- **emacsclient**: Use forward slashes: `C:/Users/Apollo/file.el`
- **bash**: Use backslashes or quotes: `"C:\\Users\\Apollo\\file.el"`

The plugin automatically converts paths where needed.

### Binary Locations
Default paths (customize if different):
- Emacs: `C:/Program Files/Emacs/emacs-30.2/bin/emacs.exe`
- emacsclient: `C:/Program Files/Emacs/emacs-30.2/bin/emacsclient.exe`

## Troubleshooting

### "Emacs server not responding"
```bash
# Check if Emacs is running
tasklist | findstr emacs

# Start Emacs server
emacs --daemon

# Or in running Emacs
M-x server-start
```

### "Diagnostic helpers not found"
Load the helper libraries in your `init.el`:
```elisp
(load-file "path/to/claude-helpers.el")
(load-file "path/to/claude-paren-diagnostics.el")
```

### Commands fail with path errors
Ensure paths use forward slashes for Emacs:
```bash
# Good
/emacs-diag C:/Users/Apollo/file.el

# May fail
/emacs-diag C:\Users\Apollo\file.el
```

## Performance

| Operation | Traditional | With Plugin | Savings |
|-----------|-------------|-------------|---------|
| Paren diagnosis | ~15000 tokens | ~50 tokens | 99.7% |
| Config query | ~1000 tokens | ~20 tokens | 98% |
| Buffer inspection | ~5000 tokens | ~50 tokens | 99% |
| REPL check | ~3000 tokens | ~100 tokens | 97% |

## Development

### Adding New Skills

Create a new skill directory:
```bash
mkdir -p emacs-integration/skills/my-skill
```

Create `SKILL.md` with frontmatter:
```yaml
---
name: my-skill
description: What this skill does and when to use it
allowed-tools:
  - Bash
  - Read
---

# Skill instructions here
```

### Adding New Commands

Create `.md` file in `commands/`:
```bash
echo "# My Command\n\nDoes something cool." > emacs-integration/commands/my-cmd.md
```

Use `/my-cmd` to invoke.

### Testing Locally

```bash
# Copy to project
cp -r emacs-integration .claude/plugins/

# Test commands
/emacs-ping

# Test skills (ask questions that should trigger them)
"Check if I have paren errors in init.el"
```

## Contributing

This plugin is part of the [your repository]. Contributions welcome!

### Ideas for Future Enhancements
- MCP server wrapper for standardized tool access
- Integration with LSP (eglot/lsp-mode) diagnostics
- REPL evaluation history tracking
- Package installation helpers
- Custom major mode support
- Org-mode integration
- Magit integration for git operations from Claude

## License

MIT

## Credits

Built for the Claude Code + Emacs integration workflow.
Uses token-efficient diagnostic techniques and programmatic Emacs control patterns.

## See Also

- [CLAUDE.md](../CLAUDE.md) - Repository instructions for Claude Code
- [PROGRAMMATIC-EMACS.md](../PROGRAMMATIC-EMACS.md) - Emacsclient interaction guide
- [LLM-PAREN-DIAGNOSTICS.md](../LLM-PAREN-DIAGNOSTICS.md) - Diagnostic function details
- [claude-helpers.el](../claude-helpers.el) - Helper function library
- [claude-paren-diagnostics.el](../claude-paren-diagnostics.el) - Diagnostic tools
