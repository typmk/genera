---
description: Query an Emacs configuration variable
args:
  - name: variable
    description: Name of the Emacs variable to query
    required: true
---

# Query Emacs Configuration Variable

Get the value and documentation for an Emacs configuration variable.

**Usage:** `/emacs-config variable-name`

```bash
emacsclient -e "(claude/get-config \"$ARGUMENTS\")"
```

This returns structured information including:
- Current value
- Variable type
- Documentation
- Source file (where it's defined)

**Examples:**
- `/emacs-config load-path` - See Emacs load path
- `/emacs-config user-emacs-directory` - Get .emacs.d location
- `/emacs-config cider-version` - Check CIDER version
- `/emacs-config eglot-server-programs` - See LSP server configuration
- `/emacs-config mcp-hub-servers` - Check MCP server configuration
