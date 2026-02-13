# Installation Guide - Emacs Integration Plugin

Quick installation guide for the Emacs Integration plugin for Claude Code.

## Prerequisites Checklist

- [ ] Emacs ≥28.0 installed
- [ ] emacsclient accessible in PATH
- [ ] Claude Code CLI installed
- [ ] Emacs server running (or auto-starts on launch)

## Quick Install

### For Single Project

```bash
# Navigate to your project
cd /path/to/your/project

# Create plugins directory if it doesn't exist
mkdir -p .claude/plugins

# Copy the plugin
cp -r /path/to/emacs-integration .claude/plugins/

# Verify installation
claude /plugin list
```

### For All Projects (User-wide)

```bash
# Create user plugins directory
mkdir -p ~/.claude/plugins

# Copy the plugin
cp -r /path/to/emacs-integration ~/.claude/plugins/

# Plugin now available in all projects
```

## Emacs Configuration

### Minimal Required Setup

Add to your `init.el` (typically at `~/.emacs.d/init.el` or `C:/Users/USERNAME/AppData/Roaming/.emacs.d/init.el` on Windows):

```elisp
;; Start Emacs server automatically
(require 'server)
(unless (server-running-p)
  (server-start))

;; Load helper functions for Claude integration
(load-file "C:/Users/Apollo/em/claude-helpers.el")
(load-file "C:/Users/Apollo/em/claude-paren-diagnostics.el")
```

**Note:** Adjust paths to match your actual file locations.

### Full Recommended Setup

If you don't have the helper files yet, here's how to get them:

1. **Get the helper files from the repository:**
   ```bash
   # Copy from your em directory (adjust path as needed)
   cp C:/Users/Apollo/em/claude-helpers.el ~/.emacs.d/
   cp C:/Users/Apollo/em/claude-paren-diagnostics.el ~/.emacs.d/
   ```

2. **Update init.el with correct paths:**
   ```elisp
   ;; Start server
   (require 'server)
   (unless (server-running-p)
     (server-start))

   ;; Load Claude integration helpers
   (load-file (expand-file-name "claude-helpers.el" user-emacs-directory))
   (load-file (expand-file-name "claude-paren-diagnostics.el" user-emacs-directory))
   ```

3. **Reload Emacs config:**
   - Restart Emacs, OR
   - `M-x eval-buffer` in your init.el, OR
   - `M-x load-file RET ~/.emacs.d/init.el RET`

## Verification

### 1. Check Plugin Installation

```bash
claude /plugin list
```

Should show `emacs-integration` in the list.

### 2. Verify Emacs Server

```bash
claude /emacs-ping
```

Should return:
```
(:status "ok" :emacs-version "30.2" :uptime ...)
```

### 3. Test Helper Functions

```bash
claude /emacs-eval (fboundp 'claude/minimal-diagnostic)
```

Should return `t` (true).

### 4. Run Test Diagnostic

```bash
# Create a test file with an error
echo "(defun test ()" > /tmp/test.el

# Run diagnostic
claude /emacs-diag /tmp/test.el
```

Should detect the unmatched opening paren.

## Troubleshooting

### Issue: "Emacs server not responding"

**Solution 1: Check if Emacs is running**
```bash
# Windows
tasklist | findstr emacs

# Linux/Mac
ps aux | grep emacs
```

**Solution 2: Start server manually**
In Emacs:
```
M-x server-start
```

Or from command line:
```bash
emacs --daemon
```

**Solution 3: Check server socket location**
```elisp
;; In Emacs, check:
(server-running-p)  ; Should return t

;; Check server file location
server-socket-dir   ; Should point to valid directory
```

### Issue: "Diagnostic helpers not found"

**Check if files are loaded:**
```bash
claude /emacs-eval (fboundp 'claude/minimal-diagnostic)
```

If returns `nil`, the helper files aren't loaded.

**Solutions:**
1. Verify file paths in init.el are correct
2. Check files exist at those paths
3. Reload init.el: `M-x load-file RET ~/.emacs.d/init.el RET`
4. Check for errors in `*Messages*` buffer

### Issue: "Symbol's function definition is void"

This means Emacs is running but helper functions aren't defined.

**Solution:**
```elisp
;; In Emacs, manually load:
(load-file "C:/path/to/claude-helpers.el")
(load-file "C:/path/to/claude-paren-diagnostics.el")

;; Verify:
(fboundp 'claude/ping)  ; Should return t
```

### Issue: Commands not found

**Verify plugin directory structure:**
```bash
# Should show these files:
ls -la .claude/plugins/emacs-integration/
# .claude-plugin/plugin.json
# skills/
# agents/
# commands/
# hooks/
```

**Re-copy if structure is wrong:**
```bash
rm -rf .claude/plugins/emacs-integration
cp -r /path/to/source/emacs-integration .claude/plugins/
```

### Issue: Hooks not triggering

Hooks require proper JSON syntax and permissions.

**Test hook manually:**
```bash
# Should check connectivity
bash -c 'emacsclient -e "(claude/ping)"'
```

**Check hooks.json syntax:**
```bash
# Validate JSON
python -m json.tool .claude/plugins/emacs-integration/hooks/hooks.json
```

## Platform-Specific Notes

### Windows

**Path Separators:**
- In Elisp: Use forward slashes `C:/Users/Apollo/file.el`
- In PowerShell/CMD: Either works, but forward slashes are safer

**emacsclient Location:**
Ensure emacsclient is in PATH, or use full path in commands:
```bash
"C:/Program Files/Emacs/emacs-30.2/bin/emacsclient.exe"
```

**Line Endings:**
Ensure helper files use Unix line endings (LF, not CRLF) if you encounter issues.

### Linux/Mac

**Socket Permissions:**
If server won't start, check socket directory permissions:
```bash
ls -la ~/.emacs.d/server/
chmod 700 ~/.emacs.d/server/
```

**Alternative Installation Paths:**
```bash
# System-wide (requires sudo)
sudo cp -r emacs-integration /usr/local/share/claude-code/plugins/

# XDG-compliant
cp -r emacs-integration ~/.local/share/claude-code/plugins/
```

## Post-Installation

### Enable Skills

Skills should be automatically available. Test by asking Claude:

```
"Check if I have paren errors in my init file"
```

Claude should automatically invoke the `emacs-diagnostics` skill.

### Configure Hooks

Hooks are enabled by default. To disable:

1. Open `.claude/plugins/emacs-integration/hooks/hooks.json`
2. Remove or comment out unwanted hooks
3. Restart Claude Code session

### Customize Commands

Command files are in `commands/`. Edit them to customize behavior:

```bash
# Edit a command
nano .claude/plugins/emacs-integration/commands/emacs-ping.md

# Changes take effect immediately
```

## Next Steps

- Read [README.md](./README.md) for full feature documentation
- Try `/emacs-ping` to verify everything works
- Run `/emacs-diag` on a test file
- Ask Claude questions that trigger skills automatically
- Review [CLAUDE.md](../CLAUDE.md) for integration patterns

## Getting Help

If issues persist:

1. Check helper files are correctly loaded:
   ```bash
   claude /emacs-eval (mapcar 'symbol-name '(claude/ping claude/minimal-diagnostic claude/get-config))
   ```

2. Verify plugin structure:
   ```bash
   find .claude/plugins/emacs-integration -type f
   ```

3. Test emacsclient directly:
   ```bash
   emacsclient -e '(+ 1 2 3)'  # Should return 6
   ```

4. Check Emacs `*Messages*` buffer for errors:
   ```bash
   claude /emacs-eval (with-current-buffer "*Messages*" (buffer-substring-no-properties (max 1 (- (point-max) 500)) (point-max)))
   ```

## Uninstallation

To remove the plugin:

```bash
# Project-level
rm -rf .claude/plugins/emacs-integration

# User-level
rm -rf ~/.claude/plugins/emacs-integration
```

Plugin will no longer be available after restarting Claude Code.
