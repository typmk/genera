# Claude Code Emacs Integration Guide

## Installation Status
Successfully installed claude-code.el with all dependencies using straight.el package manager.

## Installed Components
- **straight.el** - Package manager that clones from Git repositories
- **inheritenv** - Environment variable inheritance (required dependency)
- **eat** - Terminal emulator (chosen for better Windows support)
- **claude-code.el** - Main integration package
- **transient** - Command interface framework (dependency)

## Important Configuration Notes
- **Claude CLI Command**: Configured to use `claude` (your system's command) instead of the default `claude-code`
- **Claude CLI Location**: `C:\Users\Apollo\.local\bin\claude.exe`

## Configuration Location
Your Emacs configuration file is located at:
```
C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el
```

## Starting Emacs

### GUI Mode
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe"
```

### Optional: Add to PATH
Consider adding `C:\Program Files\Emacs\emacs-30.2\bin` to your Windows PATH environment variable for easier access.

## Using Claude Code in Emacs

### Key Bindings
All claude-code commands are bound to the `C-c c` prefix (Control+c, then c).

After pressing `C-c c`, you'll see a transient menu with available commands:

**Main Commands:**
- `C-c c c` - Start Claude
- `C-c c s` - Send command/prompt
- `C-c c r` - Send region as context
- `C-c c t` - Toggle Claude Code window
- `C-c c b` - Switch to Claude Code buffer
- `C-c c m` - Show transient menu (displays all available commands)

**Mode Cycling:**
- `C-c c M` - Cycle between different Claude Code modes
- After first use, just press `M` to continue cycling (repeat-mode)

### Basic Workflow

1. **Start Emacs**
   ```bash
   "C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe"
   ```

2. **Start Claude Code**
   - Press `C-c c` to open the command menu
   - Select the start command

3. **Send Prompts**
   - Open a file or write in a buffer
   - Press `C-c c` and choose the appropriate command
   - Send your prompt to Claude

4. **View Output**
   - Claude's responses will appear in the eat terminal buffer
   - Navigate between buffers with `C-x b`

### Configuration Options

The configuration in `init.el` includes:

```elisp
;; Terminal backend (eat is configured for Windows compatibility)
(claude-code-terminal-backend 'eat)

;; IMPORTANT: Set the correct command for your system
(claude-code-program "claude")

;; Key prefix for all commands
("C-c c" . claude-code-command-map)

;; Mode cycling with repeat-mode
(:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))

;; Enabled globally
(claude-code-mode 1)
```

### Customization

To customize settings, you can edit your `init.el` file and modify the `:custom` section:

```elisp
:custom
(claude-code-terminal-backend 'eat)  ; Terminal type
;; Add more customizations here
```

Then restart Emacs or evaluate the buffer with `M-x eval-buffer`.

## Troubleshooting

### "spawning child process: invalid argument" Error
This error occurs when claude-code.el can't find or run the Claude CLI. The fix has been applied:
- Set `claude-code-program` to `"claude"` (your system's command name)
- This is already configured in your `init.el`

If you still get this error, verify:
1. `claude` command works in your terminal: run `claude --version`
2. The command is in your PATH
3. Restart Emacs after making configuration changes

### First Launch
On first launch, Emacs will:
- Download and install straight.el
- Clone all required packages from their repositories
- Build and configure each package

This may take a minute or two depending on your internet connection.

### Checking Installation
To verify claude-code is loaded:
1. Start Emacs
2. Press `M-x` (Alt+x)
3. Type `describe-mode` and press Enter
4. Look for claude-code-mode in the list

### Getting Help
- Press `C-h k` followed by any key binding to see what it does
- Press `C-h m` to see all active modes and their bindings
- Press `C-h f claude-code` then Tab to see all claude-code functions

## Additional Resources

- claude-code.el GitHub: https://github.com/stevemolitor/claude-code.el
- Emacs Manual: Press `C-h r` in Emacs
- straight.el docs: https://github.com/radian-software/straight.el

## Next Steps

1. Launch Emacs and wait for packages to initialize
2. Try `C-c c` to see the command menu
3. Explore the available commands
4. Start a Claude Code session and begin interacting with Claude

Enjoy using Claude Code in Emacs!
