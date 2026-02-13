# Testing & Debugging Claude Code in Emacs

## Quick Start

### For You (User)
1. Start Emacs: `"C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe"`
2. Server starts automatically (configured in init.el)
3. Run test script: `./test-claude-code.bat` (Windows) or `bash test-claude-code.sh`

### For Me (Claude Code Assistant)
Once your Emacs is running with server, I can interact with it in real-time using:

```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "<elisp-expression>"
```

## What I Can Now Do

### 1. Check Configuration in Real-Time
```bash
# Check if claude-code loaded
emacsclient --eval "(featurep 'claude-code)"

# Check settings
emacsclient --eval "claude-code-program"
emacsclient --eval "claude-code-terminal-backend"

# Check if mode is active
emacsclient --eval "claude-code-mode"
```

### 2. Debug Issues Interactively
```bash
# Check if functions exist
emacsclient --eval "(fboundp 'claude-code-start)"

# Check keybindings
emacsclient --eval "(key-binding (kbd \"C-c c\"))"

# List all buffers (to see if claude-code started)
emacsclient --eval "(mapcar 'buffer-name (buffer-list))"
```

### 3. Test Changes Without Restart
```bash
# Reload package
emacsclient --eval "(require 'claude-code t)"

# Temporarily change settings
emacsclient --eval "(setq claude-code-program \"new-value\")"

# Test if a command would work
emacsclient --eval "(executable-find \"claude\")"
```

### 4. Send Debug Messages
```bash
# This appears in your *Messages* buffer
emacsclient --eval "(message \"Debug: Test from Claude Code\")"
```

### 5. Trigger Commands
```bash
# Could even trigger claude-code functions
emacsclient --eval "(call-interactively 'claude-code-start)"
```

## Files Created

### Documentation
- **EMACS-DEBUG-METHODS.md** - Complete guide to all debugging methods
- **README-TESTING.md** - This file

### Test Scripts
- **test-claude-code.bat** - Windows batch script for testing
- **test-claude-code.sh** - Bash script for testing (Git Bash/WSL)
- **emacsclient-examples.sh** - Examples of emacsclient usage

### Configuration
- **~/.emacs.d/init.el** - Updated with server-start

## Testing Workflow

### Initial Setup (One Time)
1. ✅ Emacs 30.2 installed
2. ✅ init.el configured with claude-code.el
3. ✅ Server-start added to init.el
4. ✅ claude-code-program set to "claude"

### Every Session
1. You start Emacs GUI
2. Server starts automatically
3. I can now use emacsclient to:
   - Check status
   - Debug issues
   - Test changes
   - Monitor state

### Debugging Process
When you encounter an issue:

1. **You tell me the error**
2. **I can investigate with emacsclient**:
   ```bash
   # Check what went wrong
   emacsclient --eval "(get-buffer \"*Messages*\")"

   # Check configuration
   emacsclient --eval "claude-code-program"

   # Verify command exists
   emacsclient --eval "(executable-find \"claude\")"
   ```
3. **I can test fixes immediately**:
   ```bash
   # Try a different setting
   emacsclient --eval "(setq claude-code-program \"claude.exe\")"
   ```
4. **If fix works, update init.el permanently**

## Comparison: Before vs. After

### Before (Batch Mode Only)
- ❌ Had to restart Emacs for every test
- ❌ Slow iteration cycle
- ❌ Couldn't inspect running state
- ❌ No interaction with GUI state
- ✅ Clean slate every time

### After (With Server/Client)
- ✅ Real-time interaction
- ✅ Fast iteration
- ✅ Can inspect running state
- ✅ Can test without restart
- ✅ Can modify state on the fly
- ✅ Can trigger commands programmatically
- ✅ Still have batch mode when needed

## Other Methods Available

### Built into Emacs (You Use These)
1. **IELM** - `M-x ielm` - Interactive REPL
2. ***scratch*** - Default buffer, press `C-j` to evaluate
3. **edebug** - `C-u C-M-x` on function to instrument for debugging
4. ***Messages*** - `C-h e` to view debug messages
5. **describe-function** - `C-h f` to see function documentation
6. **describe-variable** - `C-h v` to see variable value and docs

### I Can Use (External)
1. **emacsclient** - Primary method (as described above)
2. **Batch mode** - For clean-slate testing
3. **File inspection** - Read source files directly

## Example: Debugging the "spawning child process" Error

### How We Solved It Before
1. You reported error
2. I guessed possible causes
3. Updated init.el
4. You restarted Emacs
5. Tested
6. Repeat if wrong

### How We Could Solve It Now
1. You report error
2. I check: `emacsclient --eval "claude-code-program"` → sees "claude-code"
3. I check: `emacsclient --eval "(executable-find \"claude-code\")"` → sees nil
4. I check: `emacsclient --eval "(executable-find \"claude\")"` → sees path
5. I test fix: `emacsclient --eval "(setq claude-code-program \"claude\")"`
6. You test if it works
7. I update init.el with permanent fix

Much faster! ⚡

## Security Note

The Emacs server (by default) only accepts connections from:
- Same user account
- Same machine
- Via secure socket file

So it's safe to leave running.

## Commands Reference Card

```bash
# Path to emacsclient
EMACSCLIENT="C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe"

# Check if server running
"$EMACSCLIENT" --eval "(server-running-p)"

# Evaluate expression
"$EMACSCLIENT" --eval "(+ 1 2)"

# Get variable
"$EMACSCLIENT" --eval "variable-name"

# Set variable
"$EMACSCLIENT" --eval "(setq variable-name value)"

# Call function
"$EMACSCLIENT" --eval "(function-name arg1 arg2)"

# Check if function exists
"$EMACSCLIENT" --eval "(fboundp 'function-name)"

# Check if feature loaded
"$EMACSCLIENT" --eval "(featurep 'feature-name)"

# Send message (visible in *Messages*)
"$EMACSCLIENT" --eval "(message \"text\")"

# Load/reload package
"$EMACSCLIENT" --eval "(require 'package-name t)"
```

## Next Steps

1. **Start Emacs** to activate the server
2. **Run test script** to verify everything works
3. **Try C-c c c** in Emacs to start Claude Code
4. **If issues occur**, I can now debug interactively!

## Files Summary

| File | Purpose |
|------|---------|
| `~/.emacs.d/init.el` | Main Emacs configuration |
| `CLAUDE-CODE-EMACS-GUIDE.md` | User guide for claude-code.el |
| `EMACS-DEBUG-METHODS.md` | Complete debugging reference |
| `README-TESTING.md` | This file - testing overview |
| `test-claude-code.bat` | Windows test script |
| `test-claude-code.sh` | Bash test script |
| `emacsclient-examples.sh` | Example commands |

All files are in: `C:\Users\Apollo\em\`
