# Methods for Interacting with Emacs for Testing & Debugging

## Overview
There are several ways to interact with a running Emacs instance for testing, debugging, and controlling claude-code.el.

## 1. Emacs Server/Client Mode (RECOMMENDED)

### How It Works
- Start Emacs with a server running
- Use `emacsclient` to send commands to the running Emacs instance
- This allows real-time interaction without restarting Emacs

### Setup

#### Option A: Auto-start server in init.el
Add to your `init.el`:
```elisp
;; Start Emacs server automatically
(require 'server)
(unless (server-running-p)
  (server-start))
```

#### Option B: Start server manually
In running Emacs, press `M-x` and type `server-start`

### Usage Examples

#### Evaluate Lisp expressions:
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "(+ 1 2)"
```

#### Check if claude-code is loaded:
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "(featurep 'claude-code)"
```

#### Get variable values:
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "claude-code-program"
```

#### Check if claude-code-mode is active:
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "claude-code-mode"
```

#### Execute complex commands:
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe" --eval "(progn (require 'claude-code) (message \"Loaded successfully\"))"
```

### Benefits
- No need to restart Emacs
- Real-time interaction with running instance
- Can inspect and modify state on the fly
- Can trigger claude-code commands programmatically

## 2. Batch Mode Evaluation (CURRENT METHOD)

### How It Works
- Starts fresh Emacs instance
- Loads configuration
- Evaluates expression
- Exits

### Usage
```bash
"C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe" --batch --load "C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el" --eval "(message \"Test: %s\" (featurep 'claude-code))"
```

### Benefits
- Clean state every time
- Good for testing configuration loading
- No interference from existing sessions

### Drawbacks
- Slower (starts new process each time)
- Can't interact with running GUI state
- Doesn't test interactive behavior

## 3. IELM (Interactive Emacs Lisp Mode) - Built-in REPL

### How It Works
- Built-in REPL inside Emacs
- Like a Lisp scratch pad

### Usage
1. Inside Emacs, press `M-x ielm`
2. You get an interactive Lisp prompt
3. Type expressions and see results immediately

### Example Session
```elisp
ELISP> (+ 1 2)
3
ELISP> (featurep 'claude-code)
t
ELISP> claude-code-program
"claude"
ELISP> (describe-function 'claude-code-mode)
; Opens documentation
```

### Benefits
- Interactive exploration
- Tab completion
- Access to full Emacs state
- Great for prototyping

## 4. *scratch* Buffer

### How It Works
- Default buffer in Emacs for Lisp evaluation
- Press `C-j` at end of expression to evaluate

### Usage
1. Switch to `*scratch*` buffer (usually open by default)
2. Type Lisp expression
3. Put cursor at end and press `C-j`

### Example
```elisp
;; Type these lines and press C-j after each:
(+ 1 2)
;; => 3

(featurep 'claude-code)
;; => t

claude-code-program
;; => "claude"
```

## 5. edebug (Emacs Debugger)

### How It Works
- Step-through debugger for Elisp
- Set breakpoints, inspect variables, step through code

### Usage
1. Open claude-code.el source file
2. Navigate to a function
3. Press `C-u C-M-x` to instrument function for debugging
4. Run the function normally
5. Debugger will activate

### Commands in edebug
- `SPC` - step through
- `n` - next
- `c` - continue
- `e` - evaluate expression
- `q` - quit debugger

### Benefits
- Full debugging capabilities
- Inspect variable state at each step
- Can modify values during execution

## 6. *Messages* Buffer Monitoring

### How It Works
- All `message` calls appear in `*Messages*` buffer
- Add debug logging to claude-code.el

### Usage
Add logging to functions:
```elisp
(message "DEBUG: claude-code-program is %s" claude-code-program)
```

Then check `*Messages*` buffer with `C-h e`

## 7. trace-function

### How It Works
- Trace function calls and their arguments/returns

### Usage
```elisp
M-x trace-function RET claude-code-start RET
```

Now every call to `claude-code-start` will be logged.

### Stop tracing:
```elisp
M-x untrace-function RET claude-code-start RET
```

## 8. Remote Debugging via Socket REPL

### Advanced: Could set up a socket REPL server
```elisp
;; In init.el
(setq server-use-tcp t)
(setq server-port 9999)
(server-start)
```

Then connect via TCP socket for programmatic control.

## Recommended Workflow for Claude Code Integration Testing

### Setup Phase (One Time)
1. Add server-start to init.el
2. Start Emacs GUI normally
3. Server runs in background

### Testing Phase (Repeated)
```bash
# Check if running
emacsclient --eval "(server-running-p)"

# Load/reload claude-code
emacsclient --eval "(require 'claude-code t)"

# Check configuration
emacsclient --eval "claude-code-program"

# Test if command exists
emacsclient --eval "(fboundp 'claude-code-start)"

# Check mode status
emacsclient --eval "claude-code-mode"

# Get buffer list
emacsclient --eval "(mapcar 'buffer-name (buffer-list))"
```

### Interactive Debugging
Use IELM or *scratch* buffer inside Emacs for quick tests.

### Deep Debugging
Use edebug to step through claude-code.el functions.

## Best Method for Your Use Case

**For automated testing/verification**: Emacs Server + emacsclient
**For interactive exploration**: IELM or *scratch* buffer
**For deep debugging**: edebug
**For simple logging**: *Messages* buffer + message calls
**For clean-slate testing**: Batch mode (current method)

## Example: Complete Test Script Using emacsclient

```bash
#!/bin/bash
EMACSCLIENT="C:\Program Files\Emacs\emacs-30.2\bin\emacsclient.exe"

echo "Testing Claude Code Integration..."

# Check if server is running
if ! $EMACSCLIENT --eval "(server-running-p)" 2>/dev/null; then
    echo "Error: Emacs server not running. Start Emacs first."
    exit 1
fi

# Test 1: Check if claude-code is loaded
echo "Test 1: Checking if claude-code is loaded..."
$EMACSCLIENT --eval "(featurep 'claude-code)"

# Test 2: Check program name
echo "Test 2: Checking claude-code-program..."
$EMACSCLIENT --eval "claude-code-program"

# Test 3: Check if mode is enabled
echo "Test 3: Checking if mode is enabled..."
$EMACSCLIENT --eval "claude-code-mode"

# Test 4: List available commands
echo "Test 4: Listing claude-code commands..."
$EMACSCLIENT --eval "(apropos-command \"^claude-code-\")"

echo "Tests complete!"
```

## Notes for Windows

- Use full paths to emacsclient.exe
- Quote paths with spaces
- Consider creating aliases or adding to PATH
- Use `emacsclientw.exe` for GUI-less operation
