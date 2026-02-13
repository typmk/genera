# mcp.el Quick Start Guide

## Installation Status
✅ **INSTALLED** - mcp.el is ready to use!

## What You Need to Do Next

### Step 1: Install Prerequisites (if needed)

#### For npm-based MCP servers (most common):
```bash
# Check if Node.js is installed
node --version
npm --version

# If not, download and install from: https://nodejs.org/
```

#### For Python-based MCP servers:
```bash
# Check if Python and uvx are installed
python --version
uvx --version

# Install uv if needed
pip install uv
```

### Step 2: Configure Your First MCP Server

Edit `C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el` around line 50.

**Uncomment and modify this section:**

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em")))))
```

Change `"C:/Users/Apollo/em"` to any directories you want MCP to access.

### Step 3: Restart Emacs
Close and reopen Emacs to load the configuration.

### Step 4: Start MCP Hub
In Emacs:
```
M-x mcp-hub
```

### Step 5: Start Your Server
In the MCP hub window:
- Press `s` to start a server
- Select "filesystem" from the list
- Wait for it to show as "running"

## Quick Command Reference

| Key | Command | Description |
|-----|---------|-------------|
| `M-x mcp-hub` | Open hub | Main control panel |
| `s` | Start | Start selected server |
| `k` | Kill | Stop selected server |
| `r` | Restart | Restart selected server |
| `l` | Logs | View server logs |
| `q` | Quit | Close hub window |

## Example Configurations

### Minimal (Filesystem only)
```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em"))))))
```

### Multi-Server Setup
```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em"
                                 "C:/Users/Apollo/Documents")))
        ("git" . (:command "npx"
                  :args ("-y" "@modelcontextprotocol/server-git")))
        ("fetch" . (:command "uvx"
                    :args ("mcp-server-fetch")))))
```

### Auto-start on Emacs Launch
Add this to your config (currently commented out at line 58):
```elisp
(add-hook 'after-init-hook #'mcp-hub-start-all-server)
```

## Testing from Command Line

```bash
# Check if mcp is loaded
emacsclient --eval "(featurep 'mcp)"

# Check configured servers
emacsclient --eval "mcp-hub-servers"

# List available commands
emacsclient --eval "(apropos-command \"^mcp-\")"
```

## Troubleshooting

### "Command not found: npx"
- Install Node.js from https://nodejs.org/
- Restart Emacs after installation

### "Command not found: uvx"
```bash
pip install uv
```

### Server won't start
1. Check logs: Press `l` in mcp-hub
2. Verify the command works in terminal:
   ```bash
   npx -y @modelcontextprotocol/server-filesystem
   ```
3. Check that Node.js is in your PATH

### Server starts but crashes
- Check logs for specific errors
- Verify environment variables (API keys, etc.)
- Make sure specified directories exist (for filesystem server)

## What MCP Does

MCP servers provide **tools** that can be used by:
- AI assistants (like Claude via claude-code.el)
- Other Emacs packages (gptel, llm)
- Direct function calls

Example tools from filesystem server:
- Read files
- Write files
- List directories
- Search files

## Popular MCP Servers

| Server | Command | Purpose |
|--------|---------|---------|
| filesystem | npx @modelcontextprotocol/server-filesystem | File operations |
| git | npx @modelcontextprotocol/server-git | Git operations |
| github | npx @modelcontextprotocol/server-github | GitHub API |
| fetch | uvx mcp-server-fetch | Web fetching |
| brave-search | npx @modelcontextprotocol/server-brave-search | Web search |
| postgres | npx @modelcontextprotocol/server-postgres | Database access |

## Files Location

- **Configuration**: `~/.emacs.d/init.el` (lines 42-59)
- **Installed files**: `~/.emacs.d/straight/build/mcp/`
- **Documentation**: `C:\Users\Apollo\em\MCP-GUIDE.md`
- **This file**: `C:\Users\Apollo\em\MCP-QUICKSTART.md`

## Complete Workflow

1. ✅ **Install mcp.el** (DONE)
2. ⬜ **Install Node.js** (if needed)
3. ⬜ **Configure servers** in init.el
4. ⬜ **Restart Emacs**
5. ⬜ **Run `M-x mcp-hub`**
6. ⬜ **Start servers** (press `s`)
7. ⬜ **Use MCP tools** in your workflow

## Getting Help

- View full guide: `MCP-GUIDE.md`
- Check mcp.el repo: https://github.com/lizqwerscott/mcp.el
- MCP protocol docs: https://modelcontextprotocol.io/

## Integration with claude-code.el

Once MCP servers are running, Claude Code (via claude-code.el) can potentially use these tools when you interact with it in Emacs. This creates a powerful workflow where Claude can:
- Access your files (filesystem server)
- Search the web (fetch/brave-search servers)
- Work with git repositories (git server)
- Query databases (postgres server)
- And more!

All from within Emacs! 🚀
