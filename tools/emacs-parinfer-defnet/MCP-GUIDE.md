# mcp.el - Model Context Protocol for Emacs

## Installation Status
✅ Successfully installed mcp.el via straight.el

## What is MCP?
Model Context Protocol (MCP) is a protocol that allows AI assistants to connect to external tools and data sources. mcp.el brings this capability to Emacs, allowing you to:
- Connect to MCP servers (filesystem, web fetch, databases, etc.)
- Use tools from those servers within Emacs
- Integrate with AI clients like gptel or llm

## Installed Components
- **mcp.el** - Core MCP client
- **mcp-hub.el** - Server management interface
- **jsonrpc** - JSON-RPC communication (dependency)

## Location
- Built files: `C:\Users\Apollo\.emacs.d\straight\build\mcp\`
- Configuration: `C:\Users\Apollo\AppData\Roaming\.emacs.d\init.el` (lines 42-59)

## Available Commands

### Main Interface
- `M-x mcp-hub` - Opens the MCP hub interface (main control panel)

### Server Management
- `mcp-hub-start-server` - Start a specific MCP server
- `mcp-hub-start-all-server` - Start all configured servers
- `mcp-hub-close-server` - Stop a specific server
- `mcp-hub-close-all-server` - Stop all running servers
- `mcp-hub-restart-server` - Restart a specific server
- `mcp-hub-restart-all-server` - Restart all servers

### Monitoring & Configuration
- `mcp-hub-view-log` - View logs for a server
- `mcp-hub-view-roots` - View configured root directories
- `mcp-hub-add-root` - Add a root directory for filesystem access
- `mcp-hub-remove-root` - Remove a root directory

## Configuration

### Basic Setup (Already in your init.el)
The package is installed but not configured with any servers yet. To use it, you need to configure MCP servers.

### Example: Configure Filesystem Server

Uncomment and modify this section in your `init.el` (around line 50):

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em"
                                 "C:/Users/Apollo/Documents")))))
```

This configures a filesystem server that gives MCP tools access to specific directories.

### Example: Multiple Servers

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                         :args ("-y" "@modelcontextprotocol/server-filesystem")
                         :roots ("C:/Users/Apollo/em")))
        ("fetch" . (:command "uvx"
                    :args ("mcp-server-fetch")))
        ("brave-search" . (:command "npx"
                           :args ("-y" "@modelcontextprotocol/server-brave-search")
                           :env (("BRAVE_API_KEY" . "your-api-key"))))))
```

### Auto-start Servers

To automatically start all configured servers when Emacs starts, uncomment this line in your `init.el` (around line 58):

```elisp
(add-hook 'after-init-hook #'mcp-hub-start-all-server)
```

## Common MCP Servers

### Official MCP Servers (via npm/npx)

#### 1. Filesystem Server
```elisp
("filesystem" . (:command "npx"
                 :args ("-y" "@modelcontextprotocol/server-filesystem")
                 :roots ("C:/path/to/directory")))
```
**Requires**: Node.js/npm
**Purpose**: Provides file read/write access to specified directories

#### 2. Fetch Server
```elisp
("fetch" . (:command "uvx"
            :args ("mcp-server-fetch")))
```
**Requires**: Python with uvx
**Purpose**: Fetches web content

#### 3. Git Server
```elisp
("git" . (:command "npx"
          :args ("-y" "@modelcontextprotocol/server-git")))
```
**Purpose**: Git repository operations

#### 4. GitHub Server
```elisp
("github" . (:command "npx"
             :args ("-y" "@modelcontextprotocol/server-github")
             :env (("GITHUB_PERSONAL_ACCESS_TOKEN" . "your-token"))))
```
**Purpose**: GitHub API access

#### 5. Brave Search
```elisp
("brave-search" . (:command "npx"
                   :args ("-y" "@modelcontextprotocol/server-brave-search")
                   :env (("BRAVE_API_KEY" . "your-key"))))
```
**Purpose**: Web search via Brave API

#### 6. Postgres Server
```elisp
("postgres" . (:command "npx"
               :args ("-y" "@modelcontextprotocol/server-postgres")
               :env (("POSTGRES_CONNECTION_STRING" . "postgresql://..."))))
```
**Purpose**: PostgreSQL database access

## Prerequisites for MCP Servers

### Node.js (for npm/npx servers)
Check if installed:
```bash
node --version
npm --version
```

If not installed, download from: https://nodejs.org/

### Python with uv/uvx (for Python-based servers)
Check if installed:
```bash
python --version
uvx --version
```

Install uv:
```bash
pip install uv
```

## Usage Workflow

### 1. Configure Servers
Edit your `init.el` and add servers to `mcp-hub-servers`

### 2. Restart Emacs
Or reload config: `M-x eval-buffer` in init.el

### 3. Start MCP Hub
Press `M-x mcp-hub`

### 4. In the MCP Hub Interface
- Press `s` to start a server
- Press `k` to stop a server
- Press `r` to restart a server
- Press `l` to view logs
- Press `q` to quit

### 5. Use MCP Tools
Once servers are running, their tools become available to:
- AI integrations (gptel, llm)
- Direct function calls in Emacs

## Integration with Other Packages

### gptel (AI Chat)
mcp.el can provide tools to gptel for enhanced AI interactions:
```elisp
(use-package gptel
  :ensure t
  :after mcp
  :config
  ;; gptel will automatically discover MCP tools
  )
```

### llm (Large Language Models)
Similar integration with the llm package:
```elisp
(use-package llm
  :ensure t
  :after mcp)
```

## Troubleshooting

### Server Won't Start
1. Check if the command is available:
   - For npx: `npx --version`
   - For uvx: `uvx --version`
2. Check logs: `M-x mcp-hub-view-log`
3. Try running the command manually in a terminal

### "Command not found" errors
- Ensure Node.js and npm are installed and in PATH
- For Windows, you may need to restart Emacs after installing Node.js

### Server crashes immediately
- Check environment variables are set correctly
- Verify API keys are valid
- Check the server's specific requirements

## Example Complete Configuration

Here's a complete example for your init.el:

```elisp
;; Install mcp.el
(use-package mcp
  :straight (:host github
             :repo "lizqwerscott/mcp.el"
             :depth 1)
  :config
  ;; Configure MCP servers
  (setq mcp-hub-servers
        '(;; Filesystem access to your projects
          ("filesystem" . (:command "npx"
                           :args ("-y" "@modelcontextprotocol/server-filesystem")
                           :roots ("C:/Users/Apollo/em"
                                   "C:/Users/Apollo/Documents")))

          ;; Git operations
          ("git" . (:command "npx"
                    :args ("-y" "@modelcontextprotocol/server-git")))

          ;; Web fetching
          ("fetch" . (:command "uvx"
                      :args ("mcp-server-fetch")))))

  ;; Auto-start servers on Emacs startup
  (add-hook 'after-init-hook #'mcp-hub-start-all-server))
```

## Testing Your Setup

### 1. Check if mcp.el is loaded
```elisp
M-: (featurep 'mcp)
```
Should return `t`

### 2. Open MCP Hub
```
M-x mcp-hub
```

### 3. Start a server
Press `s` and select a configured server

### 4. Check server status
The hub should show the server as "running"

### 5. View logs
Press `l` to see server output and any errors

## Using emacsclient to Test

If you have Emacs server running (you do!), you can test from command line:

```bash
# Check if mcp is loaded
emacsclient --eval "(featurep 'mcp)"

# Check configured servers
emacsclient --eval "mcp-hub-servers"

# Start all servers
emacsclient --eval "(mcp-hub-start-all-server)"

# Check available commands
emacsclient --eval "(apropos-command \"^mcp-\")"
```

## Next Steps

1. **Install Node.js** if you haven't (for npx-based servers)
2. **Configure at least one server** in your init.el
3. **Restart Emacs** to load the configuration
4. **Run `M-x mcp-hub`** to manage servers
5. **Start a server** and check its logs

## Resources

- mcp.el GitHub: https://github.com/lizqwerscott/mcp.el
- MCP Protocol: https://modelcontextprotocol.io/
- Official MCP Servers: https://github.com/modelcontextprotocol/servers
- Claude Code + MCP: Allows Claude to use MCP tools from within Emacs

## Notes

- mcp.el requires Emacs 30+ ✅ (you have 30.2)
- MCP servers run as separate processes
- Each server requires its own runtime (Node.js, Python, etc.)
- Servers can be started/stopped individually
- Logs are available for debugging

Enjoy using MCP in Emacs!
