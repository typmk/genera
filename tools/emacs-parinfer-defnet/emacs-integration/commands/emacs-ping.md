# Test Emacs Server Connectivity

Test if the Emacs server is running and responsive.

```bash
emacsclient -e '(claude/ping)'
```

This returns connectivity status, Emacs version, and server uptime.
