# parinfer.el - Pure Emacs Lisp Parinfer Implementation

A complete Emacs Lisp port of [Parinfer](https://github.com/parinfer/parinfer) with enhanced diagnostic capabilities.

## Project Goals

1. **Pure Elisp Implementation**: No native dependencies, works on any Emacs installation
2. **LLM-Analyzable**: Designed for AI-assisted development and diagnostics
3. **Diagnostic Integration**: Deep integration with paren-diagnostic tools
4. **Feature Complete**: 100% parity with parinfer.js, plus selected Rust enhancements

## Status

🚧 **Under Active Development** 🚧

### Phase 1: JavaScript → Elisp Port (In Progress)
- [ ] Core data structures
- [ ] Utility functions
- [ ] State machine
- [ ] Algorithm implementation
- [ ] Test suite integration

### Phase 2: Rust Enhancements (Planned)
- [ ] Unicode handling
- [ ] Extended language support
- [ ] Enhanced error reporting

### Phase 3: Diagnostic Integration (Planned)
- [ ] Integration with claude-paren-diagnostics.el
- [ ] LLM-friendly inspection interfaces
- [ ] Algorithm tracing capabilities

## Architecture

### Source Files

- `parinfer.el` - Core algorithm (port of parinfer.js)
- `parinfer-test.el` - Test harness and ERT tests
- `parinfer-unicode.el` - Unicode enhancements (Phase 2)
- `parinfer-languages.el` - Extended language support (Phase 2)
- `parinfer-diagnostic.el` - Diagnostic tool integration (Phase 3)
- `parinfer-mode.el` - User-facing major/minor mode (Phase 4)

### Reference Implementation

This port is based on:
- **Primary**: `parinfer.js` v3.13.1 (1,808 lines) - simpler to port
- **Enhancements**: `parinfer-rust` v0.4.7 - for Unicode and extended features

## Design Decisions

### Why Port from JavaScript Instead of Rust?

1. **Simpler translation path**: JavaScript → Elisp is more direct than Rust → Elisp
2. **No lifetime complexity**: JavaScript's mutable closures map naturally to Elisp
3. **Reference implementation**: JavaScript is the original specification
4. **Proven approach**: parinferlib.el already validated the concept

### Why Not Just Use parinfer-rust-mode?

This project serves a different niche:
- **No native compilation required**: Works everywhere Emacs runs
- **Hackable and inspectable**: Users can modify algorithm behavior
- **Diagnostic-friendly**: Designed for LLM-based analysis and debugging
- **Educational**: Clean, readable code for learning Parinfer internals

### Performance Trade-offs

We accept slower performance in exchange for:
- Simplicity and hackability
- No FFI overhead for small operations
- Easy debugging with edebug
- LLM-analyzable code

**Target**: Within 3x of parinfer-rust for files < 5000 lines with native-comp.

**Fallback**: Recommend parinfer-rust-mode for large files or performance-critical use.

## Testing

Tests use the official Parinfer JSON test suite:
- `indent-mode.json`
- `paren-mode.json`
- `smart-mode.json`

Plus additional tests for:
- Unicode handling
- Edge cases
- Diagnostic integration

## Development Setup

```bash
# Clone the project (if from git later)
cd C:\Users\Apollo\em\parinfer-el

# Run tests (once implemented)
emacs -batch -L . -l parinfer-test.el -f ert-run-tests-batch-and-exit
```

## Integration with Paren-Diagnostic Tools

This implementation is designed to work with:
- `claude-helpers.el` - Programmatic Emacs interaction
- `claude-paren-diagnostics.el` - Token-efficient diagnostic tools

See `parinfer-diagnostic.el` (Phase 3) for integration examples.

## License

Same as original Parinfer (MIT License)

## Contributors

- **Original Parinfer**: Shaun LeBron and contributors
- **Elisp Port**: Claude Code project (2025)

## References

- [Parinfer](https://github.com/parinfer/parinfer) - Original implementation
- [parinfer-rust](https://github.com/eraserhd/parinfer-rust) - Rust implementation with extensions
- [parinfer-rust-mode](https://github.com/justinbarclay/parinfer-rust-mode) - Emacs integration for Rust version
