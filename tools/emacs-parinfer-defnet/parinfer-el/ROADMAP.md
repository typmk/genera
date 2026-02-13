# Parinfer.el Implementation Roadmap

**Project**: Pure Emacs Lisp implementation of Parinfer
**Based on**: parinfer.js v3.13.1 (1,808 lines)
**Target**: Complete feature parity + diagnostic integration
**Timeline**: 10-14 weeks

---

## Phase 1: Core Algorithm Port (Weeks 1-6)

### Week 1: Foundation & Test Infrastructure ✅ STARTED

**Status**: 15% complete

**Completed**:
- ✅ Project structure setup
- ✅ Constants and type predicates
- ✅ String and array helper functions
- ✅ Stack operations
- ✅ Result structure (`cl-defstruct`)
- ✅ Initial options parsing

**This Week**:
- [ ] Test infrastructure setup (ERT + JSON test files)
- [ ] Character classification functions
- [ ] Error handling system
- [ ] Basic smoke tests

**Deliverable**: Working test harness that can load and parse JSON tests

**Files**:
- `parinfer.el` (currently ~380 lines, target ~500)
- `parinfer-test.el` (new, target ~200 lines)

**Lines to Port**: ~150 lines from parinfer.js (lines 200-350)

---

### Week 2: State Machine & Character Processing

**Target**: 30% complete

**Tasks**:
- [ ] Context state tracking (isInCode, isInStr, isInComment)
- [ ] Character event handlers (onChar, onOpenParen, onCloseParen)
- [ ] Quote and escape handling
- [ ] Comment detection

**Deliverable**: Can parse through text and track parsing state

**Files**:
- `parinfer.el` (target ~800 lines)

**Lines to Port**: ~250 lines from parinfer.js (lines 650-900)

**Tests**: Basic state tracking tests

---

### Week 3: Line Processing & Indentation

**Target**: 45% complete

**Tasks**:
- [ ] Line initialization (initLine)
- [ ] Indentation tracking (trackingIndent)
- [ ] Whitespace handling
- [ ] Line finalization (finalizeResult)

**Deliverable**: Can process lines and track indentation

**Files**:
- `parinfer.el` (target ~1,000 lines)

**Lines to Port**: ~200 lines from parinfer.js (lines 1400-1600)

**Tests**: Line processing tests, indentation detection

---

### Week 4: Paren Stack & Trail Management

**Target**: 60% complete

**Tasks**:
- [ ] Paren stack push/pop operations
- [ ] Paren trail detection and tracking
- [ ] Paren trail clamping
- [ ] Tab stop calculation

**Deliverable**: Can track and manipulate paren structure

**Files**:
- `parinfer.el` (target ~1,200 lines)

**Lines to Port**: ~200 lines from parinfer.js (lines 700-900)

**Tests**: Paren stack tests, trail detection tests

---

### Week 5: Parent Opener Index (The Complex Part!)

**Target**: 75% complete

**Tasks**:
- [ ] Parent opener index algorithm (~180 lines in JS)
- [ ] Fragmentation vs adoption logic
- [ ] indentDelta tracking
- [ ] Edge case handling

**Deliverable**: Correctly determines parent-child relationships

**Files**:
- `parinfer.el` (target ~1,400 lines)

**Lines to Port**: ~200 lines from parinfer.js (lines 1095-1295)

**Tests**: Parent-opener-index tests (critical!)

**Risk**: This is the most complex algorithm. Plan for debugging time.

---

### Week 6: Indentation Correction & Paren Mode

**Target**: 85% complete

**Tasks**:
- [ ] Indentation correction (correctIndent)
- [ ] Paren trail correction (correctParenTrail)
- [ ] Max indentation tracking (Paren Mode)
- [ ] Force balance option

**Deliverable**: Basic Indent Mode and Paren Mode working

**Files**:
- `parinfer.el` (target ~1,600 lines)

**Lines to Port**: ~200 lines from parinfer.js (lines 1300-1500)

**Tests**: Full JSON test suite for indent-mode.json and paren-mode.json

---

## Phase 2: Smart Mode & Cursor Handling (Weeks 7-8)

### Week 7: Cursor Tracking & Smart Mode

**Target**: 95% complete

**Tasks**:
- [ ] Cursor position tracking
- [ ] Previous cursor handling
- [ ] Cursor-holding detection
- [ ] Smart mode decision logic
- [ ] Exit to Paren Mode mechanism

**Deliverable**: Smart Mode fully working

**Files**:
- `parinfer.el` (target ~1,750 lines)

**Lines to Port**: ~150 lines from parinfer.js (lines 750-900)

**Tests**: smart-mode.json test suite

---

### Week 8: Changes API & Incremental Processing

**Target**: 100% feature parity with parinfer.js

**Tasks**:
- [ ] Changes data structure handling
- [ ] Incremental change detection
- [ ] Selection tracking
- [ ] Return parens option (paren tree)

**Deliverable**: All parinfer.js features implemented

**Files**:
- `parinfer.el` (target ~1,850 lines - matches parinfer.js)

**Lines to Port**: ~100 lines from parinfer.js (various sections)

**Tests**: All JSON tests passing (143 tests)

---

## Phase 3: Rust Enhancements (Weeks 9-10)

### Week 9: Unicode Handling

**Tasks**:
- [ ] Grapheme cluster awareness
- [ ] Display width calculation
- [ ] Column-to-byte-index mapping
- [ ] Multi-byte character support

**Deliverable**: Correct Unicode handling

**Files**:
- `parinfer-unicode.el` (new, ~200 lines)
- Enhanced `parinfer.el` to use Unicode functions

**Tests**: Unicode edge case tests

**Reference**: parinfer-rust (lines 437-507)

---

### Week 10: Extended Language Features

**Tasks**:
- [ ] Common Lisp block comments `#|...|#`
- [ ] Scheme sexp comments `#;`
- [ ] Configurable comment characters
- [ ] Vline symbols `|...|`

**Deliverable**: Extended language support

**Files**:
- `parinfer-languages.el` (new, ~150 lines)

**Tests**: Language-specific tests

**Reference**: parinfer-rust (lines 839-979)

---

## Phase 4: Diagnostic Integration (Weeks 11-12)

### Week 11: Diagnostic Hooks

**Tasks**:
- [ ] Integration with `claude-paren-diagnostics.el`
- [ ] Pre-validation hooks
- [ ] Post-validation hooks
- [ ] State inspection functions
- [ ] Algorithm tracing mode

**Deliverable**: Diagnostic integration working

**Files**:
- `parinfer-diagnostic.el` (new, ~300 lines)

**Functions**:
- `parinfer-diagnose-buffer`
- `parinfer-explain-error`
- `parinfer-algorithm-trace`
- `parinfer-validate-state`

---

### Week 12: User-Facing Mode & Polish

**Tasks**:
- [ ] `parinfer-mode` minor mode
- [ ] Integration with Emacs editing
- [ ] Performance profiling and optimization
- [ ] Comprehensive documentation
- [ ] Migration guide from parinfer-rust-mode

**Deliverable**: Production-ready package

**Files**:
- `parinfer-mode.el` (new, ~400 lines)
- Enhanced documentation
- Performance benchmarks

---

## Detailed Function Mapping

### JavaScript → Elisp Function Translation

| JavaScript Function | Elisp Function | Lines (JS) | Priority | Week |
|---------------------|----------------|------------|----------|------|
| **Character Classification** |
| `isOpenParen` | `parinfer--open-paren-p` | 5 | HIGH | 1 |
| `isCloseParen` | `parinfer--close-paren-p` | 5 | HIGH | 1 |
| `isWhitespace` | `parinfer--whitespace-p` | 3 | HIGH | 1 |
| **Paren Matching** |
| `matchParen` | `parinfer--match-paren` | 5 | HIGH | 1 |
| **Error Handling** |
| `cacheErrorPos` | `parinfer--cache-error-pos` | 10 | HIGH | 1 |
| `createError` | `parinfer--create-error` | 40 | HIGH | 1 |
| **Context State** |
| `onChar` | `parinfer--on-char` | 25 | HIGH | 2 |
| `onOpenParen` | `parinfer--on-open-paren` | 15 | HIGH | 2 |
| `onCloseParen` | `parinfer--on-close-paren` | 20 | HIGH | 2 |
| `onTab` | `parinfer--on-tab` | 10 | MED | 2 |
| `onSemicolon` | `parinfer--on-semicolon` | 10 | MED | 2 |
| `onNewline` | `parinfer--on-newline` | 10 | MED | 2 |
| `onQuote` | `parinfer--on-quote` | 20 | HIGH | 2 |
| `onBackslash` | `parinfer--on-backslash` | 15 | HIGH | 2 |
| `afterBackslash` | `parinfer--after-backslash` | 10 | HIGH | 2 |
| `onCommentChar` | `parinfer--on-comment-char` | 10 | HIGH | 2 |
| **Line Processing** |
| `initLine` | `parinfer--init-line` | 30 | HIGH | 3 |
| `commitChar` | `parinfer--commit-char` | 20 | HIGH | 3 |
| `processChar` | `parinfer--process-char` | 40 | HIGH | 3 |
| `processLine` | `parinfer--process-line` | 25 | HIGH | 3 |
| `finalizeResult` | `parinfer--finalize-result` | 35 | HIGH | 3 |
| **Paren Trail** |
| `updateParenTrailBounds` | `parinfer--update-paren-trail-bounds` | 15 | HIGH | 4 |
| `clampParenTrailToCursor` | `parinfer--clamp-paren-trail-to-cursor` | 30 | HIGH | 4 |
| `popParenTrail` | `parinfer--pop-paren-trail` | 15 | HIGH | 4 |
| `correctParenTrail` | `parinfer--correct-paren-trail` | 40 | HIGH | 4 |
| `cleanParenTrail` | `parinfer--clean-paren-trail` | 20 | HIGH | 4 |
| `appendParenTrail` | `parinfer--append-paren-trail` | 10 | MED | 4 |
| `finishNewParenTrail` | `parinfer--finish-new-paren-trail` | 20 | MED | 4 |
| **Tab Stops** |
| `addTabStop` | `parinfer--add-tab-stop` | 10 | MED | 4 |
| `trackArgTabStop` | `parinfer--track-arg-tab-stop` | 15 | MED | 4 |
| **Parent Opener** |
| `getParentOpenerIndex` | `parinfer--get-parent-opener-index` | 180 | CRITICAL | 5 |
| **Indentation** |
| `correctIndent` | `parinfer--correct-indent` | 50 | HIGH | 6 |
| `onProperIndent` | `parinfer--on-proper-indent` | 20 | HIGH | 6 |
| `onIndent` | `parinfer--on-indent` | 30 | HIGH | 6 |
| `onLeadingCloseParen` | `parinfer--on-leading-close-paren` | 15 | HIGH | 6 |
| **Cursor** |
| `isCursorAffected` | `parinfer--cursor-affected-p` | 10 | HIGH | 7 |
| `isCursorHoldingOpenParen` | `parinfer--cursor-holding-open-paren-p` | 25 | HIGH | 7 |
| `checkCursorHolding` | `parinfer--check-cursor-holding` | 30 | HIGH | 7 |
| `isCursorInComment` | `parinfer--cursor-in-comment-p` | 10 | MED | 7 |
| `handleChangeDelta` | `parinfer--handle-change-delta` | 20 | MED | 8 |
| **High-level** |
| `processText` | `parinfer--process-text` | 40 | HIGH | 8 |
| `processError` | `parinfer--process-error` | 15 | HIGH | 8 |
| `publicResult` | `parinfer--public-result` | 40 | HIGH | 8 |
| **Public API** |
| `indentMode` | `parinfer-indent-mode` | 5 | HIGH | 8 |
| `parenMode` | `parinfer-paren-mode` | 5 | HIGH | 8 |
| `smartMode` | `parinfer-smart-mode` | 5 | HIGH | 8 |

**Total Functions**: ~60 functions
**Total Lines**: ~1,850 lines (matches parinfer.js size)

---

## Test Strategy

### Test Files (from parinfer test suite)

1. **indent-mode.json** (~50 tests)
   - Basic indentation inference
   - Nested structures
   - Edge cases

2. **paren-mode.json** (~45 tests)
   - Indentation correction
   - Paren balance
   - Max indent

3. **smart-mode.json** (~48 tests)
   - Cursor-aware behavior
   - Hold detection
   - Smart transitions

**Total**: 143 JSON test cases

### Test Phases

**Phase 1 (Week 1)**: Test infrastructure
- Load JSON files
- Parse test cases
- Run single test
- Compare results

**Phase 2 (Weeks 2-6)**: Progressive testing
- Test each function as implemented
- Unit tests for complex functions (parent-opener-index)
- Integration tests after each week

**Phase 3 (Week 8)**: Full validation
- All 143 JSON tests passing
- Performance benchmarks
- Regression tests

**Phase 4 (Weeks 9-12)**: Enhancement testing
- Unicode edge cases
- Language-specific tests
- Diagnostic integration tests

---

## Risk Assessment & Mitigation

### High-Risk Areas

1. **Parent Opener Index Algorithm** (Week 5)
   - **Risk**: Most complex algorithm, easy to get wrong
   - **Mitigation**:
     - Extensive unit tests
     - Step-by-step debugging with edebug
     - Compare intermediate state with JavaScript
     - Add diagnostic tracing

2. **Performance** (Ongoing)
   - **Risk**: Elisp significantly slower than Rust
   - **Mitigation**:
     - Use `defsubst` for hot paths
     - Profile early and often (profiler.el)
     - Native compilation from day 1
     - Accept performance trade-off, document it

3. **Unicode Handling** (Week 9)
   - **Risk**: Elisp lacks grapheme cluster iteration
   - **Mitigation**:
     - Manual implementation using char-width
     - Extensive testing with international text
     - Document limitations
     - Provide parinfer-rust-mode fallback

4. **Smart Mode Cursor Tracking** (Week 7)
   - **Risk**: Complex state transitions
   - **Mitigation**:
     - Thorough testing
     - Diagnostic mode for tracing decisions
     - Reference JavaScript implementation closely

---

## Success Metrics

### Must Have (100% Required)

- ✅ All 143 JSON tests passing
- ✅ Smart Mode working correctly
- ✅ No crashes or errors on valid input
- ✅ Correct error messages for invalid input
- ✅ Integration with paren-diagnostic tools

### Should Have (80% Target)

- ⭐ Performance within 3x of parinfer-rust (with native-comp)
- ⭐ Unicode handling for common cases (ASCII + European languages)
- ⭐ Extended language features (CL block comments, etc.)
- ⭐ Comprehensive diagnostic interface

### Nice to Have (If Time Permits)

- 🎯 Perfect Unicode handling (all grapheme clusters)
- 🎯 All Rust language extensions
- 🎯 Performance within 2x of parinfer-rust
- 🎯 Interactive debugging mode

---

## Dependencies

### Required
- Emacs 26.1+ (lexical binding, cl-lib)
- No external packages for core

### Optional
- Emacs 28+ for native compilation (performance)
- ERT for testing (built-in)

### For Phase 4 (Diagnostic Integration)
- `claude-helpers.el` (from C:\Users\Apollo\em)
- `claude-paren-diagnostics.el` (from C:\Users\Apollo\em)

---

## Deliverables Checklist

### Week 1
- [ ] parinfer.el foundation (500 lines)
- [ ] parinfer-test.el (200 lines)
- [ ] Test infrastructure working
- [ ] First 10 tests passing

### Week 8
- [ ] parinfer.el complete (1,850 lines)
- [ ] All 143 JSON tests passing
- [ ] Performance benchmarks documented
- [ ] API documentation

### Week 10
- [ ] parinfer-unicode.el (200 lines)
- [ ] parinfer-languages.el (150 lines)
- [ ] Unicode test suite passing
- [ ] Language feature tests passing

### Week 12
- [ ] parinfer-diagnostic.el (300 lines)
- [ ] parinfer-mode.el (400 lines)
- [ ] Full documentation
- [ ] Migration guide
- [ ] Performance optimization complete

---

## Next Immediate Steps

1. ✅ Create this roadmap
2. ⏭️ Set up test infrastructure (parinfer-test.el)
3. ⏭️ Download JSON test files
4. ⏭️ Implement character classification
5. ⏭️ Run first smoke test

**Ready to proceed with test infrastructure setup!**
