# Lexicon Test Suite

## Overview

This directory contains automated tests for the Lexicon editor, covering:
- **Unit tests**: Core editing, cursor movement, kill ring, buffer operations
- **Integration tests**: Window management, multi-window workflows
- **Regression tests**: Critical workflows and past bug fixes

## Test Files

- `lexicon/core_test.cljs` - Core editing functionality tests
- `lexicon/window_test.cljs` - Window management tests
- `lexicon/regression_test.cljs` - Regression tests for past bugs
- `lexicon/test_runner.cljs` - Test runner that executes all tests

## Running Tests

### Current Status

✅ **Browser-based test automation is SET UP and READY!**

Tests run in a real browser environment using **shadow-cljs :browser-test**:
1. Tests use `re-frame` with full browser API support ✅
2. Tests interact with React/Reagent components ✅
3. Tests use the WASM gap buffer with WebAssembly ✅
4. Test server runs on http://localhost:8021 ✅
5. Hot reload watches for changes ✅

**📖 See [RUNNING_TESTS.md](./RUNNING_TESTS.md) for complete documentation**

### Quick Start

```bash
# Start test server with hot reload
cd /home/nixos/projects/lexicon/packages/editor-cljs
npx shadow-cljs watch test

# Open browser to http://localhost:8021/test-index.html
# Tests run automatically and display results
```

### Test Infrastructure

- **37 automated tests** covering core functionality
- **Automatic re-running** when files change
- **Browser UI** shows pass/fail status with details
- **Console output** for debugging

### Manual Verification Checklist (Deprecated)

The following checklist is kept for reference but is now automated by the test suite.
To manually verify functionality, open the Lexicon editor and:

#### Core Editing Tests
- [ ] Type characters → should appear on screen
- [ ] Press Enter → should create newline
- [ ] Press Backspace → should delete character before cursor
- [ ] Insert text mid-line → should work correctly
- [ ] Delete text → should remove characters
- [ ] Move cursor forward/backward → C-f, C-b should work
- [ ] Move to beginning/end of line → C-a, C-e should work

#### Kill Ring Tests
- [ ] Set mark (C-SPC), move cursor, kill region (C-w) → text should be removed and stored
- [ ] Yank (C-y) → killed text should be inserted at point
- [ ] Kill line (C-k) → rest of line should be killed

#### Buffer Tests
- [ ] Create new buffer (C-x C-f) → should prompt for filename
- [ ] Switch buffer (C-x b) → should show buffer list
- [ ] Multiple buffers → should maintain separate state

#### Window Tests
- [ ] Split horizontally (C-x 2) → should create two windows
- [ ] Split vertically (C-x 3) → should create two windows
- [ ] Switch window (C-x o) → should move to other window
- [ ] Delete window (C-x 0) → should close current window
- [ ] Delete other windows (C-x 1) → should keep only current window

#### Regression Tests
- [ ] UTF-8 emoji (👍🎉) → should display correctly
- [ ] Empty lines → should render and be navigable
- [ ] Multi-window with same buffer → cursors should be independent
- [ ] Undo after insert → should restore previous state
- [ ] Undo after delete → should restore deleted text

### Future: Automated Browser Testing

To enable automated test execution, we need:

1. **Browser test runner** (Karma + Chrome Headless, or Playwright)
2. **Test harness** that initializes re-frame and WASM
3. **CI/CD integration** to run tests on every commit

Recommended approach:
```clojure
;; In shadow-cljs.edn
:test {:target :browser-test
       :test-dir "test-output"
       :devtools {:http-root "test-output"
                  :http-port 8021}}
```

Then use a headless browser to run tests:
```bash
npx shadow-cljs compile test
npx karma start  # or playwright test
```

## Test Coverage

Current test coverage (by feature):

| Feature | Tests | Status |
|---------|-------|--------|
| Text insertion | ✅ | `test-insert-text`, `test-self-insert-command` |
| Text deletion | ✅ | `test-delete-text`, `test-backspace-works` |
| Cursor movement | ✅ | `test-forward-char`, `test-backward-char`, etc. |
| Kill ring | ✅ | `test-kill-region`, `test-yank`, `test-kill-yank-workflow` |
| Undo/Redo | ✅ | `test-undo-insert`, `test-undo-delete`, `test-undo-redo-chain` |
| Buffers | ✅ | `test-buffer-creation`, `test-switch-buffer` |
| Windows | ✅ | `test-split-window-*`, `test-other-window`, `test-delete-window` |
| UTF-8 | ✅ | `test-emoji-support`, `test-multibyte-cursor-movement` |
| Regressions | ✅ | Phase 0, 3 bugs covered |

## Contributing

When adding new features:

1. **Write tests first** (TDD) or alongside the feature
2. **Add regression tests** for any bugs you fix
3. **Update this README** if you add new test files
4. **Run manual verification** using the checklist above

## Notes

These tests are written using `cljs.test` (ClojureScript's built-in testing framework). They follow Emacs testing conventions where appropriate and serve as both:
- Executable tests (once browser test automation is set up)
- Living documentation of expected behavior
