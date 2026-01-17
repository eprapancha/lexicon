# Lexicon Editor - Testing Guide

**Last Updated**: January 17, 2026
**Status**: Epic #86 (Emacs Semantic Compatibility) - 23/23 tests passing (100%)

---

## Quick Start

```bash
# Run all tests (from editor-cljs directory)
PATH=/home/nixos/.nix-profile/bin:$PATH npm run test

# Open in browser for interactive development
http://localhost:8021/test/index.html

# Tests auto-reload on file changes
```

## Running Tests

Lexicon uses **shadow-cljs browser-test** for automated testing. Tests run in a real browser environment with full WASM integration.

**Test Server**: http://localhost:8021/test/index.html
**Hot Reload**: Enabled (tests recompile on file changes)

### Option 1: Run Tests Only
```bash
cd /home/nixos/projects/lexicon/packages/editor-cljs
PATH=/home/nixos/.nix-profile/bin:$PATH npm run test
```

### Option 2: Run App + Tests Together
```bash
cd /home/nixos/projects/lexicon/packages/editor-cljs
./scripts/dev-with-tests.sh
# App:   http://localhost:8080
# Tests: http://localhost:8021/test/index.html
```

---

## Test Organization

### Test Suites

#### 1. **Semantic Compatibility Tests** (`test/lexicon/semantic/`)
Emacs behavioral compatibility tests from Epic #86.

**Status**: 23/23 passing (100%)

**Test Files**:
- `buffer_test.cljs` - Buffer creation, naming, switching (6 tests)
- `file_test.cljs` - File association, read-only buffers (3 tests)
- `point_mark_test.cljs` - Point, mark, region operations (5 tests)
- `kill_ring_test.cljs` - Kill ring, yank operations (2 tests)
- `undo_test.cljs` - Undo/redo operations (1 test)
- `mode_test.cljs` - Major/minor mode management (2 tests)
- `window_test.cljs` - Window management (2 tests)
- `minibuffer_test.cljs` - Minibuffer operations (1 test)
- `keymap_test.cljs` - Keymap operations (1 test)
- `messages_test.cljs` - Message system (2 tests)
- `command_test.cljs` - Command registration/invocation (3 tests)

**Coverage**:
- ✅ Buffer Operations (create, switch, kill, rename)
- ✅ File Operations (visit-file, buffer-file, read-only)
- ✅ Point & Mark (point, mark, region, exchange-point-and-mark)
- ✅ Kill Ring (kill-region, yank, consecutive kills append)
- ✅ Undo/Redo (buffer-local undo, undo boundaries)
- ✅ Modes (major/minor mode management)
- ✅ Windows (split, navigate, delete, buffer display)
- ✅ Minibuffer (activation, deactivation, insert, contents)
- ✅ Keymaps (global, local, prefix keys, lookup)
- ✅ Messages (message display, echo area)
- ✅ Commands (registration, invocation)

---

## Test Infrastructure

### Architecture

**Test Helpers** (`test/lexicon/semantic/helpers.cljs`):
- Provides API-only test operations (no direct state access)
- Enforces proper architecture: tests → API → events → state
- Includes WASM loading fixture (`with-wasm`)

**Test API** (`src/lexicon/api/test.cljs`):
- Public test interface - only API tests may use
- All operations dispatch through re-frame events
- Prevents tests from bypassing application logic

**Key Principles**:
1. Tests ONLY use `lexicon.api.test` functions
2. NO direct `@rfdb/app-db` access in tests
3. NO implementing application logic in helpers
4. Tests validate behavior, helpers provide setup

### WASM Integration

Tests run in real browser with full WASM integration:
- Async WASM loading with promise-based initialization
- Fresh WASM instance per buffer (proper isolation)
- UTF-8 byte position handling for emoji/multi-byte characters

---

## Recent Achievements

### January 17, 2026 - Undo System Fixed (100% Pass Rate)

**Fixed Issues**:
1. **Async dispatch problem**: Undo entries were dispatched asynchronously via `:fx`, causing tests to complete before undo was recorded
2. **Entry structure mismatch**: Events used `:action`/`:pos` but undo system expected `:op`/`:position`
3. **Invert function bug**: `invert-undo-entry` checked `:action` instead of `:op`
4. **Point management**: Kill-region didn't set point to start of killed region (Emacs behavior)
5. **Yank position**: Yank helper inserted at end instead of at point
6. **Event handler restrictions**: Can't call `dispatch-sync` from within event handler

**Solution**:
- Changed undo recording to synchronous (direct db updates instead of `:fx` dispatch)
- Fixed all field names to use `:op` and `:position`/:start`
- Refactored `:edit/undo` to call undo logic directly (no dispatch)
- Set point correctly after kill-region
- Fixed yank to use point position
- Removed problematic dispatch-sync calls from event handlers

**Files Modified**:
- `src/lexicon/advanced_undo.cljs` - Fixed invert function
- `src/lexicon/events/buffer.cljs` - Synchronous undo recording
- `src/lexicon/events/edit.cljs` - Synchronous undo for kill/yank, direct undo logic
- `src/lexicon/effects/log.cljs` - Removed dispatch-sync calls
- `src/lexicon/events/wasm.cljs` - Use console.log instead of message
- `test/lexicon/semantic/helpers.cljs` - Fixed yank to use point
- `test/lexicon/semantic/kill_ring_test.cljs` - Updated test expectation

---

## Writing Tests

### Example Test

```clojure
(ns lexicon.semantic.buffer-test
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

(use-fixtures :once h/with-wasm)

(deftest buffer-creation
  (testing "Create and switch buffers"
    (h/reset-editor-db!)
    (let [buf-id (h/create-buffer "test-buffer")]
      (is (some? buf-id) "Buffer should be created")
      (is (h/buffer-exists? "test-buffer") "Buffer should exist")
      (h/switch-to-buffer "test-buffer")
      (is (= buf-id (h/current-buffer)) "Should switch to buffer"))))
```

### Best Practices

**DO**:
- ✅ Use `h/reset-editor-db!` at start of each test
- ✅ Use API functions from `lexicon.semantic.helpers`
- ✅ Test behavior, not implementation
- ✅ Keep tests focused and atomic
- ✅ Use descriptive test names

**DON'T**:
- ❌ Access `@rfdb/app-db` directly
- ❌ Implement application logic in helpers
- ❌ Share state between tests
- ❌ Use `swap!` or `assoc-in` on app-db
- ❌ Test implementation details

---

## Debugging Tests

### Common Issues

**1. Empty undo stack**
- **Symptom**: Undo doesn't work, stack-size=0
- **Cause**: Async dispatch hasn't completed
- **Fix**: Use synchronous db updates, not `:fx [[:dispatch ...]]`

**2. Wrong buffer text after operation**
- **Symptom**: Buffer has unexpected content
- **Cause**: WASM operation parameters wrong (e.g., delete(start, end) vs delete(pos, length))
- **Fix**: Check WASM API usage, ensure correct parameters

**3. Test interference**
- **Symptom**: Tests pass individually but fail together
- **Cause**: Missing `reset-editor-db!` call
- **Fix**: Add `(h/reset-editor-db!)` at start of test

**4. Dispatch-sync errors**
- **Symptom**: "can't call dispatch-sync within event handler"
- **Cause**: Calling `lexicon.api.message/message` or `dispatch-sync` from event
- **Fix**: Use `js/console.log` or direct db updates instead

---

## Test Results History

| Date | Tests | Pass | Fail | Pass Rate |
|------|-------|------|------|-----------|
| Jan 17, 2026 | 23 | 23 | 0 | 100% |
| Jan 16, 2026 | 23 | 21 | 2 | 91% |
| Dec 27, 2025 | 41 | 35 | 6 | 85% |

---

## Next Steps

### Current Epic: #86 - Emacs Semantic Compatibility
- ✅ Phase 1: Buffer semantics (100%)
- ✅ Phase 2: File semantics (100%)
- ✅ Phase 3: Point & Mark (100%)
- ✅ Phase 4: Kill Ring (100%)
- ✅ Phase 5: Undo (100%)
- ✅ Phase 6: Modes (100%)
- ✅ Phase 7: Windows (100%)
- ✅ Phase 8: Minibuffer (100%)
- ✅ Phase 9: Keymaps (100%)
- ✅ Phase 10: Messages (100%)
- ✅ Phase 11: Commands (100%)

**Status**: All semantic compatibility tests passing! 🎉

### Future Work
1. Additional edge case coverage
2. Performance tests
3. Integration tests
4. Visual regression tests

---

## Files

### Test Files
- `test/lexicon/semantic/*_test.cljs` - Semantic compatibility tests
- `test/lexicon/semantic/helpers.cljs` - Test helper functions
- `test/lexicon/semantic/helpers_macros.clj` - Test macros (with-buffer)
- `test/lexicon/test_setup.cljs` - WASM loading setup

### Production Files (Test API)
- `src/lexicon/api/test.cljs` - Public test API
- `src/lexicon/api/message.cljs` - Message system API

### Documentation
- `test/TESTING.md` - This file (comprehensive testing guide)
- `test/MANUAL_TEST_PLAN.md` - Manual test procedures

---

## Contributing

When adding new tests:
1. Add test file to `test/lexicon/semantic/`
2. Use `lexicon.semantic.helpers` API only
3. Follow existing test patterns
4. Ensure test isolation with `reset-editor-db!`
5. Run full test suite to check for regressions

When fixing bugs:
1. Write failing test first
2. Implement fix
3. Verify test passes
4. Check for regressions
5. Update documentation

---

**Maintained by**: Claude Code
**Epic**: #86 - Emacs Semantic Compatibility Tests
**Status**: ✅ Complete (100% pass rate)
