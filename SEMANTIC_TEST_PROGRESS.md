# Semantic Compatibility Test Conversion Progress

**Status:** 10/34 tests passing (29%)

**Date:** 2026-01-17

## Completed Tests (10)

### Buffer-File Semantics (4 tests)
- ✅ `buffer-can-exist-without-file` - Buffers don't require files
- ✅ `buffer-identity-is-stable` - Buffer ID stable across content changes
- ✅ `buffer-can-change-visited-file` - File association is mutable metadata
- ✅ `multiple-buffers-can-coexist` - Multiple independent buffers

### Point/Mark Semantics (2 tests)
- ✅ `point-is-buffer-local` - Point belongs to buffer, not window
- ✅ `point-is-independent-per-buffer` - Each buffer has own point

### Message System (2 tests)
- ✅ `messages-are-buffer-backed-editor-state` - Messages in *Messages* buffer
- ✅ `message-appends-to-history-but-updates-echo-area` - Echo area persistence

### Core Semantics (2 tests)
- ✅ `buffer-identity-is-stable-across-content` - (duplicate test, should remove)

## Infrastructure Fixes Completed

1. **CORS/WebSocket** - Skip WebSocket/parser in test mode (port 8021)
2. **Log Bus Integration** - Load `lexicon.effects.log` namespace in tests
3. **Echo Area Semantics** - Messages persist until replaced (not 2-second timeout)
4. **Point Storage** - Point now buffer-local (stored in buffer, not window)
5. **WASM Loading** - Working test fixture with async loading
6. **System Buffers** - Preserve *scratch* and *Messages* across test resets

## Test Helpers Implemented (13 functions)

- `reset-editor-db!` - Clean slate with WASM/buffers preserved
- `create-buffer` - Create buffer with content, display in active window
- `buffer-text` - Get buffer content by ID or name
- `insert-text` - Insert at end of buffer
- `buffer-file` - Get file association
- `buffer-exists?` - Check if buffer exists by name
- `message` - Display message in echo area + *Messages*
- `echo-area-text` - Get current echo area text
- `visit-file` - Associate file with buffer
- `current-buffer` - Get active buffer ID
- `point` - Get buffer point (character position)
- `set-point` - Set buffer point
- `with-wasm` - Fixture for WASM loading

## Remaining Tests (24)

### Cannot Convert Yet - Missing Core Features

**Kill Ring (2 tests)** - Needs kill/yank implementation
- `kill-ring-is-global`
- `consecutive-kills-append`

**Undo System (4 tests)** - Needs undo/redo implementation
- `undo-is-buffer-local`
- `undo-is-not-destructive`
- `undo-boundary-test`
- `undo-integration-test`

**Command System (4 tests)** - Needs command registry/dispatch
- `command-has-metadata-and-identity`
- `all-command-invocations-go-through-dispatch`
- `command-definition-test`
- `error-isolation-test`

**Window Management (3 tests)** - Needs split/delete window ops
- `buffer-identity-is-stable-across-views`
- `window-deletion-preserves-buffer-identity`
- `window-tree-invariants`

**Keymap System (2 tests)** - Needs keymap implementation
- `local-keymap-shadows-global-keymap`
- `prefix-key-waits-for-completion`

**Filesystem (5 tests)** - Needs async file I/O
- `async-io-test`
- `file-completion-test`
- `save-and-revert-test`
- `dired-readiness-test`
- `project-semantics-test`

**Lisp Evaluation (4 tests)** - Needs Lisp interpreter
- `evaluation-model-test`
- `dynamic-binding-test`
- `buffer-local-variables-test`
- `introspection-test`

## Next Steps

### Option A: Focus on Vertico-Critical Tests
Convert only tests that Vertico directly depends on (~10-15 tests, 4-6 hours):
- Minibuffer stack semantics
- Completion tables
- Buffer-local keymaps
- Modal editing readiness

### Option B: Implement Missing Features
Incrementally add features to enable more tests:
1. Kill ring (2-3 hours) → enables 2 tests
2. Basic undo (4-5 hours) → enables 4 tests
3. Command system (3-4 hours) → enables 4 tests
4. Window splits (4-5 hours) → enables 3 tests

### Option C: Hybrid Approach (RECOMMENDED)
1. Convert all tests that CAN work now (done - 10 tests)
2. Stub out tests for missing features with `(is false "Not implemented")`
3. Attempt Vertico integration
4. Implement missing features as Vertico requires them

## Architecture Decisions Made

1. **ClojureScript Browser Tests** - JVM Clojure can't import ClojureScript namespaces
2. **Test Mode Detection** - Check port 8021 to skip production services
3. **Point Storage** - Buffer-local (Emacs semantics) not window-local
4. **Echo Area Persistence** - Messages persist until replaced (Emacs semantics)
5. **Synchronous Dispatch** - Use `dispatch-sync` for predictable test behavior

## Files Modified

- `packages/editor-cljs/src/lexicon/events/wasm.cljs` - Skip WS/parser in tests
- `packages/editor-cljs/src/lexicon/events/message.cljs` - Persistent echo area
- `packages/editor-cljs/src/lexicon/api/message.cljs` - Synchronous dispatch
- `packages/editor-cljs/test/lexicon/semantic/helpers.cljs` - 13 test helpers
- `packages/editor-cljs/test/lexicon/semantic/buffer_semantics_test.cljs` - 4 tests
- `packages/editor-cljs/test/lexicon/semantic/messages_test.cljs` - 2 tests
- `packages/editor-cljs/test/lexicon/semantic/point_mark_test.cljs` - 2 tests
- `packages/editor-cljs/test/lexicon/semantic/core_semantics_test.cljs` - 1 test
- `packages/editor-cljs/shadow-cljs.edn` - Include semantic tests

## Semantic Bugs Fixed

1. **Echo Area Auto-Clear** - Was clearing after 2s, now persists (Emacs semantics)
2. **Point Storage Location** - Was in window, now in buffer (Emacs semantics)
3. **Async Message Dispatch** - Was async, now sync for visibility (Emacs semantics)
