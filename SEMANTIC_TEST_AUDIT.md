# Semantic Test Audit - Test Helper Violations

**Date:** 2026-01-17
**Status:** CRITICAL - Test helpers implementing application logic instead of testing it

## Executive Summary

**73% of test helpers (24/33) are implementing application logic directly**, bypassing the actual application. Tests are passing by testing the helpers themselves, not the application code.

This violates fundamental TDD principles and gives false confidence in test coverage.

## Violations by Category

### 🚨 CRITICAL: Application Logic in Test Helpers

#### 1. Kill Ring System (Lines 447-483)
**Helpers:**
- `kill-region` - Implements kill ring storage, consecutive kill appending
- `yank` - Implements yank logic

**Problem:** Entire kill ring system exists ONLY in test helpers. Application has no kill ring.

**Real Implementation Needed:**
- `src/lexicon/events/edit.cljs` - `:edit/kill-region`, `:edit/yank` events
- `src/lexicon/state/kill_ring.cljs` - Kill ring state management

#### 2. Undo System (Lines 489-506)
**Helper:**
- `undo` - Directly manipulates WASM, implements undo logic

**Problem:** Application has no undo system. Helper records undo entries and applies them directly.

**Real Implementation Needed:**
- `src/lexicon/events/edit.cljs` - `:edit/undo` event
- Undo stack management in buffer state
- Integration with WASM or re-frame interceptors

#### 3. Keymap System (Lines 512-555)
**Helpers:**
- `set-global-key` - Creates keymap data structure
- `set-buffer-local-key` - Creates buffer-local keymaps
- `lookup-key` - Implements keymap resolution precedence
- `press-key-sequence` - Implements prefix key accumulation
- `in-prefix-state?` - Checks prefix state
- `last-invoked-command` - Tracks command invocation

**Problem:** Entire keymap system is fake. Buffer-local shadowing, prefix keys, resolution order - all in helpers.

**Real Implementation Needed:**
- `src/lexicon/events/keymap.cljs` - Keymap registration, lookup
- `src/lexicon/events/input.cljs` - Key sequence handling
- Proper keymap data structure in app-db

#### 4. Mode System (Lines 170-196)
**Helpers:**
- `enable-major-mode` - Direct db manipulation
- `current-major-mode` - Query
- `enable-minor-mode` - Direct db manipulation
- `minor-mode-enabled?` - Query

**Problem:** Mode activation bypasses any application logic.

**Real Implementation Needed:**
- `src/lexicon/events/mode.cljs` - `:mode/set-major`, `:mode/enable-minor` events
- Mode hooks, initialization, cleanup

#### 5. Window Management (Lines 284-378)
**Helpers:**
- `split-window-horizontally` - Implements window splitting logic
- `show-buffer-in-two-windows` - Window manipulation
- `show-buffer-in-new-window` - Window manipulation
- `window-text` - Query (OK)
- `delete-other-windows` - Implements window deletion

**Problem:** Window tree manipulation is in helpers, not using application APIs.

**Real Implementation Needed:**
- `src/lexicon/events/window.cljs` - `:window/split`, `:window/delete-others` events
- Proper window tree state management

#### 6. Minibuffer Operations (Lines 384-441)
**Helpers:**
- `activate-minibuffer` - Creates buffer, manipulates minibuffer-stack
- `deactivate-minibuffer` - Pops stack
- `minibuffer-insert` - Inserts text
- `minibuffer-contents` - Gets contents (query OK)

**Problem:** Minibuffer lifecycle management in helper.

**Real Implementation Needed:**
- `src/lexicon/events/minibuffer.cljs` - `:minibuffer/activate`, `:minibuffer/deactivate` events

#### 7. Read-Only Flag (Lines 125-128)
**Helper:**
- `set-read-only` - Direct db manipulation

**Problem:** Bypasses any buffer protection logic.

**Real Implementation Needed:**
- `src/lexicon/events/buffer.cljs` - `:buffer/set-read-only` event

#### 8. Buffer Metadata (Lines 147-149)
**Helper:**
- `visit-file` - Direct db manipulation

**Problem:** File association bypasses application logic.

**Real Implementation Needed:**
- `src/lexicon/events/buffer.cljs` - `:buffer/set-file` event

### ✅ LEGITIMATE Test Helpers (Keep These)

#### Test Setup (Lines 17-36)
- `reset-editor-db!` - Clean state for tests ✓
- `with-wasm` - WASM loading fixture ✓

#### Queries (Read-Only)
- `current-db` - Get app-db ✓ (for inspection only)
- `buffer-text` - Query buffer content ✓
- `echo-area-text` - Query echo area ✓
- `current-buffer` - Query active buffer ✓
- `point` - Query point ✓
- `buffer-file` - Query file association ✓
- `buffer-exists?` - Query buffer existence ✓

#### Real API Calls
- `create-buffer` - Calls `dispatch-sync [:create-buffer ...]` ✓
- `insert-text` - Calls `dispatch-sync [:buffer/insert ...]` ✓ (but adds undo - violation!)
- `message` - Calls `api-msg/message` ✓

#### Test Infrastructure
- `define-test-command` - Uses `dispatch-sync [:register-command ...]` ✓
- `command-name`, `command-doc`, `command-fn` - Extract metadata ✓
- `invoke-command` - Calls `dispatch-sync [:execute-command ...]` ✓
- `with-instrumented-dispatch` - Test instrumentation ✓
- `dispatch-was-used?` - Test query ✓

#### Helper for Test Macros
- `set-point` - Used by `with-buffer` macro (questionable but minor)

## Test Impact Assessment

### Tests That Will FAIL After Cleanup

**Kill Ring (2 tests):**
- `kill-ring-is-global` ❌
- `consecutive-kills-append` ❌

**Undo (1 test):**
- `undo-is-buffer-local` ❌

**Keymaps (2 tests):**
- `local-keymap-shadows-global-keymap` ❌
- `prefix-key-waits-for-completion` ❌

**Modes (2 tests):**
- `exactly-one-major-mode-per-buffer` ❌ (uses direct manipulation)
- `multiple-minor-modes-can-coexist` ❌ (uses direct manipulation)

**Windows (2 tests):**
- `buffer-identity-is-stable-across-views` ❌ (uses split-window helper)
- `window-deletion-preserves-buffer-identity` ❌ (uses delete-other-windows)

**Minibuffer (1 test):**
- `minibuffer-is-a-buffer` ❌ (uses activate-minibuffer helper)

**Read-Only (1 test):**
- `read-only-buffer-prevents-modification` ❌ (uses set-read-only)

**Window Tree (1 test):**
- `window-tree-is-a-binary-partition` ❌ (uses split-window helper)

**Total: 14 tests will fail** (out of claimed 24 passing)

### Tests That SHOULD Still Pass

**Buffer Semantics (4 tests):**
- `buffer-can-exist-without-file` ✓ (uses create-buffer + queries)
- `buffer-identity-is-stable` ✓ (uses insert-text event)
- `buffer-can-change-visited-file` ✓ (uses visit-file but testable)
- `multiple-buffers-can-coexist` ✓ (uses create-buffer)

**Point/Mark (2 tests):**
- `point-is-buffer-local` ✓ (uses point/set-point)
- `point-is-independent-per-buffer` ✓ (uses point/set-point)

**Messages (2 tests):**
- `messages-are-buffer-backed-editor-state` ✓ (uses api-msg/message)
- `message-appends-to-history-but-updates-echo-area` ✓ (uses api-msg/message)

**Commands (2 tests):**
- `command-has-metadata-and-identity` ✓ (uses real command registry)
- `all-command-invocations-go-through-dispatch` ✓ (uses dispatch-sync)

**Estimated: 10 tests should still pass** (if their helpers don't have hidden violations)

## Recovery Plan

### Phase 1: Create API Boundary (Day 1)

**File:** `src/lexicon/api/test.cljs`

Public API that tests MUST use. All operations go through re-frame events.

```clojure
(ns lexicon.api.test
  "Test API - ONLY interface tests may use.

  Prevents tests from bypassing application logic."
  (:require [re-frame.core :as rf]))

;; Buffer operations
(defn create-buffer! [name content] ...)
(defn insert-text! [buffer-id position text] ...)
(defn buffer-text [buffer-id] @(rf/subscribe [:buffer/text buffer-id]))

;; Kill ring (will fail until implemented)
(defn kill-region! [buffer-id start end]
  (rf/dispatch-sync [:edit/kill-region buffer-id start end]))

;; Undo (will fail until implemented)
(defn undo! [buffer-id]
  (rf/dispatch-sync [:edit/undo buffer-id]))

;; Keymaps (will fail until implemented)
(defn set-global-key! [key-seq cmd]
  (rf/dispatch-sync [:keymap/set-global key-seq cmd]))

;; etc...
```

### Phase 2: Strip Helpers (Day 1)

**File:** `test/lexicon/semantic/helpers.cljs`

Delete all application logic. Keep ONLY:
- Test setup (`reset-editor-db!`, `with-wasm`)
- Calls to `lexicon.api.test` functions
- Pure queries

### Phase 3: Run Tests - Document Failures (Day 1-2)

Run full test suite. Document:
- Which tests pass (honest baseline)
- Which tests fail (feature gaps)
- Error messages

### Phase 4: Implement Real Features (Day 2-5)

For each failing test:
1. Implement feature in `src/lexicon/events/`
2. Wire up to `lexicon.api.test`
3. Test passes

Priority order:
1. Buffer operations (if any missing)
2. Mode system (2 tests)
3. Window management (3 tests)
4. Minibuffer (1 test)
5. Kill ring (2 tests)
6. Undo (1 test)
7. Keymaps (2 tests)

### Phase 5: Prevent Recurrence

**Linting Rules:**

`.clj-kondo/config.edn`:
```clojure
{:linters
 {:forbidden-namespaces
  {;; Tests cannot access re-frame.db directly
   "test/**" {"re-frame.db" {:level :error
                             :message "Use lexicon.api.test, not re-frame.db"}}}}}
```

**Documentation:**

Add to `.CLAUDE.md`:
```markdown
## Test Helper Rules

**ABSOLUTE RULES for test/lexicon/semantic/helpers.cljs:**

❌ FORBIDDEN:
- Direct state access: `@rfdb/app-db`, `(get-in @rfdb/app-db ...)`
- Direct state mutation: `(swap! rfdb/app-db ...)`, `(assoc-in ...)`
- Implementing application logic (kill ring, undo, keymaps, etc.)
- Direct WASM manipulation (except in test setup)

✅ ALLOWED:
- Test setup: `reset-editor-db!`, `with-wasm`
- Calling `lexicon.api.test/*` functions
- Pure queries via subscriptions
- Test instrumentation (recording dispatch calls, etc.)

**Test helpers are for TESTING, not IMPLEMENTING.**
```

## Metrics

- **Before:** 24/34 tests passing (claimed)
- **After cleanup:** ~10/34 tests passing (honest)
- **Final target:** 34/34 tests passing (with real features)

## Lessons Learned

1. **Direct state access is dangerous** - Tests should never touch `@rfdb/app-db`
2. **APIs enforce boundaries** - `lexicon.api.test` prevents bypassing
3. **Green tests aren't always honest** - Need to verify what's being tested
4. **TDD requires discipline** - Easy to cheat by implementing in helpers

## References

- Epic #86 - Emacs Semantic Compatibility
- `.CLAUDE.md` - Updated test philosophy
- This audit document
