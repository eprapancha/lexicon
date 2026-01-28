# Semantic Test Honest Baseline

**Date:** 2026-01-17
**Status:** Post-cleanup, awaiting test run

## What Happened

We discovered that 73% of test helpers were implementing application logic instead of testing it. After nuclear reset, we now have an honest baseline.

## Expected Test Results

### Should PASS (~10 tests)

These tests use real application events that actually exist:

**Buffer Semantics (4 tests):**
- ✅ `buffer-can-exist-without-file` - Uses `:create-buffer` event
- ✅ `buffer-identity-is-stable` - Uses `:buffer/insert` event
- ✅ `buffer-can-change-visited-file` - Query-based
- ✅ `multiple-buffers-can-coexist` - Uses `:create-buffer` event

**Point/Mark (2 tests):**
- ✅ `point-is-buffer-local` - Uses point storage in buffer
- ✅ `point-is-independent-per-buffer` - Uses point storage

**Messages (2 tests):**
- ✅ `messages-are-buffer-backed-editor-state` - Uses `api-msg/message`
- ✅ `message-appends-to-history-but-updates-echo-area` - Uses `api-msg/message`

**Commands (2 tests):**
- ✅ `command-has-metadata-and-identity` - Uses `:register-command` event
- ✅ `all-command-invocations-go-through-dispatch` - Uses `:execute-command` event

### Will FAIL (~14 tests)

These tests call events that don't exist yet:

**Kill Ring (2 tests) - Missing `:edit/kill-region`, `:edit/yank`:**
- ❌ `kill-ring-is-global`
- ❌ `consecutive-kills-append`

**Undo (1 test) - Missing `:edit/undo`:**
- ❌ `undo-is-buffer-local`

**Keymaps (2 tests) - Missing `:keymap/*`, `:input/keys`:**
- ❌ `local-keymap-shadows-global-keymap`
- ❌ `prefix-key-waits-for-completion`

**Modes (2 tests) - Missing `:mode/set-major`, `:mode/enable-minor`:**
- ❌ `exactly-one-major-mode-per-buffer`
- ❌ `multiple-minor-modes-can-coexist`

**Windows (3 tests) - Missing `:window/split`, `:window/delete-others`:**
- ❌ `buffer-identity-is-stable-across-views`
- ❌ `window-deletion-preserves-buffer-identity`
- ❌ `window-tree-is-a-binary-partition`

**Minibuffer (1 test) - Missing `:minibuffer/activate`:**
- ❌ `minibuffer-is-a-buffer`

**Read-Only (1 test) - Missing `:buffer/set-read-only`:**
- ❌ `read-only-buffer-prevents-modification`

**Point/Mark (2 tests) - Missing subscriptions:**
- ❌? `point-is-buffer-local` (may fail on subscription)
- ❌? `point-is-independent-per-buffer` (may fail on subscription)

### Compilation Errors Expected

Several API calls will fail because events/subscriptions don't exist:

```clojure
;; These will fail during dispatch:
(rf/dispatch-sync [:edit/kill-region ...])    ; Event not registered
(rf/dispatch-sync [:mode/set-major ...])      ; Event not registered
(rf/dispatch-sync [:window/split ...])        ; Event not registered
(rf/dispatch-sync [:keymap/set-global ...])   ; Event not registered

;; These will fail during subscription:
@(rf/subscribe [:buffer/major-mode ...])      ; Subscription not registered
@(rf/subscribe [:keymap/lookup ...])          ; Subscription not registered
@(rf/subscribe [:input/prefix-state?])        ; Subscription not registered
```

## Implementation Priority

Based on Vertico requirements and test criticality:

### Priority 1: Critical for Vertico (4-5 days)
1. **Keymaps** (2 tests) - REQUIRED for minibuffer
   - Implement `src/lexicon/events/keymap.cljs`
   - Events: `:keymap/set-global`, `:keymap/set-local`
   - Implement `src/lexicon/events/input.cljs`
   - Events: `:input/keys` (key sequence handling)
   - Subscriptions: `:keymap/lookup`, `:input/prefix-state?`, `:input/last-command`

2. **Modes** (2 tests) - For buffer-local behavior
   - Implement `src/lexicon/events/mode.cljs`
   - Events: `:mode/set-major`, `:mode/enable-minor`
   - Subscriptions: `:buffer/major-mode`, `:buffer/minor-mode-enabled?`

3. **Minibuffer** (1 test) - VERTICO GATE
   - Implement `src/lexicon/events/minibuffer.cljs`
   - Events: `:minibuffer/activate`, `:minibuffer/deactivate`
   - Buffer lifecycle management

### Priority 2: User Trust (3-4 days)
4. **Kill Ring** (2 tests) - Copy/paste workflow
   - Implement `src/lexicon/events/edit.cljs`
   - Events: `:edit/kill-region`, `:edit/yank`
   - State: Kill ring with consecutive kill appending

5. **Undo** (1 test) - Data safety
   - Extend `src/lexicon/events/edit.cljs`
   - Event: `:edit/undo`
   - State: Undo stack per buffer

### Priority 3: Window Management (3-4 days)
6. **Windows** (3 tests) - Multi-window editing
   - Implement `src/lexicon/events/window.cljs`
   - Events: `:window/split`, `:window/delete-others`, `:window/show-buffer`
   - Window tree management

7. **Read-Only** (1 test) - Buffer protection
   - Extend `src/lexicon/events/buffer.cljs`
   - Event: `:buffer/set-read-only`
   - Enforcement in insert/delete operations

## Success Criteria

**Honest Progress:**
- Start: ~10/34 tests passing (29%)
- After Priority 1: ~17/34 (50%)
- After Priority 2: ~20/34 (59%)
- After Priority 3: ~24/34 (71%)
- Final: 34/34 (100%)

**Quality Metrics:**
- Zero application logic in test helpers
- All operations go through `lexicon.api.test`
- Tests fail when features don't exist
- Tests pass when features are properly implemented

## Next Step

Run tests and record actual results:

```bash
# Open test page
http://localhost:8021/test.html

# Document:
# - Which tests pass (honest baseline)
# - Which tests fail (feature gaps)
# - Error messages for each failure
```

Then begin Priority 1 implementation.
