# Events.cljs Refactoring Plan - Phase 7.2

**Start Date:** 2026-01-02
**Duration:** Week 2 of Phase 7
**Current Size:** 3154 lines
**Target:** 8 focused modules (~400 lines each)

## Refactoring Strategy

### Principles

1. **TDD Approach**: Write tests first for each module
2. **Incremental**: Extract one module at a time
3. **Test After Each**: Run full test suite after each extraction
4. **Commit When Green**: Commit immediately after tests pass
5. **No Functionality Changes**: Pure refactoring, zero behavior changes

### Module Breakdown (8 modules)

#### 1. `buffer-events.cljs` (~450 lines)

**Responsibility**: Buffer lifecycle, file I/O, buffer switching

**Event Handlers** (16):
- `:create-buffer`
- `:kill-buffer`
- `:kill-buffer-by-name`
- `:close-buffer`
- `:switch-buffer`
- `:switch-to-buffer`
- `:switch-to-buffer-by-name`
- `:list-buffers`
- `:buffer-saved`
- `:find-file`
- `:save-buffer`
- `:save-buffer-with-hooks`
- `:save-buffer-internal`
- `:write-file`
- `:file-read-success`
- `:file-read-failure`

**Helper Functions**:
- `detect-language-from-filename`
- Buffer creation helpers
- File I/O helpers

---

#### 2. `command-events.cljs` (~400 lines)

**Responsibility**: Command registration, execution, prefix arguments

**Event Handlers** (15):
- `:register-command`
- `:execute-command`
- `:execute-command-by-name`
- `:execute-extended-command`
- `:initialize-commands`
- `:initialize-mode-commands`
- `:update-save-buffer-command`
- `:keyboard-quit`
- `:universal-argument`
- `:clear-prefix-argument`
- `:apropos-command`
- `:apropos-command/show`
- `:describe-key`
- `:describe-key/show`
- `:describe-function`
- `:describe-function/show`
- `:describe-bindings`
- `:help-for-help`

**Helper Functions**:
- Command registry helpers
- Prefix argument handling

---

#### 3. `edit-events.cljs` (~600 lines)

**Responsibility**: Text editing, cursor movement, undo, kill/yank

**Event Handlers** (28):
- `:delete-backward-char`
- `:delete-forward-char`
- `:handle-text-input`
- `:forward-char`
- `:backward-char`
- `:next-line`
- `:previous-line`
- `:forward-word`
- `:backward-word`
- `:beginning-of-line`
- `:end-of-line`
- `:beginning-of-buffer`
- `:end-of-buffer`
- `:kill-line`
- `:kill-region`
- `:open-line`
- `:set-mark-command`
- `:set-mark`
- `:copy-region-as-kill`
- `:yank`
- `:yank-pop`
- `:undo`
- `:undo-complete`
- `:click-to-position`
- `:set-cursor-position`
- `:update-cursor-position`
- `:initialize-buffer-cursor`
- `:set-selection`
- `:ime-commit-composition`
- `:ime-composition-start`
- `:ime-composition-update`
- `:ime-composition-end`

**Helper Functions**:
- `word-char?`
- `find-forward-word-boundary`
- `find-backward-word-boundary`
- `linear-pos-to-line-col`
- `line-col-to-linear-pos`
- Cursor position helpers

---

#### 4. `keymap-events.cljs` (~200 lines)

**Responsibility**: Key sequence handling, keymap lookup

**Event Handlers** (3):
- `:handle-key-sequence`
- `:set-prefix-key-state`
- `:clear-prefix-key-state`

**Helper Functions**:
- Key sequence processing
- Keymap lookup

---

#### 5. `mode-events.cljs` (~350 lines)

**Responsibility**: Major/minor modes, hooks

**Event Handlers** (9):
- `:set-major-mode`
- `:toggle-minor-mode`
- `:enable-minor-mode`
- `:disable-minor-mode`
- `:line-number-mode`
- `:column-number-mode`
- `:add-hook`
- `:remove-hook`
- `:run-hook`

**Helper Functions**:
- Mode activation/deactivation
- Hook management

---

#### 6. `ui-events.cljs` (~500 lines)

**Responsibility**: Rendering, windows, minibuffer, echo area

**Event Handlers** (20):
- `:reconcile-dom-state`
- `:reconcile-dom-if-needed`
- `:set-reconciliation-active`
- `:view-updated`
- `:update-viewport`
- `:set-mutation-observer`
- `:echo/message`
- `:echo/clear`
- `:show-error`
- `:split-window-below`
- `:split-window-right`
- `:delete-window`
- `:delete-other-windows`
- `:other-window`
- `:set-active-window`
- `:minibuffer/activate`
- `:minibuffer/deactivate`
- `:minibuffer/set-input`
- `:minibuffer/complete`
- `:minibuffer/confirm`

**Helper Functions**:
- Window tree manipulation
- DOM reconciliation
- Minibuffer state

---

#### 7. `wasm-events.cljs` (~400 lines)

**Responsibility**: WASM initialization, transactions, parser

**Event Handlers** (19):
- `:wasm-module-loaded`
- `:wasm-load-failed`
- `:dispatch-transaction`
- `:apply-transaction-result`
- `:editor/queue-transaction`
- `:editor/set-flight-status`
- `:editor/transaction-success`
- `:editor/transaction-failure`
- `:transaction-failed`
- `:compound-transaction`
- `:parser/worker-created`
- `:parser/worker-ready`
- `:parser/worker-error`
- `:parser/request-parse`
- `:parser/request-incremental-parse`
- `:parser/parse-error`
- `:parser/ast-updated`

**Helper Functions**:
- Transaction queue management
- Parser worker communication

---

#### 8. `core-events.cljs` (~250 lines)

**Responsibility**: Initialization, coordination, special modes, dev tools

**Event Handlers** (9):
- `:initialize-db`
- `:load-package`
- `:unload-package`
- `:buffer-menu/select-buffer`
- `:ws/connect`
- `:ws/connecting`
- `:ws/opened`
- `:ws/closed`
- `:ws/error`
- `:ws/message-received`
- `:debug/check-parser-state`

**Helper Functions**:
- DB initialization
- Package loading (SCI integration preparation)

---

## Extraction Order

Extract modules in **dependency order** (least dependent → most dependent):

1. **wasm-events.cljs** (no dependencies on other event modules)
2. **keymap-events.cljs** (minimal dependencies)
3. **mode-events.cljs** (depends on hooks, but self-contained)
4. **buffer-events.cljs** (depends on mode for initialization)
5. **edit-events.cljs** (depends on buffer, mode)
6. **command-events.cljs** (depends on buffer, edit)
7. **ui-events.cljs** (depends on buffer, window, minibuffer)
8. **core-events.cljs** (coordinator, depends on all others)

## Extraction Process (Per Module)

### Step 1: Create Test File

```clojure
;; test/lexicon/events/<module>_test.cljs
(ns lexicon.events.<module>-test
  (:require [cljs.test :refer [deftest is testing]]
            [lexicon.events.<module> :as sut]))

(deftest test-event-handler-name
  (testing "Event handler behavior"
    (is (= expected-result (sut/handler-fn ...)))))
```

### Step 2: Create New Module File

```clojure
;; src/lexicon/events/<module>.cljs
(ns lexicon.events.<module>
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; Copy helper functions from events.cljs

;; Copy event handlers from events.cljs
```

### Step 3: Update events.cljs

Remove extracted code, add require:
```clojure
(ns lexicon.events
  (:require [lexicon.events.<module>]))  ;; Auto-registers handlers
```

### Step 4: Run Tests

```bash
bb test:unit        # Unit tests
bb test:e2e         # E2E tests (critical!)
```

### Step 5: Commit When Green

```bash
git add -A
git commit -m "refactor(events): extract <module> module

- Extract N event handlers to lexicon.events.<module>
- Move M helper functions
- All tests passing (X unit, Y E2E)
- Zero behavior changes"
```

---

## Testing Strategy

### Per-Module Tests

Each module gets:
1. **Unit tests** for helper functions
2. **Event handler tests** (mock re-frame)
3. **Integration tests** (with test db)

### Regression Testing

After each extraction:
1. Run **all unit tests**: `bb test:unit`
2. Run **all E2E tests**: `bb test:e2e` (7 tests, 10 assertions)
3. Manual smoke test: Open editor, type, save, switch buffers

### Test Files to Create

```
test/lexicon/events/
├── buffer_events_test.cljs
├── command_events_test.cljs
├── edit_events_test.cljs
├── keymap_events_test.cljs
├── mode_events_test.cljs
├── ui_events_test.cljs
├── wasm_events_test.cljs
└── core_events_test.cljs
```

---

## Success Criteria

- [ ] 8 modules created (~400 lines each)
- [ ] events.cljs reduced to ~50 lines (just requires)
- [ ] All 114 event handlers still registered
- [ ] All unit tests passing
- [ ] All 7 E2E tests passing (0 failures, 0 errors)
- [ ] 8 commits (1 per module extraction)
- [ ] No behavior changes (pure refactoring)

---

## Timeline (Week 2)

| Day | Task | Commits |
|-----|------|---------|
| Day 1 | Extract wasm-events.cljs, keymap-events.cljs | 2 |
| Day 2 | Extract mode-events.cljs, buffer-events.cljs | 2 |
| Day 3 | Extract edit-events.cljs | 1 |
| Day 4 | Extract command-events.cljs, ui-events.cljs | 2 |
| Day 5 | Extract core-events.cljs, final cleanup | 1 |

**Total**: 8 commits, ~40 hours

---

## Rollback Plan

If any step breaks tests:
1. `git reset --hard HEAD~1` (undo last commit)
2. Identify issue
3. Fix and retry
4. Never move forward with broken tests

---

## Notes

- Keep events.cljs as "coordinator" that just requires all modules
- All modules auto-register event handlers on load (re-frame style)
- No circular dependencies (use dependency order)
- Preserve all comments and documentation
- Update ROADMAP.md after completion

---

**Status**: Ready to begin
**Next Action**: Extract wasm-events.cljs (Step 1)
