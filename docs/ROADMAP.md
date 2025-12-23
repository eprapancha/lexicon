# Lexicon Development Roadmap

**Last Updated:** 2025-12-21
**Current Phase:** Phase 0 Complete! ✅ Ready for Phase 1

---

## Overview

This roadmap tracks Lexicon's evolution from the current state (architecture misalignment, broken basic input) to a working bare Emacs implementation, followed by package ecosystem development.

**Strategy:** Clean refactor to establish solid foundation aligned with Emacs principles. See [CORE_PRINCIPLES.md](./CORE_PRINCIPLES.md) for architectural guidelines.

---

## Phase 0: Architecture Reset & Foundation

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Separate core from packages, fix basic text input, establish clean architecture
**Timeline:** 2 days (Dec 20-21, 2025)

### Objectives

1. **Extract Evil-mode from Core** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Move `fsm/` directory → `packages/evil-mode/src/lexicon/evil/fsm/`
   - ✅ Move `macros.clj` → `packages/evil-mode/src/lexicon/evil/macros.clj`
   - ✅ Move `command/` dispatcher → `packages/evil-mode/src/lexicon/evil/command/`
   - ✅ Move `keymaps/` → `packages/evil-mode/src/lexicon/evil/keymaps/`
   - ✅ Remove FSM state from core `events.cljs`
   - ✅ Core compiles cleanly with 0 errors
   - ✅ Created `evil-mode/README.md` documenting future integration

2. **Replace Piece Tree with Gap Buffer** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Implemented gap buffer in Rust (350 lines, clean and simple)
   - ✅ WASM bindings created (`gap_buffer_wasm.rs`)
   - ✅ 15 comprehensive unit tests (all passing)
   - ✅ UTF-8 correctness validated (emoji support)
   - ✅ Fixed nix-shell TMPDIR issue for testing

3. **Fix Core Text Input** ✅ COMPLETE (Dec 21, 2025)
   - ✅ Integrated gap buffer into ClojureScript
   - ✅ Basic typing works (characters appear on screen)
   - ✅ Enter key inserts newline and advances cursor
   - ✅ Backspace deletes character before cursor
   - ✅ Delete key deletes character at cursor
   - ✅ Arrow keys move cursor (with C-f/C-b/C-n/C-p bindings)
   - ✅ Mouse click positions cursor
   - ✅ Multi-line editing works correctly
   - ✅ Fixed cursor position calculation for newlines
   - ✅ Fixed line rendering to show empty lines

4. **Restructure Directories**
   ```
   packages/
   ├── lexicon-core/
   │   ├── wasm/              # Rust gap buffer + bindings
   │   └── cljs/              # ClojureScript core
   │       └── src/lexicon/core/
   │           ├── buffer.cljs
   │           ├── command.cljs
   │           ├── keymap.cljs
   │           ├── mode.cljs
   │           └── ...
   └── evil-mode/             # First package
       └── src/lexicon/evil/
   ```

5. **Update Documentation** ✅ COMPLETE (Dec 20, 2025)
   - ✅ Created CORE_PRINCIPLES.md - Architectural foundation
   - ✅ Created ROADMAP.md - This document
   - ✅ Updated README.md - Reflects reality (broken features marked)
   - ✅ Updated architecture.md - Gap buffer design documented

### Success Criteria

- [x] **Can type text and see it on screen** ✅ Dec 21
- [x] **Enter key creates new line** ✅ Dec 21
- [x] **Backspace deletes characters** ✅ Dec 21
- [x] **Arrow keys move cursor** ✅ Dec 21
- [x] **Delete key works** ✅ Dec 21
- [x] **Mouse click positions cursor** ✅ Dec 21
- [x] **Core compiles without Evil-mode** ✅ Dec 20
- [x] **Evil-mode code completely moved to `packages/evil-mode/`** ✅ Dec 20
- [x] **Gap buffer passes all unit tests** ✅ Dec 20 (15/15 tests)
- [x] **README accurately describes current state** ✅ Dec 20

**Progress:** 10/10 complete (100%) ✅ PHASE 0 COMPLETE!

### Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Hidden Evil-mode dependencies in core | High | Grep entire codebase for `fsm`, `evil`, `operator-pending` |
| Gap buffer bugs with UTF-8 | High | Comprehensive unit tests with emoji, multi-byte chars |
| Transaction queue adds complexity | Medium | Simplify to direct synchronous calls for Phase 0, optimize later |

---

## Phase 1: Core Emacs - Basic Editing

**Status:** ✅ COMPLETE (Dec 21, 2025)
**Goal:** Implement essential Emacs editing commands
**Timeline:** 1 day (Dec 21, 2025)
**Prerequisites:** Phase 0 complete

### Core Commands to Implement

#### Navigation
- `forward-char` (C-f) / `backward-char` (C-b)
- `next-line` (C-n) / `previous-line` (C-p)
- `beginning-of-line` (C-a) / `end-of-line` (C-e)
- `beginning-of-buffer` (M-<) / `end-of-buffer` (M->)
- `forward-word` (M-f) / `backward-word` (M-b)

#### Editing
- `self-insert-command` - Insert printable characters
- `newline` (RET)
- `delete-backward-char` (DEL) / `delete-forward-char` (C-d)
- `kill-line` (C-k)
- `open-line` (C-o)

#### Selection & Kill Ring
- `set-mark-command` (C-SPC)
- `kill-region` (C-w)
- `copy-region-as-kill` (M-w)
- `yank` (C-y)
- `yank-pop` (M-y) - Cycle through kill ring

#### Undo
- `undo` (C-/) / (C-_)

### Success Criteria

- [x] All navigation commands work correctly (C-f/b/n/p, C-a/e, M-</>, M-f/b)
- [x] Can select text with mark and region (C-SPC, M-w, C-w via M-x)
- [x] Kill ring works (cut, copy, paste, C-k, C-y, M-y)
- [x] Undo works for all operations (C-/, C-_)
- [x] Manual testing checklist passes

**Progress:** 5/5 complete (100%) ✅ PHASE 1 COMPLETE!

### Known Gaps (Post-completion)

**🎨 Visual Feedback - Region Highlighting**
- **Issue:** Region (mark to point) not visually highlighted
- **Impact:** Users can't see what they're selecting with C-SPC + motion
- **Commands affected:** `kill-region` (C-w), `copy-region-as-kill` (M-w), any region-based command
- **Priority:** High - poor UX without visual feedback
- **Effort:** ~30 minutes (CSS + render logic when mark active)
- **Should be fixed:** Before Phase 3

**⌨️ Prefix Arguments - Universal Argument (C-u)**
- **Issue:** No support for numeric prefix arguments
- **Impact:** Can't do `C-u 10 C-n` (move 10 lines), `C-u C-k` (kill including newline), etc.
- **Priority:** High - fundamental Emacs feature
- **Effort:** 2-3 hours
  - Add `:prefix-argument` state to app-db
  - C-u sets multiplier (4, 16, 64...)
  - Modify commands to accept/consume prefix arg
  - Show "C-u -" in mode line during input
- **Should be fixed:** Before Phase 3 (simpler than doing it after window splits)

---

## Technical Debt & Code Quality Improvements

**Source:** ClojureScript code review (Dec 21, 2025)
**Status:** Ongoing - address incrementally across phases

### 🚨 Critical (Before Phase 2)

#### 1. Subscription Caching & Performance
**Issue:** Subscriptions recompute derived data eagerly, calling WASM on every render
```clojure
;; Current: calls .getText on every subscription evaluation
(rf/reg-sub ::visible-lines
  (fn [wasm-instance ...]
    (.getText wasm-instance)))  ;; EXPENSIVE!
```

**Impact:** Performance degradation with multiple buffers, large files
**Solution:**
- Add `:editor-version` to buffer state (increment on each edit)
- Cache `:full-text` and `:line-count` in buffer state
- Invalidate cache only when version changes
- Make subscriptions use cached values

**Timeline:** Before Phase 2 starts
**Estimated Effort:** 1-2 hours

---

### ⚠️ High Priority (During Phase 2)

#### 2. Overloaded app-db Structure
**Issue:** Mixing authoritative state, derived state, and UI state
```clojure
;; Current - unclear boundaries:
{:buffers {1 {:wasm-instance ...        ;; Authoritative (Rust)
              :cursor-position {:line 0} ;; Derived from Rust
              :undo-stack []}}           ;; Meta-state
 :ui {:cursor-position 0}}               ;; Duplicate!
```

**Impact:** Risk of state divergence, unclear data flow
**Solution:** Clearer state boundaries
```clojure
{:editor/core   {1 {:wasm-instance ... :version 42}}  ;; Opaque Rust state
 :editor/view   {1 {:cursor ... :selection ...}}      ;; Derived view state
 :editor/cache  {1 {:text ... :lines ...}}            ;; Cached computations
 :editor/meta   {1 {:undo-stack ... :modified?}}      ;; Editor metadata
 :ui            {:active-buffer-id 1}}                ;; UI-only state
```

**Timeline:** Refactor during Phase 2 buffer management work
**Estimated Effort:** 4-6 hours

---

#### 3. Large Event Handlers - Excessive Complexity
**Issue:** `:editor/process-queue` is 100+ lines with side effects
```clojure
(rf/reg-fx :editor/process-queue
  (fn [db-state]
    ;; 100+ lines of:
    ;; - WASM calls
    ;; - db reads
    ;; - state updates
    ;; - nested case statements
```

**Impact:** Hard to test, reason about, debug
**Solution:** Extract into composable functions
```clojure
(defn apply-insert [wasm-instance pos text] ...)
(defn apply-delete [wasm-instance pos len] ...)
(defn record-undo-entry [operation] ...)

(rf/reg-fx :editor/process-queue
  (fn [db-state]
    (let [operation (first-operation db-state)]
      (apply-operation wasm-instance operation))))
```

**Timeline:** Refactor during Phase 2
**Estimated Effort:** 3-4 hours

---

### 📋 Medium Priority (Phase 3-4)

#### 4. Error Handling & Observability
**Issue:** Errors are unstructured strings, no recovery strategy
```clojure
;; Current:
{:error "Something broke"}

;; Better:
{:type :wasm-error
 :severity :error
 :message "..."
 :context {:operation :insert :position 42}
 :recoverable? true}
```

**Timeline:** Phase 3-4 (when adding LSP diagnostics)
**Estimated Effort:** 2-3 hours

---

#### 5. Potential Over-rendering in Views
**Issue:** Large editor state passed to components may cause excessive re-renders
**Solution:**
- Fine-grained subscriptions
- Pass minimal props to components
- Use `reaction` for stable references

**Timeline:** Phase 3-4 (as performance issues emerge)
**Estimated Effort:** 2-4 hours

---

### 📝 Low Priority (Ongoing)

#### 6. Testing Coverage
**Issue:** No meaningful ClojureScript tests
**Impact:** Hard to refactor safely, regression risk
**Solution:**
- Unit test event handlers (pure functions)
- Test command dispatch logic
- Mock WASM for deterministic tests
- Property-test command sequences

**Timeline:** Add tests incrementally starting Phase 2
**Target:** 70% coverage by Phase 5
**Estimated Effort:** Ongoing (1-2 hours per phase)

---

#### 7. Documentation Gaps
**Issue:**
- No docstrings on public events/subs
- No CLJS architecture documentation
- No state flow diagrams

**Solution:**
- Add `docs/ARCHITECTURE_CLJS.md`
- Document data flow: `UI → re-frame → WASM → patch → UI`
- Add docstrings to all `reg-event-*` and `reg-sub`
- Create state transition diagrams

**Timeline:** Ongoing, improve incrementally
**Estimated Effort:** 1 hour per phase

---

### ✅ Non-Issues (Reviewer Misunderstood)

#### String-based WASM Protocol
**Reviewer claimed:** Using JSON strings for WASM communication
**Reality:** We use **wasm-bindgen** with compile-time type safety
```clojure
(.insert wasm-instance position text)  ;; Direct typed calls
;; NOT: (.applyTransaction wasm-editor json-str)
```
**No action needed.**

#### Patch Generation Logic
**Reviewer claimed:** Patch-based operational transforms
**Reality:** We use **direct gap buffer mutations**, no patches
**No action needed.**

---

### Progress Tracking

| Item | Priority | Phase | Status |
|------|----------|-------|--------|
| Subscription caching | 🚨 Critical | Before Phase 2 | ✅ Complete |
| app-db structure | ⚠️ High | Phase 2 | 🔲 Planned |
| Event handler refactor | ⚠️ High | Phase 2 | 🔲 Planned |
| Error handling | 📋 Medium | Phase 3-4 | 🔲 Planned |
| Over-rendering | 📋 Medium | Phase 3-4 | 🔲 Planned |
| Testing coverage | 📝 Low | Ongoing | 🔲 Planned |
| Documentation | 📝 Low | Ongoing | 🔲 Planned |

---

## Phase 2: Core Emacs - Buffers & Files

**Status:** ✅ Complete
**Goal:** Multi-buffer editing with file I/O
**Timeline:** 2 weeks (Jan 24 - Feb 7, 2025)
**Prerequisites:** ✅ Phase 1 complete, ✅ Subscription caching complete

### Features

#### Buffer Management
- `switch-to-buffer` (C-x b) - With minibuffer completion
- `kill-buffer` (C-x k)
- `list-buffers` (C-x C-b)
- `buffer-menu` - Interactive buffer selection
- Buffer list tracking

#### File Operations
- `find-file` (C-x C-f) - With file picker
- `save-buffer` (C-x C-s)
- `write-file` (C-x C-w) - Save as
- `insert-file` - Insert file contents at point
- Auto-detect file type for major mode

#### Buffer State
- Modified indicator (*scratch*)
- File association
- Major mode display

### Success Criteria

- [x] Can create, switch, and kill buffers
- [x] File open/save works reliably
- [x] Modified buffers show indicator
- [x] Switching buffers preserves state (cursor, content)
- [x] Can have multiple buffers open simultaneously

---

## Phase 2.5: Core Polish (Before Phase 3)

**Status:** ✅ **COMPLETE** (Dec 21, 2025)
**Goal:** Fix critical UX gaps from Phase 1-2 before adding window complexity
**Timeline:** 7.5 hours total (Show-stoppers: 3h, Usability: 4.5h)
**Prerequisites:** ✅ Phase 2 complete

### 🚨 SHOW-STOPPER Issues ✅ **ALL FIXED** (Dec 21, 2025)

**Problem 1: Layout/Focus - Two-area editor with dead zone** ✅ **FIXED**
- **Symptom:** Content only appears in narrow left column (~60px gutter width)
- **Symptom:** Huge dark empty area on right doesn't accept clicks/focus
- **Symptom:** Must click in specific narrow area to type
- **Symptom:** M- commands don't work until after clicking in editor
- **Impact:** **CRITICAL** - Cannot type without mouse, defeats entire purpose of Emacs
- **Root Cause:** `.editable-area` not expanding to fill `.text-container`
- **Fix:** (1 hour)
  - Ensure `.text-container` flex:1 fills parent width
  - Ensure `.editable-area` width: 100% or inherits container width
  - Add explicit width calculations if needed
  - Fix z-index/pointer-events issues preventing click propagation

**Problem 2: Focus management - Unpredictable focus behavior** ✅ **FIXED**
- **Symptom:** Sometimes can't type without explicitly clicking in editor
- **Symptom:** Minibuffer doesn't reliably return focus to buffer after cancel
- **Impact:** **CRITICAL** - Using mouse in Emacs is "the biggest joke"
- **Solution implemented:** Added :focus-editor effect, minibuffer returns focus on cancel

**Problem 3: C-g in minibuffer - Not working** ✅ **FIXED (Already worked)**
- **Symptom:** Esc works to cancel minibuffer, but C-g doesn't
- **Impact:** HIGH - C-g is THE Emacs "get out of jail" key
- **Solution:** Was already implemented, verified working (views.cljs:530)

**Problem 4: Horizontal scrollbar - Shouldn't exist** ✅ **FIXED**
- **Symptom:** Ugly horizontal scrollbar just above mode line
- **Impact:** MEDIUM - Visual pollution, suggests layout issue
- **Solution implemented:** Added `overflow-x: hidden` to .editor-scroller

**Problem 5: No echo area - Messages have nowhere to go** ✅ **FIXED**
- **Symptom:** Mode line at very bottom, no blank line below for messages
- **Impact:** MEDIUM - Can't show transient messages (saving, errors, etc.)
- **Solution implemented:** Added .echo-area component, shows "Wrote <file>", auto-clears

### Critical UX Issues (Makes Phase 2 frustrating to use)

**Problem 6: C-x b (switch-to-buffer) - No completion**
- With multiple buffers, must type exact name character-by-character
- No way to see available buffer names
- No TAB completion or partial matching
- **Impact:** Unusable with 5+ buffers

**Problem 7: C-x C-b (list-buffers) - Read-only buffer list**
- Shows buffer list but can't interact with it
- Must memorize buffer name, exit, then use C-x b
- Should be able to navigate and press RET to switch
- **Impact:** Feature is essentially broken

### Essential Additions (In Priority Order)

#### 0. FIX SHOW-STOPPERS FIRST (3 hours)
**THESE MUST BE FIXED BEFORE ANYTHING ELSE**
1. Layout/Focus - Two-area editor (1 hour) - **BLOCKING**
2. Focus management (30 min) - **BLOCKING**
3. C-g in minibuffer (15 min) - **BLOCKING**
4. Horizontal scrollbar (15 min)
5. Echo area for messages (30 min)
6. Test thoroughly - ensure can type anywhere, focus is predictable

#### 1. Region Highlighting (30 min) - HIGH PRIORITY
**Why now:** Phase 1 commands (C-w, M-w) unusable without visual feedback
- Add CSS for `.selected-region` with background color
- Update view to render highlighted spans when mark is active
- Calculate mark position to cursor position range

#### 2. Minibuffer TAB Completion for C-x b (1-2 hours) - **CRITICAL**
**Why now:** Cannot effectively use multiple buffers without this
- Add `:completion-candidates` to minibuffer state
- Implement TAB handler to show/cycle completions
- Display completion list (simple format below minibuffer input)
- Partial name matching (prefix-based)
- This is BASIC Emacs - not a "nice to have"

**Simple completion UI:**
```
Switch to buffer: sh[TAB]
Completions: *scratch*, shell.nix
```

#### 3. Interactive buffer-menu-mode (1 hour) - **CRITICAL**
**Why now:** C-x C-b currently unusable for buffer switching
- Add special keybindings for *Buffer List* buffer
- `RET` on a line → switch to that buffer
- Parse line to extract buffer name
- Basic navigation with arrow keys (already works)
- Foundation: This IS a major mode (buffer-menu-mode)

#### 4. Basic Help - describe-bindings (1 hour)
**Why now:** Need to discover implemented commands as list grows
- `describe-bindings` (C-h b) - List all keybindings in buffer
- Simple implementation: format keymap data, no completion yet
- Foundation for full help system in Phase 4

#### 5. Universal Argument (C-u) (2-3 hours) - Can defer to Phase 2.6
**Why later:** Useful but not blocking; simpler before window splits
- Add `:prefix-argument` state to app-db
- Implement C-u handler (accumulates 4, 16, 64...)
- Modify motion commands to accept count parameter
- Modify editing commands (C-k, etc.) for prefix arg behavior
- Show "C-u -" or "C-u 4" in mode line

### Success Criteria
- [x] **Can type anywhere in editor without clicking narrow column first** ← CRITICAL ✅ FIXED
- [x] **M- commands work immediately after app loads** ← CRITICAL ✅ FIXED
- [x] **C-g cancels minibuffer and returns focus to editor** ← CRITICAL ✅ FIXED
- [x] No horizontal scrollbar visible ✅ FIXED
- [x] Echo area shows messages (e.g., "Saving...", "Saved") ✅ FIXED
- [x] Can see region selection visually ✅ FIXED
- [x] C-x b shows completions on TAB, can select with arrow keys ✅ FIXED
- [x] C-x C-b allows RET to switch to buffer on current line ✅ FIXED
- [x] C-h b lists all current keybindings ✅ FIXED
- [x] C-u sets prefix argument (4, 16, 64...) ✅ FIXED

### Implementation Priority
**PHASE 1 - SHOW-STOPPERS (Must fix first):** ✅ **COMPLETE**
1. ✅ Layout/Focus - Fix two-area editor (1 hour) - Buffer fills viewport
2. ✅ Focus management - Reliable focus (30 min) - M-x works on load
3. ✅ C-g in minibuffer (15 min) - Both Esc and C-g work
4. ✅ Horizontal scrollbar removal (15 min) - Clean UI
5. ✅ Echo area implementation (30 min) - Shows "Wrote <file>"
6. ✅ **THOROUGH TESTING** - Basic typing works everywhere

**PHASE 2 - USABILITY:** ✅ **COMPLETE**
7. ✅ Region highlighting (30 min) - Visual selection feedback
8. ✅ TAB completion for C-x b (1 hour) - Buffer name completion
9. ✅ Interactive buffer-menu-mode (45 min) - RET to switch buffers
10. ✅ C-h b describe-bindings (1 hour) - List all keybindings
11. ✅ C-u universal argument (1.5 hours) - Prefix argument support

**Total Time Spent:**
- Show-stoppers: ✅ COMPLETE (3 hours)
- Core usability: ✅ COMPLETE (4.5 hours)
- **Phase 2.5 Total: 7.5 hours**

---

## Phase 3: Core Emacs - Windows & Frames

**Status:** ✅ COMPLETE (Dec 22, 2025)
**Goal:** Window splitting and management
**Timeline:** Completed Dec 22, 2025
**Prerequisites:** ✅ Phase 2.5 complete

### Phase 3A: Window Tree Infrastructure ✅ COMPLETE

#### Data Structure (✅ Complete)
- [x] Window tree binary tree structure (:hsplit, :vsplit, :leaf nodes)
- [x] Window helper functions (find, update, get-all-leaf-windows)
- [x] Per-window cursor and mark positions
- [x] :next-window-id counter for ID generation

#### Commands (✅ Complete)
- [x] `split-window-below` (C-x 2) - Creates horizontal split
- [x] `split-window-right` (C-x 3) - Creates vertical split
- [x] `other-window` (C-x o) - Navigate through windows
- [x] `delete-other-windows` (C-x 1) - Keep only active window
- [x] `delete-window` (C-x 0) - Stub (needs tree rebalancing)

#### Integration (✅ Complete)
- [x] Updated subscriptions to use window tree
- [x] Updated 34+ events to use window tree
- [x] Cursor/mark now per-window (not per-buffer)
- [x] All commands registered with keybindings

### Phase 3B: Visual Multi-Window Rendering ✅ COMPLETE

#### Recursive Layout Rendering (✅ Complete)
- [x] Render window tree recursively
- [x] Calculate split dimensions (50/50 flexbox)
- [x] Render multiple editor panes simultaneously
- [x] Show active window indicator (2px blue border)

#### Per-Window State (✅ Complete)
- [x] Parameterized subscriptions for window-specific data
- [x] Each window shows independent cursor position
- [x] Each window has independent viewport
- [x] Click-to-activate window functionality

#### Window Deletion (Partial)
- [ ] Implement tree rebalancing for delete-window
- [ ] Reclaim space when window is deleted

### 🐛 Critical Regression Fixed (Dec 22, 2025)

**Issue:** Cursor did not move when typing (stuck at line 1, col 0) ✅ FIXED
- Text insertion worked correctly (characters appeared at correct positions)
- Backspace and Return worked correctly
- Cursor remained stuck at initial position `{:line 0 :column 0}`
- Status bar also showed "Line 1, Col 0" permanently

**Root Cause:** Phase 3 moved cursor-position from buffer to window, but `:editor/transaction-success` event handler still updated the old buffer path `[:buffers ... :cursor-position]` which no longer affects rendering. The window tree's cursor-position was never updated.

**Fix Applied:** `events.cljs:1573-1585` - Calculate new window tree with updated cursor position using `db/update-window-in-tree`, then update `(assoc :window-tree new-window-tree)` instead of the old buffer cursor path.

### Success Criteria

- [x] Window tree data structure works
- [x] Can create splits (data structure updates correctly)
- [x] Can navigate between windows (C-x o cycles)
- [x] Cursor/mark are per-window
- [x] **Visual rendering shows multiple windows**
- [x] Active window shows blue border
- [x] Click to activate windows
- [x] **Cursor moves when typing (Fixed)**
- [ ] Same buffer in multiple windows displays correctly (untested)
- [ ] Deleting windows rebalances tree and reclaims space

### Current Status

**Phase 3 Complete:** Full window management system implemented and working. Windows can be split horizontally (C-x 2) or vertically (C-x 3), navigated (C-x o), and managed (C-x 1 to keep only active). Visual rendering shows multiple windows simultaneously with recursive tree traversal. Each window has independent cursor position. Active window indicated by blue border. Click-to-activate works.

---

## Phase 4: Core Emacs - Minibuffer & Completion

**Status:** ⏳ In Progress (Dec 22, 2025)
**Goal:** Interactive command input and completion
**Timeline:** Started Dec 22, 2025
**Prerequisites:** ✅ Phase 3 complete

### Features

#### Minibuffer Basics (✅ Complete - Phase 2.5)
- [x] `minibuffer-mode` - Special mode for minibuffer
- [x] Minibuffer activation/deactivation
- [x] Prompt display
- [x] Input reading

#### Reading Functions (Partial)
- [ ] `read-string` - Simple string input
- [ ] `read-number` - Number input
- [x] `read-buffer` - Buffer name with completion (Phase 2.5)
- [ ] `read-file-name` - File name with completion **TODO**
- [x] `completing-read` - Generic completion (via minibuffer)

#### Completion (✅ Complete - Phase 2.5)
- [x] Basic prefix completion (TAB)
- [x] Completion candidates display (echo area)
- [x] Completion cycling through matches
- [x] Common prefix expansion

#### M-x Command (✅ Complete - Phase 2)
- [x] `execute-extended-command` (M-x)
- [x] Command name completion
- [ ] Show keybinding if available **TODO**
- [ ] Recent commands tracking **TODO**

#### Help System (C-h) (✅ Complete - Dec 22, 2025)
- [x] `describe-key` (C-h k) - Show what a key binding does
- [x] `describe-function` (C-h f) - Describe a command/function with keybindings
- [x] `describe-bindings` (C-h b) - List all current key bindings
- [x] `apropos-command` (C-h a) - Search commands by keyword/regex
- [x] `help-for-help` (C-h ?) - Show help menu
- [x] Command registry for introspection
- [x] Keybinding reverse lookup (command → key)

### Success Criteria

- [x] M-x works with completion
- [ ] File completion works in `find-file` **TODO**
- [x] Buffer completion works in `switch-to-buffer`
- [x] TAB completes, RET confirms, C-g cancels
- [x] Minibuffer shows prompt and input correctly
- [x] C-h k shows keybinding documentation
- [x] C-h b lists all keybindings in a buffer
- [x] C-h f describes any command
- [x] C-h a searches commands by pattern
- [x] C-h ? shows help menu

### Remaining Work

1. **File name completion** - Implement browser File System Access API integration for `find-file`
2. **read-file-name function** - Generic file reading with completion
3. **M-x enhancements** - Show keybinding hints, track recent commands

---

## Phase 5: Core Emacs - Modes & Keymaps

**Status:** ✅ COMPLETE (Dec 22, 2025)
**Goal:** Robust major/minor mode system
**Timeline:** Completed Dec 22, 2025
**Prerequisites:** ✅ Phase 4 complete

### Features

#### Major Modes (✅ Complete)
- [x] `fundamental-mode` - Base mode (already existed)
- [x] `text-mode` - Plain text editing (already existed)
- [x] `clojure-mode` - ClojureScript mode (basic) (already existed)
- [x] Mode-specific hooks run on activation (NEW: `text-mode-hook`, etc.)
- [ ] Mode inheritance (derive-mode concept) **Deferred to Phase 6**
- [ ] Syntax tables (basic) **Deferred to Phase 9 (Tree-sitter)**

#### Minor Modes (✅ Complete)
- [ ] `auto-fill-mode` - Automatic line wrapping **Deferred to Phase 7**
- [x] `line-number-mode` - Toggle line numbers in status bar (NEW)
- [x] `column-number-mode` - Toggle column number in status bar (NEW)
- [x] Minor mode toggling (already existed)

#### Mode Hooks (✅ Complete)
- [x] `after-change-hook` (already existed)
- [x] `before-save-hook` (already existed)
- [x] `after-save-hook` (already existed)
- [x] Mode-specific hooks (NEW: `text-mode-hook`, `fundamental-mode-hook`, etc.)
- [x] `:add-hook` and `:remove-hook` commands (NEW)

#### Keymap Refinement (✅ Already Complete)
- [x] Keymap inheritance (major/minor/global precedence)
- [x] Sparse keymaps (only define needed bindings)
- [x] Prefix command setup (C-x, C-h, M-, etc.)
- [x] Custom prefix keys (via keymap structure)

### Success Criteria

- [x] Can switch major modes
- [x] Major modes have distinct keymaps
- [x] Minor modes can be toggled on/off
- [x] Hooks fire at appropriate times (mode hooks run on activation)
- [x] Keymap precedence works correctly (already working)
- [x] line-number-mode and column-number-mode toggle display

---

## Phase 6: Package System & Evil-mode

**Status:** 🔲 Planned
**Goal:** External package loading, Evil-mode as first package
**Timeline:** 3 weeks (Mar 18 - Apr 8, 2025)
**Prerequisites:** Phase 5 complete

### Package System

#### Package Manager (Simple)
```clojure
(lexicon.packages/load-package! :evil-mode)
(lexicon.packages/package-loaded? :evil-mode)
(lexicon.packages/package-list)
```

#### Package Structure
```
packages/evil-mode/
├── package.edn        # Metadata
└── src/lexicon/evil/
    ├── core.cljs      # (initialize!) function
    ├── fsm.cljs
    ├── operators.cljs
    ├── motions.cljs
    └── keymaps.cljs
```

#### Package API
- Packages register commands via `lexicon.core.command/register-command!`
- Packages register keymaps via `lexicon.core.keymap/define-keymap`
- Packages define minor modes via `lexicon.core.mode/define-minor-mode`
- Packages add hooks via `lexicon.core.hooks/add-hook`

### Evil-mode Package

#### Features
- Normal, Insert, Visual modes (FSM state machine)
- Operators: `d`, `c`, `y`, `>`, `<`
- Motions: `w`, `b`, `e`, `$`, `0`, `gg`, `G`
- Text objects: `iw`, `aw`, `i"`, `a"`
- Operator-motion composition (`dw`, `ciw`, etc.)
- Evil-specific keymaps

#### Integration
- Toggle with `(evil-mode 1)` / `(evil-mode -1)`
- Coexists with Emacs keybindings
- Can be disabled without breaking core

### Success Criteria

- [ ] Package loading system works
- [ ] Evil-mode loads as external package
- [ ] Evil normal mode works (hjkl navigation)
- [ ] Evil insert mode works (i, a, o, O)
- [ ] Operators + motions work (dw, ciw, etc.)
- [ ] Disabling Evil-mode returns to pure Emacs
- [ ] No Evil code in `lexicon-core/` directory

---

## Phase 6.5: Testing & Quality Assurance

**Status:** 🔲 Planned
**Goal:** Comprehensive automated testing for core stability
**Timeline:** 2 weeks (Before public release)
**Prerequisites:** Phase 6 complete
**Priority:** CRITICAL - Required before accepting external contributions

### Rationale

Before opening the project to external contributors and accepting pull requests, we need:
- Confidence that changes don't break existing functionality
- Automated regression testing for critical workflows
- Test infrastructure that contributors can extend
- Clear quality bar for new features

### Test Infrastructure

#### Unit Tests (ClojureScript + cljs.test)
- **Command execution**: Test all registered commands in isolation
- **Text operations**: Insert, delete, undo/redo correctness
- **Window management**: Split, navigate, delete operations
- **Buffer operations**: Create, switch, kill buffer logic
- **Cursor movement**: All motion commands (C-f, C-b, C-n, C-p, M-f, M-b, etc.)
- **Kill ring**: Copy, kill, yank operations
- **Helper functions**: `linear-pos-to-line-col`, keymap resolution, etc.

#### Integration Tests
- **Multi-window workflows**: Split → edit → navigate → merge
- **File operations**: Open → edit → save → reopen verification
- **Undo/redo chains**: Complex edit sequences with undo
- **Command sequences**: C-u prefix args, multi-key bindings (C-x C-f)
- **Minibuffer**: Completion, confirmation, cancellation flows
- **Help system**: C-h k, C-h f, C-h b output correctness

#### Regression Tests (Critical Workflows)
- **Basic editing**: Type text, move cursor, delete, verify buffer state
- **Phase 3 regression**: Window tree cursor position updates (the bug we fixed)
- **Line ending handling**: Newlines, empty lines render correctly
- **UTF-8 correctness**: Emoji, multi-byte characters
- **Region operations**: Mark → select → kill → yank
- **Window state isolation**: Multiple windows showing same buffer, independent cursors

#### WASM/Rust Unit Tests (Already Exists)
- [x] Gap buffer operations (15 tests in `gap_buffer_core.rs`)
- [x] UTF-8 boundary handling
- [x] Insert/delete edge cases
- [ ] Extend coverage for new Rust functionality

### Testing Tools

#### Framework: cljs.test
```clojure
(ns lexicon.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [lexicon.events :as events]
            [lexicon.db :as db]))

(deftest test-forward-char
  (testing "C-f moves cursor forward by one character"
    (let [db (-> db/default-db
                 (assoc-in [:buffers 1 :wasm-instance] (mock-wasm "hello"))
                 (assoc-in [:ui :cursor-position] 0))
          result (events/forward-char {:db db} nil)]
      (is (= 1 (get-in result [:db :ui :cursor-position]))))))
```

#### Test Runners
- **Development**: `npm run test` - Run all tests with watch mode
- **CI/CD**: `npm run test:ci` - Single run with coverage report
- **Coverage**: Aim for >80% coverage of core commands

#### Mock Infrastructure
- **Mock WASM**: Lightweight gap buffer mock for unit tests
- **Mock File System**: In-memory FS for file operation tests
- **Mock Bridge**: Test WebSocket communication without server

### Test Organization

```
packages/editor-cljs/test/
├── lexicon/
│   ├── commands_test.cljs      # All command handlers
│   ├── events_test.cljs        # Event handler logic
│   ├── subs_test.cljs          # Subscription correctness
│   ├── window_test.cljs        # Window management
│   ├── buffer_test.cljs        # Buffer operations
│   ├── editing_test.cljs       # Text editing operations
│   ├── killring_test.cljs      # Kill ring operations
│   └── integration/
│       ├── workflows_test.cljs # End-to-end workflows
│       └── regression_test.cljs # Known bug regressions
└── test_helpers.cljs           # Shared test utilities
```

### Success Criteria

- [ ] 80%+ code coverage on core command handlers
- [ ] All Phase 0-6 features have regression tests
- [ ] CI/CD pipeline runs tests on every commit
- [ ] Test suite runs in <10 seconds
- [ ] Contributing guide includes "write tests for new features"
- [ ] No PR merged without tests (enforced by CI)

### Deliverables

1. **Test suite**: Comprehensive tests for all core functionality
2. **CI/CD pipeline**: GitHub Actions workflow running tests
3. **Coverage reports**: Track test coverage over time
4. **Test documentation**: Guide for writing tests, mocking patterns
5. **Contributing guidelines**: Testing requirements for PRs

### Timeline

- **Week 1**: Unit tests for commands, events, window management
- **Week 2**: Integration tests, regression tests, CI/CD setup

**This phase MUST be complete before:**
- Opening issues for external contributions
- Accepting pull requests from others
- Any "1.0" or public release announcement

---

## Phase 7: Advanced Editing Features

**Status:** 🔲 Planned
**Goal:** Polish core editing experience
**Timeline:** 2 weeks (Apr 8 - Apr 22, 2025)
**Prerequisites:** Phase 6 complete

### Features

#### Search & Replace
- `isearch-forward` (C-s)
- `isearch-backward` (C-r)
- `query-replace` (M-%)
- `occur` - List matching lines

#### Advanced Editing
- `transpose-chars` (C-t)
- `transpose-words` (M-t)
- `capitalize-word` (M-c)
- `upcase-word` (M-u)
- `downcase-word` (M-l)
- `fill-paragraph` (M-q)

#### Rectangle Commands
- `rectangle-mark-mode` (C-x SPC)
- `kill-rectangle` (C-x r k)
- `yank-rectangle` (C-x r y)
- `string-rectangle` (C-x r t)

#### Macros (Keyboard)
- `kmacro-start-macro` (F3 / C-x ()
- `kmacro-end-macro` (F4 / C-x ))
- `kmacro-call-macro` (F4 / C-x e)

### Success Criteria

- [ ] Incremental search works
- [ ] Query-replace works
- [ ] Rectangle operations work
- [ ] Keyboard macros record and replay
- [ ] Text transformation commands work

---

## Phase 8: Package Ecosystem

**Status:** 🔲 Planned
**Goal:** Essential packages for modern editing
**Timeline:** 4 weeks (Apr 22 - May 20, 2025)
**Prerequisites:** Phase 7 complete

### Packages to Implement

#### Vertico (Completion UI)
- Vertical completion interface
- Candidate preview
- Filtering as you type
- Integration with completing-read

#### Orderless (Completion Style)
- Flexible matching (space-separated terms)
- Out-of-order matching
- Component highlighting

#### Consult (Enhanced Commands)
- `consult-buffer` - Better buffer switching
- `consult-line` - Search current buffer
- `consult-ripgrep` - Project-wide search

#### Corfu (In-buffer Completion)
- Popup completion at point
- Integration with language servers (future)
- Company-mode alternative

### Success Criteria

- [ ] Vertico provides modern completion UI
- [ ] Orderless enables flexible matching
- [ ] Consult commands work as enhancements
- [ ] Corfu shows completions in buffer
- [ ] Packages can be loaded independently

---

## Phase 9: Syntax Highlighting & Tree-sitter

**Status:** 🔲 Planned
**Goal:** Modern syntax highlighting
**Timeline:** 3 weeks (May 20 - Jun 10, 2025)
**Prerequisites:** Phase 8 complete

### Features

#### Tree-sitter Integration
- Web Worker for parsing (keep UI responsive)
- Incremental parsing on edits
- Parse tree caching
- Query-based highlighting

#### Language Support
- JavaScript/TypeScript
- ClojureScript
- Python
- Rust
- Markdown

#### Highlighting
- Semantic token types
- Face (style) system
- Theme support (modus-operandi, modus-vivendi)

### Success Criteria

- [ ] Tree-sitter parses in background
- [ ] Syntax highlighting updates on edit
- [ ] No UI lag during typing
- [ ] At least 3 languages supported
- [ ] Themes can be switched

---

## Phase 10: LSP Integration

**Status:** 🔲 Planned
**Goal:** Language server protocol support
**Timeline:** 3 weeks (Jun 10 - Jul 1, 2025)
**Prerequisites:** Phase 9 complete, Bridge server working

### Features

#### LSP Client
- Initialize language server via bridge
- Document synchronization
- Diagnostics display (inline, gutter)
- Hover information
- Go-to-definition
- Find references
- Code actions

#### Supported Languages
- TypeScript (typescript-language-server)
- Rust (rust-analyzer)
- Clojure (clojure-lsp)
- Python (pylsp)

### Success Criteria

- [ ] LSP servers start via bridge
- [ ] Diagnostics show in editor
- [ ] Go-to-definition works (C-c g d)
- [ ] Hover shows type info
- [ ] Code actions available
- [ ] At least 2 languages work end-to-end

---

## Long-term Vision (Beyond Phase 10)

### Org-mode
- Outline editing
- TODO items
- Agenda
- Export to HTML/Markdown

### Magit (Git Interface)
- Status buffer
- Staging/unstaging
- Commits
- Branch management

### Collaboration
- Real-time collaborative editing
- Conflict resolution
- User presence indicators

### Cloud Integration
- Remote workspaces
- Devcontainer support
- Multi-user environments

---

## Metrics & Success Indicators

### Phase Completion Criteria
- All success criteria checkboxes ticked
- Demo video recorded showing features working
- Manual test pass (no automated tests yet)
- Documentation updated
- No known critical bugs

### Overall Project Health
- Can use Lexicon for editing its own source code
- Community interest (GitHub stars, issues, discussions)
- Package ecosystem growth
- Performance acceptable (< 100ms input latency)

---

## Current Focus

**Active Phase:** Phase 1 ✅ COMPLETE!
**Achievements:**
- All navigation commands working (line, buffer, word boundaries)
- Kill ring system with cut/copy/paste (C-k, M-w, C-y, M-y)
- Mark and region selection (C-SPC)
- Atomic :replace operation for gap buffer (eliminates race conditions)
- Full undo system with inverse operation recording (C-/, C-_)
- Fixed C-k end-of-buffer bug
- Browser shortcut prevention for Emacs keybindings

**Next Phase:** Phase 2 - Core Emacs Buffers & Files
**Next Milestone:** Multi-buffer support and file I/O
**Blocking Issues:** None

**Last Updated:** 2025-12-21

---

*This roadmap is a living document. Update it as we progress, learn, and adapt.*
