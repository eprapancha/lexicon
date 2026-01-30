# Lexicon Project Memory

**Last Updated:** 2026-01-18
**Status:** Phase 6.6 - Epic #86 Emacs Semantic Compatibility - Undo System Fixed! 🎉

---

## 🔧 CRITICAL: Debugging E2E Tests

**SEE:** `.claude/DEBUGGING_E2E_TESTS.md` for comprehensive debugging guide.

**Quick Reference:**
- Use `lexicon.log/debug` for E2E test debugging
- Messages buffer is automatically captured on test failure
- Log at decision points, not just entry/exit
- Include relevant state in log messages
- Remove debug logs after fixing issues

---

## Project Mission

**Lexicon is a faithful recreation of GNU Emacs for the modern web.** We implement Emacs's actual architecture (gap buffers, hierarchical keymaps, mode system, command model) using modern technologies (Rust/WASM, ClojureScript, re-frame).

**Core Philosophy:** "When in doubt, do what Emacs does" - We study Emacs source code before implementing features. This is not Emacs-inspired, this IS Emacs for the browser.

---

## 📋 Current Status (2026-01-18)

### Epic #86: Emacs Semantic Compatibility - Core Invariant Tests

**Epic:** [#86](https://github.com/eprapancha/lexicon/issues/86) - Emacs Semantic Compatibility Test Suite (34 tests)

**Status:** 🎯 UNDO SYSTEM WORKING - Debugging Methodology Established ✅

**Major Milestone Achieved (2026-01-18):**
- ✅ **Undo system fixed** - C-/ now works correctly in scratch and new buffers
- ✅ **Debugging methodology established** - Messages buffer logging approach documented
- ✅ **Advanced undo tests** - 3/4 tests passing (boundary issue remains)
- ✅ **Bug fixes:** Missing `:text` in undo entries, double inversion bug, view update after undo

**Bugs Fixed:**
1. Undo entries missing `:text` field (wasm.cljs:56)
2. Double inversion of undo operations (advanced_undo.cljs:228)
3. View not updating after WASM buffer modification (edit.cljs:652-676)

**Test Progress by Category:**
- [#87](https://github.com/eprapancha/lexicon/issues/87) - Core Invariants: 4/6 tests (67%)
- [#88](https://github.com/eprapancha/lexicon/issues/88) - Filesystem/Buffers: 3/6 tests (50%)
- [#89](https://github.com/eprapancha/lexicon/issues/89) - Lisp/SCI: 0/7 tests (needs SCI integration)
- [#90](https://github.com/eprapancha/lexicon/issues/90) - Interaction: 5/7 tests (71%)
- [#91](https://github.com/eprapancha/lexicon/issues/91) - History: 1/6 tests (17% - undo working, needs kill-ring)
- [#92](https://github.com/eprapancha/lexicon/issues/92) - Minibuffer: 1/1 tests (100%) ✅ **GATE PASSED**

**Remaining Work:**
- Kill ring/yank (3 tests) - Issue #97
- Undo boundaries (1 test) - Issue #98
- Query-replace-regexp (1 test) - Issue #99
- Keymap system (4 tests)
- Filesystem I/O (5 tests)
- Lisp/SCI integration (4 tests)

**Key Architectural Validations Completed:**
- ✅ Buffers are primary entities (files optional)
- ✅ Point is buffer-local (not window-local)
- ✅ Minibuffer IS a buffer (VERTICO GATE)
- ✅ Commands are inspectable entities
- ✅ Major modes mutually exclusive, minor modes composable
- ✅ Windows don't own buffers
- ✅ Read-only enforcement at correct level

### Phase 6.5: Lexicon Core Foundation (COMPLETED ✅)

**Epic:** [#75](https://github.com/eprapancha/lexicon/issues/75) - All 4 components implemented:
- ✅ [#76](https://github.com/eprapancha/lexicon/issues/76) - Prefix Arguments (C-u System)
- ✅ [#77](https://github.com/eprapancha/lexicon/issues/77) - Minibuffer State Machine
- ✅ [#78](https://github.com/eprapancha/lexicon/issues/78) - Buffer-Local Environments
- ✅ [#79](https://github.com/eprapancha/lexicon/issues/79) - Runtime Evaluation (SCI Integration)

### Test Analysis Resources (in `/tmp/`)

**Comprehensive documentation generated 2026-01-16:**
1. `/tmp/EXECUTIVE-SUMMARY.md` (16 KB) - Management overview, effort estimates
2. `/tmp/test-helper-analysis.md` (31 KB) - Complete helper function inventory
3. `/tmp/test-helper-quick-reference.md` (9.4 KB) - Quick lookup cheat sheet
4. `/tmp/test-implementation-roadmap.md` (21 KB) - Visual roadmap, dependencies
5. `/tmp/helpers-starter-implementation.clj` (12 KB) - Week 1 scaffolding code
6. `/tmp/IMPLEMENTATION-CHECKLIST.md` (14 KB) - Week-by-week task tracking

**Key Findings:**
- 62 unique helper functions needed
- 28 functions (45%) are simple wrappers around existing code
- Week 1 is critical bottleneck - all other work blocks on test harness
- Lexicon already has all required capabilities (~7,100 lines of working code)

---

## 📋 GitHub First - Source of Truth

**⚠️ IMPORTANT: GitHub issues are the primary source of truth.**

- **Current Work:** [Open Issues](https://github.com/eprapancha/lexicon/issues)
- **Completed:** [Closed Issues](https://github.com/eprapancha/lexicon/issues?q=is%3Aissue+is%3Aclosed)
- **Phase Tracking:** Labels `phase-6.5`, `phase-7`, etc.

**This File (.CLAUDE.md) Purpose:**
- Engineering standards & critical rules
- Recent technical learnings (WASM quirks, architecture decisions)
- Context that doesn't belong in issues

---

## 🔴 CRITICAL: Test-Driven Development Philosophy

### The Tests Are The Specification

**Epic #86 Semantic Compatibility Tests exist to expose gaps and brittleness in our code.**

**ABSOLUTE RULE: Never weaken a test to make it pass.**

- ✅ **CORRECT:** Test fails → Implement missing feature → Test passes
- ❌ **WRONG:** Test fails → Simplify test → Test passes (defeats the purpose)

**Examples of What NOT To Do:**
- Removing assertions from a test because the feature doesn't exist
- Changing a test to use simpler helpers instead of the required ones
- Making a test "pending" because implementation is hard
- Modifying test expectations to match current (wrong) behavior

**What TO Do Instead:**
- Implement the missing helper function properly
- Add the missing feature the test requires
- Fix the semantic bug the test exposes
- Ask "What is this test trying to validate?" and ensure we validate it

**The Test Is Right Until Proven Wrong:**
- Tests come from Emacs semantics research and expert review
- Tests validate architectural invariants, not features
- If a test seems wrong, verify against Emacs source code first
- Only change a test if it's fundamentally flawed (rare)

## 🔴 CRITICAL: Test Helper Encapsulation Rules

**AUDIT FINDING (2026-01-17):** Test helpers were implementing application logic, making tests pass without testing the actual application.

### ABSOLUTE RULES for `test/lexicon/semantic/helpers.cljs`

**❌ FORBIDDEN:**
- Direct state access: `@rfdb/app-db`, `(get-in @rfdb/app-db ...)` (except for test setup)
- Direct state mutation: `(swap! rfdb/app-db ...)`, `(assoc-in ... app-db)`
- Implementing application logic (kill ring, undo, keymaps, modes, window management)
- Direct WASM manipulation (except in test setup/fixtures)
- Creating data structures that should be in `src/`

**✅ ALLOWED:**
- Test setup: `reset-editor-db!`, `with-wasm` fixture
- Calling `lexicon.api.test/*` functions (ONLY interface to application)
- Read-only queries for test assertions (temporary, until subscriptions exist)
- Test instrumentation (recording dispatch calls, measuring execution)
- Helper wrappers that delegate to `lexicon.api.test`

### Enforcement

**All test operations MUST go through `src/lexicon/api/test.cljs`.**

This API boundary:
- Forces tests to use real re-frame events
- Prevents bypassing application logic
- Makes missing features obvious (events don't exist → test fails)
- Ensures we're testing the application, not the helpers

**If a helper needs to do something the API doesn't support:**
1. ❌ DO NOT implement it directly in the helper
2. ✅ Implement the event in `src/lexicon/events/`
3. ✅ Add to `src/lexicon/api/test.cljs`
4. ✅ Helper calls the API function

### Why This Matters:**
- These tests prevent architectural drift
- They expose semantic bugs we don't know we have
- They validate degrees of freedom, not feature parity
- Weakening tests = lying to ourselves about Emacs compatibility

**Session 2026-01-17 Example:**
- Test required `with-buffer` macro → Implemented it properly
- Test required read-only enforcement → Added it at correct level (insert-text)
- Test required proper command metadata → Used existing command registry
- Did NOT simplify tests, DID implement proper infrastructure

This is documented in `.CLAUDE.md` and `SEMANTIC_TEST_PROGRESS.md` for continuity across sessions.

---

## Core Architecture Principles

### 1. State Ownership (CRITICAL)

**Problem:** Shared mutable state creates tight coupling, debugging nightmares, and regressions.

**Rule:** Each state variable OWNED by exactly ONE module. Others MUST dispatch events.

**Ownership Map:**
- `:minibuffer` → `ui.cljs` events: `:minibuffer/activate`, `:minibuffer/deactivate`
- `:echo-area` → `ui.cljs` events: `:echo/message`, `:echo/clear`
- `:mark-position` → `edit.cljs` events: `:set-mark`, `:deactivate-mark`
- `:window-tree` → `ui.cljs` events: `:window/set-buffer`, `:window/set-mark`, `:window/set-cursor`
- `:ui :cursor-position` → `edit.cljs` events: `:cursor/set-position`
- `:buffers` → `buffer.cljs` events: `:buffer/set-mode`, `:buffer/update-version`
- `:kill-ring` → `edit.cljs` (kill/yank commands only)

**Enforcement:**
```clojure
;; ❌ WRONG: Direct state manipulation
(assoc db :minibuffer {...})

;; ✅ CORRECT: Dispatch to owner
[:dispatch [:minibuffer/activate {...}]]
```

See `db.cljs` for full ownership documentation.

### 2. Userspace/Kernel Split

**Rust/WASM "Kernel":** Gap buffer, UTF-8, performance-critical primitives
**ClojureScript "Userspace":** All commands (even `self-insert-command`), UI, extensibility

**Rule:** Only speed-critical functions in Rust. Everything else in ClojureScript.

### 3. Core vs Package

**Core** (emacs -Q): Buffer/window/frame, keymaps, modes, minibuffer, editing, file I/O
**Packages** (loadable): Evil, Vertico, LSP, syntax highlighting, themes

**Rule:** If Emacs doesn't have it by default, it's a package.

### 4. Emacs Fidelity

**When in doubt, study Emacs source code:**
- Emacs source: `/tmp/emacs-source` (available for reference)
- Key files: `lisp/simple.el`, `src/callint.c`, `lisp/minibuffer.el`, `src/buffer.c`
- Read Emacs implementation BEFORE writing features
- Replicate behavior, not just approximate

---

## Phase 6.5 Core Goals (NEW)

### 1. Prefix Arguments (Week 1-2)

**What:** C-u system for modifying command behavior

**Implementation:**
- Add `:prefix-arg` and `:current-prefix-arg` to db
- Implement `universal-argument`, `digit-argument`, `negative-argument`
- Add transient keymap support
- Update interactive spec parser for "P" and "p" codes
- Clear prefix-arg after command execution

**Acceptance:**
- C-u → (4), C-u C-u → (16), C-u 5 → 5, C-u - 3 → -3
- Commands receive via `(interactive "P")` or `(interactive "p")`

### 2. Minibuffer State Machine (Week 3-4)

**What:** Stack-based minibuffer with recursive support

**Implementation:**
- Change `:minibuffer` from map to stack of frames
- Each frame: `{:prompt :input :completion-table :on-confirm :on-cancel}`
- `enable-recursive-minibuffers` config variable
- Fix command-to-command minibuffer handoff

**Fixes:** Issue #72 (query-replace-regexp timeout)

**Acceptance:**
- Minibuffer depth tracking works
- Recursive minibuffer when enabled
- M-x → command transition works without race condition

### 3. Buffer-Local Environments (Week 5-6)

**What:** Per-buffer variable bindings and mode infrastructure

**Implementation:**
- Add `:local-vars` map to each buffer
- Variable lookup: buffer-local → global fallback
- `define-minor-mode` infrastructure
- Example: `line-number-mode`

**Acceptance:**
- Different buffers have independent local values
- Minor modes toggle per-buffer
- Mode keymaps activate when enabled

### 4. Runtime Evaluation (Week 7-8)

**What:** SCI-based eval for user code and init files

**Implementation:**
- Integrate `@borkdude/sci` npm package
- Create `lexicon.eval` with safe sandboxing
- Implement eval functions: `eval-string`, `eval-region`, `eval-buffer`, `eval-last-sexp`
- Load `~/.lexicon/init.cljs` on startup

**Acceptance:**
- C-x C-e evaluates code in *scratch*
- M-: evaluates from minibuffer
- Init file can define custom commands and keybindings
- Security: Only approved namespaces accessible

---

## Recent Technical Learnings

### WASM API (Simplified)
- **WasmGapBuffer** only - simple gap buffer interface
- Methods: `.insert(pos, text)`, `.delete(pos, length)`, `.getText()`
- Property: `.-length` (NOT a method)
- `WasmEditorCore` was deleted (unused complexity)

### Cursor Position (3 Locations)
- `[:ui :cursor-position]` - linear integer (for WASM)
- `[:buffers id :cursor-position]` - line/column per-buffer
- Window `:cursor-position` - line/column per-window

### Log Bus (Issue #73)
- Lifecycle-independent logging system
- `log/info`, `log/warn`, `log/error` - always work
- Messages replay to *Messages* buffer when ready
- No dependencies on buffer/minibuffer state

### Prefix Arguments Research (Phase 6.5)
- Emacs uses transient keymap for accumulation
- `prefix-arg` (raw) vs `current-prefix-arg` (during execution)
- Interactive spec: "P" (raw), "p" (numeric via `prefix-numeric-value`)
- Value types: nil, (4), (16), numbers, negative

### Minibuffer Stack Research (Phase 6.5)
- Emacs tracks depth, each level has independent state
- `enable-recursive-minibuffers` controls nesting
- Issue #72 root cause: Single-state minibuffer race condition
- Solution: Stack allows command to replace/push frame

---

## Engineering Standards

### Critical Rules

**⚠️ NEVER:**
- ❌ **Run compilation commands** - User has shadow-cljs watch running for both app and tests
  - ❌ DO NOT run: `npm run build`, `npm run compile`, `shadow-cljs compile`, `shadow-cljs release`, etc.
  - ❌ Compilation output is hidden from user - you trivialize warnings
  - ✅ Make code changes, tell user, wait for watch to recompile, user reports failures
  - **User is "fed up of repeating this" - REMEMBER THIS**
- ❌ Commit code that doesn't compile cleanly
- ❌ Ignore compilation warnings (zero tolerance)
- ❌ Violate state ownership (check ownership map first)
- ❌ Create GitHub labels without documenting in this file first
- ❌ **Weaken or "relax" tests to hide bugs** - Fix the CODE, not the TEST
  - Tests failing for weeks is acceptable
  - Hiding real problems is NOT acceptable

**✅ ALWAYS:**
- ✅ Check GitHub issues FIRST before starting work
- ✅ Study Emacs source before implementing features
- ✅ Dispatch events to state owners (never mutate directly)
- ✅ Commit after each green test run (atomic commits)
- ✅ Reference issue numbers in commits

### Testing Requirements

**Coverage:**
- ✅ Rust: Unit tests for all public functions
- ✅ E2E: All user-facing features must have E2E tests
- ✅ Test-first when possible
- ✅ No regressions: All tests must pass before commit

**Running Tests:**
```bash
cargo test              # Rust unit tests
bb test:e2e             # E2E tests (app at :8080)
bb test                 # All tests
```

### Commit Hygiene

**Format:**
```
type(scope): concise description

- Detailed bullet points
- Explain what and why, not how
- Reference issue if applicable

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Types:** `feat`, `fix`, `refactor`, `test`, `docs`, `deps`

**Before Commit:**
1. Run `bb lint` - ALWAYS (catches architecture violations, code issues)
2. Ensure code compiles (user's watch will show this)
3. Run tests if applicable
4. Fix ALL warnings

### File Organization

**Size Limit:** ~500 lines per namespace - split when exceeded

**Namespace Structure:**
```
packages/editor-cljs/src/lexicon/
├── core.cljs              # Main entry, API re-exports
├── db.cljs                # Schema, ownership docs
├── events/                # Domain-specific handlers
│   ├── buffer.cljs
│   ├── command.cljs
│   ├── edit.cljs
│   ├── minibuffer.cljs
│   └── ...
├── eval.cljs              # NEW: SCI wrapper (Phase 6.5)
├── modes/                 # Mode definitions
└── api/                   # Public API modules
    ├── buffer.cljs
    └── message.cljs
```

---

## GitHub Labels - Standardized Schema

**⚠️ CRITICAL:** This is the ONLY source of truth for labels. ALL labels use `category:value` format with `:` separator.

### Label Format Standard

**Format:** `category:value`
- Category and value separated by `:` (NOT `-`)
- Multiple words in value use `-` (e.g., `component:gap-buffer`)
- Always singular, not plural (e.g., `component:buffer`, NOT `component:buffers`)
- Lowercase only

**Examples:**
- ✅ CORRECT: `priority:high`, `component:gap-buffer`, `type:enhancement`
- ❌ WRONG: `priority-high`, `component-gap_buffer`, `component:buffers`

### Required Labels (Every Issue Must Have)

**1. type:** (Pick ONE - what kind of issue)
- `type:bug` - Something broken
- `type:enhancement` - New functionality
- `type:refactor` - Code improvement (no behavior change)
- `type:docs` - Documentation only
- `type:epic` - Large multi-issue feature

**2. priority:** (Pick ONE - how urgent)
- `priority:critical` 🔴 - Blocking (build broken, data loss, total breakage)
- `priority:high` 🟠 - Important (regressions, user-facing bugs, architectural)
- `priority:medium` 🟡 - Normal (features, enhancements)
- `priority:low` 🟢 - Nice to have (polish, optimizations)

### Optional Labels

**phase:** (Which development phase, when work is planned)
- `phase:6.5`, `phase:6.6`, `phase:7`, `phase:8`, etc.

**component:** (Which subsystem - can have MULTIPLE)
- `component:buffer` - Buffer management
- `component:command` - Command system
- `component:edit` - Editing operations
- `component:gap-buffer` - WASM gap buffer engine
- `component:hook` - Hook system
- `component:keymap` - Keymap system
- `component:message` - Message/logging system
- `component:minibuffer` - Minibuffer system
- `component:mode` - Major/minor mode system
- `component:state` - State management
- `component:test` - Test infrastructure
- `component:ui` - UI rendering
- `component:undo` - Undo system
- `component:window` - Window management

**status:** (Current state - optional)
- `status:blocked` - Blocked by dependencies
- `status:in-review` - In code review
- (No status = not started or in progress)

**marker:** (Special flags - optional)
- `marker:e2e-failure` - E2E test failure
- `marker:regression` - Previously working, now broken

### Label Creation Protocol

**⚠️ BEFORE creating ANY label:**

1. **STOP and check existing:** `gh label list`
2. **Check if category exists** (priority, type, component, status, phase, marker)
3. **If category exists, use it** - Do NOT create variations
4. **If new value needed in existing category:**
   - Follow naming rules (`category:value`)
   - Add to this file FIRST
   - Then create in GitHub
5. **If entirely new category needed:**
   - Update .CLAUDE.md FIRST with rationale
   - Get user approval
   - Document in this section

**Naming Rules (CRITICAL):**
- Format: `category:value` (colon separator)
- Use `-` for multi-word values: `component:gap-buffer`
- Always singular: `buffer` not `buffers`
- Lowercase only
- No special characters except `:` and `-`

**Enforcement:**
- Claude MUST check `gh label list` before suggesting new labels
- Claude MUST use existing categories when possible
- Claude MUST NOT create variations (e.g., `priority-high` when `priority:high` exists)
- User will reject PRs/commits that violate label standards

### Common Mistakes to Avoid

❌ **WRONG:**
- Creating `priority-high` when `priority:high` exists
- Creating `component:buffers` (plural) when `component:buffer` (singular) exists
- Creating `enhancement` when `type:enhancement` exists
- Creating `bug` when `type:bug` exists
- Creating `minibuffer` when `component:minibuffer` exists
- Creating `e2e-failure` when `marker:e2e-failure` exists
- Creating `regression` when `marker:regression` exists

✅ **CORRECT:**
- Check existing labels first: `gh label list`
- Use existing category: `type:bug`, `priority:high`, `marker:regression`
- Follow format: `category:value` with colon
- Use singular: `component:buffer`
- ALL labels must have category prefix (no exceptions)

---

## Security Model

### Package Trust (Phase 7.7+)

- **:core** - Built-in, full access
- **:local** - User filesystem, full access
- **:external** - Internet, **SCI sandbox only**, Core API only

### Runtime Eval Security (Phase 6.5 Week 7-8)

- SCI sandboxed evaluation
- Allow-list: `lexicon.core`, `lexicon.api.*`, safe `clojure.core` subset
- Block: Filesystem (except approved functions), network, dangerous JS interop
- User init file: `~/.lexicon/init.cljs` runs in sandbox

---

## Development Workflow

### Daily Development

```bash
bb dev              # Start dev server (user does this)
# Make changes, verify live reload

# Before commit:
cargo test          # If Rust changed
bb test:e2e         # If features changed
```

### Debugging

**Git for Regressions:**
```bash
git diff HEAD~5              # Recent changes
git bisect start             # Find breaking commit
git log -p -- file.cljs      # File history
```

**Browser DevTools:** Console errors, re-frame-10x, network tab

**Rust:** `cargo test -- --nocapture`

---

## Quick Reference

### Key Commands

```bash
bb dev                   # Dev server
bb lint                  # Run all linters (ALWAYS before commit)
bb test                  # All tests
bb clean                 # Clean artifacts
cargo test               # Rust tests
bb test:e2e              # E2E tests
```

### Important Paths

```
packages/editor-cljs/src/lexicon/     # ClojureScript
packages/lexicon-engine/wasm/         # Rust WASM
e2e_tests/                            # E2E tests
docs/                                 # Documentation
```

### Emacs Reference

**Emacs 29.4 Source Code:** `~/projects/emacs-source/`

```
~/projects/emacs-source/lisp/simple.el      # universal-argument
~/projects/emacs-source/src/callint.c       # Interactive specs
~/projects/emacs-source/lisp/minibuffer.el  # Completion
~/projects/emacs-source/src/buffer.c        # Buffer-local vars
~/projects/emacs-source/lisp/dired.el       # Dired implementation
~/projects/emacs-source/lisp/dired-aux.el   # Dired auxiliary functions
~/projects/emacs-source/lisp/dired-x.el     # Dired extensions
```

---

## Remember

1. ✅ **GitHub first** - Check issues before starting
2. ✅ **Emacs fidelity** - Study Emacs source code
3. ✅ **State ownership** - Dispatch to owners, never mutate directly
4. ✅ **Test first** - Write tests before implementing
5. ✅ **Commit frequently** - After each green test
6. ✅ **Zero warnings** - Fix all warnings before commit
7. ✅ **Zero regressions** - All tests must pass

---

## 📋 TDD Deep Dive Workflow (Issue #94 Roadmap)

**When doing TDD deep dives for Emacs parity (issues #100-#114):**

1. **NO new markdown files** - All findings go into the relevant GitHub issue as comments
2. **Use Emacs source** at `~/projects/emacs-source/` for authoritative reference
3. **For each issue:**
   - Read relevant Emacs C source (src/*.c) for DEFUNs
   - Read relevant Emacs Lisp (lisp/*.el) for higher-level functions
   - Document findings in GitHub issue comment
   - Create test stubs in `test/lexicon/semantic/`
   - Add helper stubs to prevent compilation errors
   - Ensure both `app` and `test` builds compile with 0 warnings
4. **Batch commits** - Do multiple deep dives, commit all at once
5. **Priority order:**
   - CRITICAL: #100 Buffer, #101 Markers, #103 Undo, #104 Kill Ring, #105 Editing, #106 SCI
   - HIGH: #102 Text Properties, #107 Search, #108 Minibuffer, #110 Windows, #111 Files
   - MEDIUM: #109 Help, #112 Shell, #113 VC
   - LOW: #114 Outline

**Key Emacs Source Files:**
```
src/buffer.c      - Buffer primitives (#100)
src/marker.c      - Markers (#101)
src/undo.c        - Undo system (#103)
src/editfns.c     - Editing/Kill ring (#104, #105)
src/eval.c        - Evaluation (#106)
src/textprop.c    - Text properties (#102)
src/search.c      - Search (#107)
src/minibuf.c     - Minibuffer (#108)
src/window.c      - Windows (#110)
src/fileio.c      - Files (#111)
```

---

**This file survives conversation compactions. Updated 2026-01-25 with TDD workflow.**
