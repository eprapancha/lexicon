# Lexicon Project Memory

**Last Updated:** 2026-02-06
**Status:** Phase 6.6 - Epic #145 Skipped E2E Tests Implementation

---

## Project Mission

**Lexicon is a faithful recreation of GNU Emacs for the modern web.** We implement Emacs's actual architecture (gap buffers, hierarchical keymaps, mode system, command model) using modern technologies (Rust/WASM, ClojureScript, re-frame).

**Core Philosophy:** "When in doubt, do what Emacs does" - We study Emacs source code before implementing features. This is not Emacs-inspired, this IS Emacs for the browser.

---

## Debugging E2E Tests

**SEE:** `.claude/DEBUGGING_E2E_TESTS.md` for comprehensive debugging guide.

**Quick Reference:**
- Use `lexicon.log/debug` for E2E test debugging
- Messages buffer is automatically captured on test failure
- Log at decision points, not just entry/exit
- Remove debug logs after fixing issues

---

## GitHub First - Source of Truth

**GitHub issues are the primary source of truth.**

- **Current Work:** [Open Issues](https://github.com/eprapancha/lexicon/issues)
- **Completed:** [Closed Issues](https://github.com/eprapancha/lexicon/issues?q=is%3Aissue+is%3Aclosed)
- **Phase Tracking:** Labels `phase-6.5`, `phase-6.6`, `phase:7`, etc.

**This File Purpose:**
- Engineering standards & critical rules
- Recent technical learnings (WASM quirks, architecture decisions)
- Context that doesn't belong in issues

---

## CRITICAL: Test-Driven Development Philosophy

### The Tests Are The Specification

**ABSOLUTE RULE: Never weaken a test to make it pass.**

- **CORRECT:** Test fails -> Implement missing feature -> Test passes
- **WRONG:** Test fails -> Simplify test -> Test passes (defeats the purpose)

**What NOT To Do:**
- Removing assertions because the feature doesn't exist
- Changing a test to use simpler helpers
- Making a test "pending" because implementation is hard
- Modifying test expectations to match current (wrong) behavior

**What TO Do:**
- Implement the missing helper function properly
- Add the missing feature the test requires
- Fix the semantic bug the test exposes

### Test Helper Encapsulation Rules

**FORBIDDEN in `test/lexicon/semantic/helpers.cljs`:**
- Direct state access: `@rfdb/app-db`, `(get-in @rfdb/app-db ...)`
- Direct state mutation: `(swap! rfdb/app-db ...)`
- Implementing application logic (kill ring, undo, keymaps, modes)
- Direct WASM manipulation (except in test setup/fixtures)

**ALLOWED:**
- Test setup: `reset-editor-db!`, `with-wasm` fixture
- Calling `lexicon.api.test/*` functions (ONLY interface to application)
- Read-only queries for test assertions
- Helper wrappers that delegate to `lexicon.api.test`

**All test operations MUST go through `src/lexicon/api/test.cljs`.**

---

## E2E Test Guidelines

### Test Location and Organization

**Directory Structure:**
```
e2e_tests/lexicon/
├── ui/                    # User interaction tests (keyboard-only)
│   ├── editing/           # Text editing (undo, kill-ring, point-mark)
│   ├── buffers/           # Buffer operations (identity, switching)
│   ├── windows/           # Window management (split, delete)
│   ├── minibuffer/        # Minibuffer and completion
│   ├── modes/             # Major/minor mode tests
│   ├── files/             # File operations (dired, find-file)
│   ├── search/            # Search (isearch, occur, grep)
│   └── ...                # Other UI features
├── lisp/                  # Lisp API tests (for package developers)
│   ├── primitives_test.clj
│   ├── security_test.clj
│   └── ...
└── test_helpers.clj       # Shared test utilities
```

**Namespace Convention:**
- File: `e2e_tests/lexicon/ui/editing/undo_test.clj`
- Namespace: `lexicon.ui.editing.undo-test`
- Use hyphens in namespaces, underscores in filenames

### UI Tests vs Lisp Tests

**UI Tests (`ui/` folder):**
- Test user-visible behavior through keyboard simulation
- NEVER use `eval-lisp` or `evalLisp` - this is a strict rule enforced by lint
- Use helper functions: `h/type-text`, `h/press-ctrl`, `h/press-ctrl-x`, etc.
- Verify state via read-only queries: `h/get-buffer-text*`, `h/get-echo-area-text`

**Lisp Tests (`lisp/` folder):**
- Test Emacs Lisp API for package developers
- MAY use `eval-lisp` to test Lisp functions directly
- Examples: security sandboxing, Lisp primitive correctness

### Keyboard-Only Testing Philosophy

**CORRECT - Keyboard simulation:**
```clojure
(deftest test-undo-restores-text
  (testing "C-/ undoes the last change"
    (h/setup-test*)
    (h/type-text "hello")           ; User types
    (h/press-ctrl "/")              ; User presses C-/
    (is (= "" (h/get-buffer-text*)) ; Verify result
        "Undo should remove typed text")))
```

**WRONG - Using eval-lisp in UI tests:**
```clojure
;; DON'T DO THIS IN ui/ TESTS
(deftest test-undo-restores-text
  (h/setup-test*)
  (h/eval-lisp "(insert \"hello\")")  ; WRONG: bypasses user input
  (h/eval-lisp "(undo)")              ; WRONG: bypasses keyboard
  ...)
```

### Test Helper Functions

**Setup:**
- `(h/setup-test*)` - Initialize test state
- `(h/clear-buffer)` - Clear buffer content
- `(use-fixtures :once h/with-driver)` - Required fixture

**Keyboard Input:**
- `(h/type-text "string")` - Type text characters
- `(h/press-ctrl "x")` - Press C-x
- `(h/press-ctrl-x "2")` - Press C-x 2 (two-key sequence)
- `(h/press-meta "x")` - Press M-x
- `(h/press-key "Enter")` - Press special keys

**Minibuffer:**
- `(h/type-in-minibuffer "text")` - Type in minibuffer
- `(h/press-minibuffer-enter)` - Confirm minibuffer input
- `(h/run-mx-command "command-name")` - Full M-x workflow

**State Verification:**
- `(h/get-buffer-text*)` - Get current buffer text
- `(h/get-echo-area-text)` - Get echo area message
- `(h/get-mode-line-text)` - Get mode line content
- `(e/query-all h/*driver* {:css ".selector"})` - Query DOM elements

### How to Add New E2E Tests

1. **Determine test category:**
   - User feature? → `ui/` folder
   - Lisp API? → `lisp/` folder

2. **Find or create appropriate file:**
   - Group related tests together
   - One file per feature area (e.g., `ui/editing/undo_test.clj`)

3. **Write the test:**
```clojure
(ns lexicon.ui.feature.my-test
  "Description of what this tests"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-feature-behavior
  (testing "User does X and sees Y"
    (h/setup-test*)
    ;; Simulate user actions
    (h/type-text "input")
    (h/press-ctrl "c")
    ;; Verify results
    (is (= "expected" (h/get-buffer-text*))
        "Descriptive failure message")))
```

4. **Run the test:**
```bash
bb test:e2e ui.feature.my-test
```

### Skipped Tests

**When to use `^:skip`:**
- Features planned but not yet implemented
- Tests for future phases (LSP, shell, VC, etc.)

**Format:**
```clojure
(deftest ^:skip test-future-feature
  (testing "Feature description"
    (is true "PENDING: feature-name - needs implementation")))
```

**When NOT to use `^:skip`:**
- Never skip a test just because implementation is hard
- Never skip to hide a bug
- If a test fails, fix the implementation or the test (not skip it)

### Test Quality Checklist

Before committing tests:
- [ ] Tests are in correct folder (`ui/` vs `lisp/`)
- [ ] No `eval-lisp` in `ui/` tests (run `bb lint`)
- [ ] Uses proper test helpers (not direct state access)
- [ ] Has descriptive test names and failure messages
- [ ] Tests actual behavior, not just command existence
- [ ] Includes appropriate `Thread/sleep` for async operations

---

## Core Architecture Principles

### 1. State Ownership (CRITICAL)

**Rule:** Each state variable OWNED by exactly ONE module. Others MUST dispatch events.

**Ownership Map:**
- `:minibuffer` -> `ui.cljs` events: `:minibuffer/activate`, `:minibuffer/deactivate`
- `:echo-area` -> `ui.cljs` events: `:echo/message`, `:echo/clear`
- `:mark-position` -> `edit.cljs` events: `:set-mark`, `:deactivate-mark`
- `:window-tree` -> `ui.cljs` events: `:window/set-buffer`, `:window/set-mark`
- `:buffers` -> `buffer.cljs` events: `:buffer/set-mode`, `:buffer/update-version`
- `:kill-ring` -> `edit.cljs` (kill/yank commands only)

**Enforcement:**
```clojure
;; WRONG: Direct state manipulation
(assoc db :minibuffer {...})

;; CORRECT: Dispatch to owner
[:dispatch [:minibuffer/activate {...}]]
```

### 2. Minibuffer Architecture

**IMPORTANT:** The minibuffer uses `:minibuffer` map directly (NOT a stack).

All handlers must read/write consistently:
```clojure
;; Reading state:
(get-in db [:minibuffer :input])
(get-in db [:minibuffer :on-confirm])

;; Writing state:
(assoc-in db [:minibuffer :input] new-input)
```

Do NOT mix stack-based and map-based access patterns.

### 3. Userspace/Kernel Split

**Rust/WASM "Kernel":** Gap buffer, UTF-8, performance-critical primitives
**ClojureScript "Userspace":** All commands (even `self-insert-command`), UI, extensibility

### 4. Core vs Package

**Core** (emacs -Q): Buffer/window/frame, keymaps, modes, minibuffer, editing, file I/O
**Packages** (loadable): Evil, Vertico, LSP, syntax highlighting, themes

### 5. Emacs Fidelity

**When in doubt, study Emacs source code:**
- Emacs source: `~/projects/emacs-source/`
- Key files: `lisp/simple.el`, `src/callint.c`, `lisp/minibuffer.el`, `src/buffer.c`

---

## Recent Technical Learnings

### WASM API
- **WasmGapBuffer** only - simple gap buffer interface
- Methods: `.insert(pos, text)`, `.delete(pos, length)`, `.getText()`
- Property: `.-length` (NOT a method)

### Cursor Position (3 Locations)
- `[:ui :cursor-position]` - linear integer (for WASM)
- `[:buffers id :cursor-position]` - line/column per-buffer
- Window `:cursor-position` - line/column per-window

### Initialization Order
- Event handlers must be registered before dispatch-sync calls
- Wrap top-level dispatch-sync in init functions
- Call init functions after `:initialize-commands` in main.cljs

### Browser Caching
- After code changes, browser may cache stale JS
- Hard refresh (Ctrl+Shift+R) if changes don't appear

---

## Engineering Standards

### Critical Rules

**NEVER:**
- Run compilation commands - User has shadow-cljs watch running
  - DO NOT run: `npm run build`, `shadow-cljs compile`, etc.
  - Make code changes, user's watch will recompile
- Commit code that doesn't compile cleanly
- Ignore compilation warnings (zero tolerance)
- Violate state ownership (check ownership map first)
- Weaken or "relax" tests to hide bugs

**ALWAYS:**
- Check GitHub issues FIRST before starting work
- Study Emacs source before implementing features
- Dispatch events to state owners (never mutate directly)
- Commit after each green test run (atomic commits)
- Reference issue numbers in commits

### Testing

**Running Tests:**
```bash
cargo test              # Rust unit tests
bb test:e2e             # All E2E tests
bb test:e2e <pattern>   # Specific E2E tests (e.g., bb test:e2e ui.isearch-test tests lexicon.ui.isearch-test)
bb test                 # All tests
```

### Commit Format

```
type(scope): concise description

- Detailed bullet points
- Explain what and why

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Types:** `feat`, `fix`, `refactor`, `test`, `docs`, `deps`

**Before Commit:**
1. Run `bb lint` - ALWAYS
2. Ensure code compiles (user's watch shows this)
3. Run tests if applicable
4. Fix ALL warnings

---

## GitHub Labels Schema

**Format:** `category:value` (colon separator)

**Required Labels (Every Issue):**
- `type:bug`, `type:enhancement`, `type:refactor`, `type:docs`, `type:epic`
- `priority:critical`, `priority:high`, `priority:medium`, `priority:low`

**Optional Labels:**
- `phase:6.5`, `phase:6.6`, `phase:7`, etc.
- `component:buffer`, `component:command`, `component:edit`, `component:minibuffer`, etc.
- `status:blocked`, `status:in-review`
- `marker:e2e-failure`, `marker:regression`

**Before creating labels:** Check existing with `gh label list`

---

## Quick Reference

### Key Commands

```bash
bb dev                   # Dev server
bb lint                  # Run all linters (ALWAYS before commit)
bb test                  # All tests
bb test:e2e              # E2E tests
bb test:e2e <pattern>    # Specific E2E tests
cargo test               # Rust tests
bb clean                 # Clean artifacts
```

### Important Paths

```
packages/editor-cljs/src/lexicon/     # ClojureScript
packages/lexicon-engine/wasm/         # Rust WASM
e2e_tests/                            # E2E tests
```

### Emacs Reference

**Emacs 29.4 Source Code:** `~/projects/emacs-source/`

```
~/projects/emacs-source/lisp/simple.el      # universal-argument
~/projects/emacs-source/src/callint.c       # Interactive specs
~/projects/emacs-source/lisp/minibuffer.el  # Completion
~/projects/emacs-source/src/buffer.c        # Buffer-local vars
```

---

## TDD Deep Dive Workflow

**When doing TDD deep dives for Emacs parity:**

1. **NO new markdown files** - Findings go into GitHub issue comments
2. **Use Emacs source** at `~/projects/emacs-source/`
3. **For each issue:**
   - Read Emacs C source (src/*.c) for DEFUNs
   - Read Emacs Lisp (lisp/*.el) for higher-level functions
   - Document findings in GitHub issue comment
   - Create test stubs in `test/lexicon/semantic/`
4. **Ensure both `app` and `test` builds compile with 0 warnings**

---

## Remember

1. **GitHub first** - Check issues before starting
2. **Emacs fidelity** - Study Emacs source code
3. **State ownership** - Dispatch to owners, never mutate directly
4. **Test first** - Write tests before implementing
5. **Commit frequently** - After each green test
6. **Zero warnings** - Fix all warnings before commit
7. **Zero regressions** - All tests must pass

---

## Issue Sweep Workflow

**Name: `issue-sweep`** - Use this name to invoke this workflow in new sessions.

**Purpose:** Systematically close all open issues by implementing missing features. This is TDD - tests expose gaps, we fill them.

### Core Rules

1. **Pick an issue, fix it completely** - Enhance core if required. No shortcuts.

2. **Test preference:** UI tests > Lisp tests. Only use Lisp tests for package-developer APIs.

3. **Never run full test suite** - Takes 30+ mins. Run specific test files only.

4. **Always tee test output:** `bb test:e2e <pattern> 2>&1 | tee /tmp/e2e-<feature>.log`
   - User runs `tail -f /tmp/e2e*.log` to monitor progress

5. **New tests must pass 100%** - Skip only if truly blocked (backend dependencies like LSP).

6. **Don't commit during sweep** - User will batch commits later. Avoids approval interruptions.

7. **DO close issues immediately** when fully implemented (with comment citing what was done).

8. **For large issues with 1-2 blockers:** Spawn new issue for blockers, close the main issue.

9. **Keep README.md updated** - Check commits since last README update, document new features.

10. **Respect linting rules** - Run `bb lint`, never modify lint rules.

11. **Compilation is user's watch process** - Don't run build commands. If changes seem stale, check watch output but don't kill the process. 99% of the time it's logic issues, not compilation.

12. **Avoid commands needing user input** - No commits (need approval), no interactive prompts.

### Workflow Steps

```
1. gh issue list --state open          # See open issues
2. Pick issue (priority: high > medium > low)
3. Read issue, understand requirements
4. Study Emacs source if needed (~/projects/emacs-source/)
5. Implement feature in core
6. Enable/write tests
7. Run: bb test:e2e <pattern> 2>&1 | tee /tmp/e2e-<feature>.log
8. Fix until tests pass
9. Close issue: gh issue close <N> --comment "Implemented: ..."
10. Move to next issue
```

### What to Skip (Defer to New Issues)

- Features requiring backend not available in browser (LSP servers, shell, VC backends)
- Features with deep infrastructure dependencies that would take days
- When complexity is genuinely blocking other progress

### What NOT to Skip

- "Hard" implementations - that's the point of TDD
- Tests that seem complicated - implement the feature
- Anything that just needs code written

---

**This file survives conversation compactions. Updated 2026-02-06.**
