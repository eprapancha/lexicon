# Lexicon Project Memory

**Last Updated:** 2026-01-30
**Status:** Phase 6.6 - Epic #86 Emacs Semantic Compatibility

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

## Current Work Session (2026-02-03)

**Active Issues (in priority order):**
1. **#130** - Code Intelligence (font-lock.el, which-func.el) - Syntax highlighting
2. **#139** - Interactive dired-mode (navigation, file operations)
3. **#115** - Buffer Menu (ibuffer, buff-menu, uniquify)
4. **#109** - Help System (help.el, info.el)
5. **#114** - Outline & Folding (outline.el, hideshow.el)

**Implementation Guidelines:**
- Full implementation without shortcuts or caveats
- No commits until ALL features are implemented and manually tested by user
- UI tests preferred; lisp tests only for non-user-testable/package-developer functionality
- If tests get complicated, skip temporarily - implementation is highest priority
- Add periodic progress updates to GitHub issues
- Study Emacs source at `~/projects/emacs-source/` for fidelity

**Workflow:**
1. Deep dive Emacs source for each feature
2. Implement the feature completely
3. Write UI tests for user-facing functionality
4. Update GitHub issue with progress
5. Move to next feature
6. When ALL done, user will manually test before committing

---

**This file survives conversation compactions. Updated 2026-02-03.**
