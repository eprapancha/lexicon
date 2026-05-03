# Lexicon Project Memory

**Last Updated:** 2026-05-03

---

## Project Mission

**Lexicon is a faithful recreation of GNU Emacs for the modern web.** We implement Emacs's actual architecture (gap buffers, hierarchical keymaps, mode system, command model) using modern technologies (Rust/WASM, ClojureScript, re-frame).

**Core Philosophy:** "When in doubt, do what Emacs does" - We study Emacs source code before implementing features. This is not Emacs-inspired, this IS Emacs for the browser.

**Full vision:** See `docs/VISION.org` for long-term direction (collaborative CMS on org-mode).
**Roadmap:** See `docs/ROADMAP.org` for development tracking.

---

## Documentation Map

| File | Purpose |
|------|---------|
| `docs/ARCHITECTURE.org` | Technical architecture, state schema, WASM API |
| `docs/ARCHITECTURE_BOUNDARY.org` | Core/package boundary rules, enforcement |
| `docs/EMACS_COMPATIBILITY_CONTRACT.org` | The project "constitution" - semantic guarantees |
| `docs/VISION.org` | Long-term vision and sequencing |
| `docs/ROADMAP.org` | Development tracking |
| `.claude/DEBUGGING_E2E_TESTS.md` | E2E test debugging guide |
| `.claude/skills/write-test/SKILL.md` | E2E test authoring templates |

### Skills (invoke with `/skill-name`)

| Skill | Purpose |
|-------|---------|
| `/chrome-test` | Generate browser test prompts for Chrome Extension |
| `/write-test` | Author E2E tests following project conventions |
| `/reconcile` | Check if docs are up-to-date with code changes |
| `/emacs-study` | Research Emacs source for a feature |
| `/gap-analysis` | Compare Emacs vs Lexicon for a feature |
| `/design-spec` | Design implementation spec for a feature |
| `/qa-check` | Run tests, lint, and validate quality |

### Research (survives compaction)

| File | Purpose |
|------|---------|
| `.claude/research/consult-study.md` | Exhaustive consult package analysis (commands, async pipeline, sources, preview) |
| `.claude/research/embark-study.md` | Exhaustive embark package analysis (targets, actions, integration) |
| `.claude/research/wasm-ecosystem.md` | WASM feasibility: ripgrep, VFS, process model, tree-sitter |
| `.claude/research/consult-implementation-strategy.md` | Five-layer implementation plan with subproject extraction |

---

## GitHub First - Source of Truth

**GitHub issues are the primary source of truth.**

- **Current Work:** [Open Issues](https://github.com/eprapancha/lexicon/issues)
- **Completed:** [Closed Issues](https://github.com/eprapancha/lexicon/issues?q=is%3Aissue+is%3Aclosed)

**GitHub Labels:** Check existing with `gh label list`. Format: `category:value` (e.g., `type:bug`, `priority:high`, `component:buffer`).

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

---

## E2E Test Rules

### UI Tests vs Lisp Tests

- **`ui/` tests:** Keyboard simulation ONLY. NEVER use `eval-lisp` or `evalLisp` (enforced by lint).
- **`lisp/` tests:** MAY use `eval-lisp` to test Lisp API directly.

### Keyboard-Only Testing

UI tests simulate user actions through helper functions: `h/type-text`, `h/press-ctrl`, `h/press-ctrl-x`, `h/press-meta`, `h/press-key`. Verify state with `h/get-buffer-text*`, `h/get-echo-area-text`, `h/get-mode-line-text`.

### Test Helper Encapsulation

**FORBIDDEN in test helpers:**
- Direct state access: `@rfdb/app-db`, `(get-in @rfdb/app-db ...)`
- Direct state mutation: `(swap! rfdb/app-db ...)`
- Implementing application logic (kill ring, undo, keymaps, modes)

**All test operations MUST go through `src/lexicon/api/test.cljs`.**

### Skipped Tests

Use `^:skip` only for features planned but not yet implemented (e.g., backend-dependent features). Never skip to hide bugs or avoid hard implementation.

### Running Tests

```bash
bb test:e2e                    # All E2E tests
bb test:e2e <pattern>          # Specific tests (e.g., bb test:e2e ui.editing.undo-test)
bb test:e2e <pattern> 2>&1 | tee /tmp/e2e-<feature>.log  # With log capture
```

### Debugging E2E Tests

See `.claude/DEBUGGING_E2E_TESTS.md` for the comprehensive guide. Quick reference:
- Use `lexicon.log/debug` for E2E test debugging
- Messages buffer is automatically captured on test failure
- Log at decision points, not just entry/exit
- Remove debug logs after fixing issues

### Writing New Tests

See `.claude/skills/write-test/SKILL.md` for templates and conventions.

---

## State Ownership (CRITICAL)

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

### Minibuffer Architecture

**IMPORTANT:** The minibuffer uses `:minibuffer` map directly (NOT a stack).

```clojure
;; Reading:
(get-in db [:minibuffer :input])

;; Writing:
(assoc-in db [:minibuffer :input] new-input)
```

Do NOT mix stack-based and map-based access patterns.

---

## Engineering Standards

### Critical Rules

**NEVER:**
- Run `git commit` - Generate the commit message and let the user commit
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
- Run `bb lint` before commit

### Commit Format

```
type(scope): concise description

- Detailed bullet points
- Explain what and why

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Types:** `feat`, `fix`, `refactor`, `test`, `docs`, `deps`

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
packages/editor-cljs/src/lexicon/     # ClojureScript source
packages/editor-cljs/src/lexicon/core/  # Internal core (events, modes, UI)
packages/editor-cljs/src/lexicon/lisp.cljs  # Public API boundary
packages/lexicon-engine/wasm/         # Rust WASM gap buffer
e2e_tests/                            # E2E tests
~/projects/lexpkgs/                   # External packages (vertico, marginalia, orderless)
~/projects/lexpa/                     # Package registry (recipes, server)
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

## Browser Testing (Chrome Extension)

E2E tests (Etaoin/WebDriver) test API contracts but can miss integration issues,
especially for external SCI packages that are loaded at runtime. Use the Chrome
Extension (`/chrome-test` skill) to verify features work end-to-end in the browser.

**Key caveat:** Changes to external packages in `lexpkgs/` require reinstalling
the package in the browser (e.g., `(install-package "vertico")`) -- shadow-cljs
hot-reload only covers core code, not SCI-loaded packages.

See `.claude/CHROME_EXTENSION_TESTING.md` for the full workflow.

---

## Remember

1. **GitHub first** - Check issues before starting
2. **Emacs fidelity** - Study Emacs source code
3. **State ownership** - Dispatch to owners, never mutate directly
4. **Test first** - Write tests before implementing
5. **Zero warnings** - Fix all warnings before commit
6. **Zero regressions** - All tests must pass
7. **Browser verify** - Test in browser after E2E, especially for package integration
8. **Reconcile docs** - Run `/reconcile` after adding features, packages, or API functions

---

**This file survives conversation compactions. Updated 2026-05-03.**
