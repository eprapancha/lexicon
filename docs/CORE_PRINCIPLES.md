# Lexicon: Core Architectural Principles

**Last Updated:** 2025-12-20
**Status:** Foundational Document - These principles override all other considerations

---

## Mission Statement

**Lexicon is a shameless recreation of GNU Emacs for the modern web.** We aim to stay true to Emacs in every possible way, so that anyone comfortable with Emacs cannot tell the difference in user experience. We use modern technologies (Rust, ClojureScript, WebAssembly) but remain faithful to Emacs's proven architectural patterns and philosophy.

---

## The Ten Commandments of Lexicon

### 1. **Emacs Purity Above All**

- When in doubt, **do what Emacs does**
- Study the Emacs source code (C and Elisp) before implementing features
- Emacs has 40+ years of battle-tested design - we learn from it, not reinvent it
- If a modern alternative contradicts Emacs patterns, **choose Emacs**

### 2. **Core Must Be Minimal and Sacred**

- Core is **bare Emacs** - what you get with `emacs -Q` (no packages)
- Core contains:
  - Buffer management (gap buffer)
  - Window/frame management
  - Keymap system
  - Command dispatcher
  - Mode system (major/minor)
  - Minibuffer
  - Basic editing commands (insert, delete, navigate)
  - File I/O (open, save)
  - Kill ring (cut/copy/paste)
- Core does **NOT** contain:
  - Evil-mode (vim emulation) → **PACKAGE**
  - Completion frameworks (vertico, orderless) → **PACKAGE**
  - Syntax highlighting beyond basic → **PACKAGE**
  - LSP integration → **BRIDGE + PACKAGE**
  - Themes beyond default → **PACKAGE**

### 3. **Gap Buffer, Not Piece Tree**

- Emacs uses gap buffers - so do we
- Gap buffers are simpler, faster for localized edits, and align with Emacs internals
- Piece trees are clever but introduce complexity misaligned with our goal
- **Decision is final** unless multi-cursor support or collaboration becomes core requirements

**Gap Buffer Principle:**
```
[Text before gap] [    GAP    ] [Text after gap]
                  ↑
                Point (cursor)
```

### 4. **Userspace/Kernel Split (Emacs C Core / Elisp Layer)**

**Rust/WASM "Kernel" (analogous to Emacs C core):**
- Gap buffer primitive operations
- UTF-8 text handling
- Performance-critical low-level operations
- Platform abstraction (via browser APIs)

**ClojureScript "Userspace" (analogous to Emacs Lisp layer):**
- Almost everything else
- All user-facing commands (even basic ones like `self-insert-command`)
- Mode definitions
- Keymap configurations
- UI rendering
- Extensibility system

**Philosophy:** "Only functions that need speed are in Rust" - steal Emacs's wisdom

### 5. **Hierarchical Keymaps (Emacs Precedence Order)**

Keymap lookup follows Emacs's exact precedence:

1. **Text/overlay properties at point** (highest precedence)
2. **Active minor mode keymaps** (reverse order of activation)
3. **Buffer's major mode keymap**
4. **Global keymap** (lowest precedence, always active)

Multi-key sequences (like `C-x C-f`) work via nested keymaps - prefix keys bind to keymaps, not commands.

### 6. **Modes: One Major, Many Minor**

- Each buffer has **exactly one major mode** (mutually exclusive)
- Each buffer can have **multiple minor modes** (composable)
- Modes define:
  - Keymap
  - Buffer-local variables
  - Hooks (activation/deactivation)
  - Syntax table (for major modes)

**Major modes** are for file types (text-mode, clojure-mode, org-mode)
**Minor modes** are for features (auto-fill-mode, evil-mode, line-number-mode)

### 7. **Packages Are External, Loadable, and Isolated**

- Packages live in `packages/` directory as separate namespaces
- Packages interact with core **only via public API**
- Packages must be **loadable and unloadable** without breaking core
- Evil-mode is the **canonical first package** - if core can't support it cleanly via API, core is incomplete

**Package structure:**
```
packages/
├── lexicon-core/         # The bare Emacs
└── evil-mode/            # First external package
    └── src/lexicon/evil/
        ├── core.cljs     # Package initialization
        ├── fsm.cljs      # Modal state machine
        ├── operators.cljs
        ├── motions.cljs
        └── keymaps.cljs
```

### 8. **Simplicity Beats Cleverness**

- Choose the simpler implementation when both work
- Debuggability is more important than theoretical performance
- Code should be understandable to someone learning the codebase
- When complexity is unavoidable, isolate it and document thoroughly

**Example:** Gap buffer (simple) beats piece tree (clever) for our use case

### 9. **Incremental Progress, Working Software**

- Each phase must result in **working, usable software**
- Basic functionality (type text, press Enter, see newline) comes before advanced features
- No phase is "complete" until it can be demonstrated working in browser
- We may fail, but we fail with working demos at each milestone

### 10. **Documentation Reflects Reality**

- README describes **actual current state**, not aspirations
- Mark features as:
  - ✅ Working and tested
  - 🔄 In progress
  - 🔲 Planned but not started
  - ⚠️ Broken/needs fixing
- Architecture docs are living documents, updated as decisions change
- When code and docs diverge, **code wins** - update docs immediately

---

## Technology Choices (Fixed)

### Core Stack

- **ClojureScript + re-frame** - Userspace (Elisp layer analog)
- **Rust + WebAssembly** - Kernel (C core analog)
- **Reagent** - View layer (React wrapper)
- **Shadow-cljs** - Build tool
- **Nix** - Reproducible dev environment

### Data Structures

- **Gap buffer** - Text representation (Rust)
- **Immutable maps** - App state (ClojureScript)
- **Red-black tree** - Future: Fast keymap lookups (if needed)

### Not Using

- ❌ Piece tree - Too complex for our needs
- ❌ CodeMirror - We're building our own editor
- ❌ Monaco - Same reason
- ❌ AssemblyScript - Switched to Rust for maturity

---

## Core API Contract

The core must provide these APIs for packages:

### Buffer API
```clojure
(lexicon.core.buffer/create-buffer name)
(lexicon.core.buffer/get-text buffer-id)
(lexicon.core.buffer/insert-at-point buffer-id text)
(lexicon.core.buffer/delete-region buffer-id start end)
(lexicon.core.buffer/point buffer-id)
(lexicon.core.buffer/set-point buffer-id position)
```

### Command API
```clojure
(lexicon.core.command/register-command! name definition)
(lexicon.core.command/execute-command name & args)
(lexicon.core.command/call-interactively command-name)
```

### Keymap API
```clojure
(lexicon.core.keymap/define-keymap name bindings)
(lexicon.core.keymap/set-global-key key-sequence command)
(lexicon.core.keymap/lookup-key key-sequence)
```

### Mode API
```clojure
(lexicon.core.mode/define-major-mode name definition)
(lexicon.core.mode/define-minor-mode name definition)
(lexicon.core.mode/enable-minor-mode! mode-name)
(lexicon.core.mode/disable-minor-mode! mode-name)
```

### Hook API
```clojure
(lexicon.core.hooks/add-hook hook-name function)
(lexicon.core.hooks/remove-hook hook-name function)
(lexicon.core.hooks/run-hooks hook-name)
```

---

## Decision Log

### Major Decisions

| Date | Decision | Rationale |
|------|----------|-----------|
| 2025-12-20 | Gap buffer over piece tree | Emacs alignment, simplicity, typical editing patterns favor gap buffers |
| 2025-12-20 | Evil-mode as package, not core | Core = bare Emacs, modal editing is optional feature |
| 2025-12-20 | Architecture reset/refactor | Current code has Evil mixed into core, basic input broken, complexity exceeded debuggability |

### Principles Violated (Learn From These)

| What Happened | Principle Violated | Lesson Learned |
|---------------|-------------------|----------------|
| Evil-mode FSM in core namespace | #2: Core Must Be Minimal | Extract to packages/ immediately when starting new feature |
| Piece tree complexity | #8: Simplicity Beats Cleverness | Choose simpler proven approach aligned with Emacs |
| README claiming working features | #10: Documentation Reflects Reality | Update README immediately when features break |
| Enter key broken | #9: Incremental Progress | Test basic functionality before moving to advanced features |

---

## Review Checklist

Before committing major changes, verify:

- [ ] Does this align with how Emacs does it?
- [ ] If in core, is it truly needed for bare Emacs?
- [ ] If complexity is added, is it isolated and documented?
- [ ] Does the feature work end-to-end before moving on?
- [ ] Does README accurately reflect what works?
- [ ] Can packages access needed functionality via clean API?

---

## Contact and Governance

This is an experimental learning project. Principles can evolve based on experience, but changes to core principles require:

1. Documentation of why the principle needs changing
2. Update to this document with date and rationale
3. Review of all code affected by the principle change

**Current Status:** Active refactoring to align with these principles (Dec 2025)

---

*These principles exist to keep us honest and aligned with our mission: building a true Emacs for the web.*
