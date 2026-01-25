# Lexicon Architecture: Core vs Packages Boundary

## The Problem (Solved)

Previously, packages and core were in the same namespace (`lexicon.*`), making it
too easy to violate the boundary. Now we have physical separation:

- `lexicon.core.*` - INTERNAL (packages must NEVER import)
- `lexicon.lisp` - PUBLIC API (the ONLY thing packages can import)
- `lexicon.<package>` - Packages (e.g., `lexicon.dired`)

## How Emacs Does It

```
┌─────────────────────────────────────┐
│  Packages (Elisp)                   │  ← Can ONLY use Lisp primitives
│  - dired.el, org.el, etc.           │
├─────────────────────────────────────┤
│  Lisp Primitives API                │  ← THE BOUNDARY
│  - buffer-*, point, insert, etc.    │
├─────────────────────────────────────┤
│  C Core                             │  ← Packages can't touch this
│  - gap buffer, redisplay, GC        │
└─────────────────────────────────────┘
```

## What Lexicon Is

```
┌─────────────────────────────────────┐
│  Packages (top-level)               │  ← ONLY uses lexicon.lisp API
│  - lexicon.dired, lexicon.org       │     No re-frame, no app-db access
├─────────────────────────────────────┤
│  lexicon.lisp (Public API)          │  ← THE BOUNDARY
│  - All Emacs primitives exposed     │
├─────────────────────────────────────┤
│  lexicon.core.* (Internal)          │  ← Packages can't touch this
│  - events/, WASM, rendering         │     (enforced by lint script)
└─────────────────────────────────────┘
```

## Directory Structure

```
packages/editor-cljs/src/lexicon/
├── dired.cljs              # PACKAGE - uses ONLY lexicon.lisp
├── lisp.cljs               # PUBLIC API - the boundary
│
└── core/                   # INTERNAL - packages must NEVER import from here
    ├── main.cljs           # Entry point
    ├── package_loader.cljs # Loads packages (correct dependency direction)
    ├── events.cljs         # Core events
    ├── db.cljs             # State schema
    ├── subs.cljs           # Subscriptions
    ├── events/             # Event handlers
    │   ├── buffer.cljs
    │   ├── edit.cljs
    │   ├── window.cljs
    │   ├── keymap.cljs
    │   ├── mode.cljs
    │   ├── isearch.cljs
    │   ├── hooks.cljs
    │   ├── text_properties.cljs
    │   └── command.cljs
    ├── api/                # Internal API helpers
    ├── ui/                 # UI components
    ├── modes/              # Core modes
    └── ...
```

## Dependency Direction (CRITICAL)

```
lexicon.core.main
    ↓
lexicon.core.package-loader  ← packages loaded HERE
    ↓
lexicon.dired                ← package at top-level
    ↓
lexicon.lisp                 ← PUBLIC API (only thing packages can use)
```

**Rule: Core NEVER imports packages. Package loading happens through package_loader.cljs**

## Enforcement

### Babashka Task

Run `bb lint` to check for violations:

```bash
bb lint
```

This task:
- Finds all package files (*.cljs at src/lexicon/, except lisp.cljs)
- Fails if any import from `lexicon.core.*`
- Fails if any import from `re-frame.*`
- Fails if any access `re-frame.db` directly

### CI Integration

Add to your CI pipeline:
```yaml
- name: Run linters
  run: bb lint
```

## What IS Core (lexicon.core.*)

These belong in `core/` as re-frame handlers:

1. **Primitives** - Gap buffer operations, cursor movement, text manipulation
2. **Buffer System** - Create, switch, kill buffers
3. **Window System** - Split, delete, resize windows
4. **Keymap System** - Key binding lookup and dispatch
5. **Mode Framework** - Major/minor mode infrastructure
6. **Minibuffer** - Prompt and completion system
7. **Command Infrastructure** - Registration and execution
8. **Hooks** - Hook system for extensibility
9. **Text Properties** - Property attachment to text ranges
10. **Undo System** - Undo/redo infrastructure
11. **Isearch** - Incremental search (core feature, uses primitives internally)

## What IS NOT Core (Packages)

These are implemented using ONLY `lexicon.lisp` API:

1. **Dired** - Directory editor (`lexicon.dired`)
2. **Org-mode** - Outlining (future, `lexicon.org`)
3. **Magit** - Git interface (future, `lexicon.magit`)
4. **Custom modes** - User-defined major/minor modes

## The Litmus Test

> "Can this feature be implemented using only functions in `lexicon.lisp`?"

- If YES → It's a package, put it at `lexicon/<name>.cljs`
- If NO → Either add the missing primitive to lexicon.lisp, OR it's core

## Package Implementation Rules

1. **Namespace**: `lexicon.<name>` (top-level, e.g., `lexicon.dired`)
2. **Imports**: ONLY `[lexicon.lisp :as lisp]`
3. **Registration**: Use `lisp/define-command` to register commands
4. **Loading**: Added to `core/package_loader.cljs`
5. **No re-frame**: Packages must not import re-frame or access state directly

## Example Package Structure

```clojure
(ns lexicon.dired
  "Dired package - uses ONLY lexicon.lisp API"
  (:require [lexicon.lisp :as lisp]))

(defn dired [directory]
  (lisp/switch-to-buffer (str "*dired " directory "*"))
  (lisp/insert-directory directory)
  (lisp/set-buffer-read-only true)
  (lisp/set-major-mode 'dired-mode))

(defn register-dired-package! []
  (lisp/define-command 'dired dired "Open directory in Dired"
    {:interactive [{:type :async :prompt "Directory: "}]}))
```

## Implementation Status

### Completed
- [x] Documented core vs packages boundary
- [x] Created `lexicon.dired` as a proper package
- [x] Added filesystem primitives to `lexicon.lisp`
- [x] Refactored isearch to use `lexicon.lisp` primitives
- [x] Established correct dependency direction via `package_loader.cljs`
- [x] **Moved all core code to `lexicon.core.*` namespace**
- [x] **Created `bb lint` task to enforce boundary**
- [x] **Added `bb lint` to CI pipeline**
- [ ] Move more features to packages as appropriate
- [ ] Add more primitives to `lexicon.lisp` as needed
