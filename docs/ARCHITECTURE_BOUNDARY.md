# Lexicon Architecture: Core vs Packages Boundary

## The Problem

Currently, "packages" like Dired are implemented as re-frame event handlers directly in
the core codebase. This means:

1. **No real separation** - Packages have full access to app-db, all events, WASM internals
2. **No isolation** - Packages can't be loaded/unloaded dynamically
3. **Testing is meaningless** - "Semantic tests" don't test the real API because packages bypass it
4. **Cheating is easy** - We can always reach into internals instead of fixing primitives

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

## What Lexicon Should Be

```
┌─────────────────────────────────────┐
│  Packages (top-level)               │  ← ONLY uses lexicon.lisp API
│  - lexicon.dired, lexicon.org       │     No re-frame, no app-db access
├─────────────────────────────────────┤
│  lexicon.lisp (Public API)          │  ← THE BOUNDARY
│  - All Emacs primitives exposed     │
├─────────────────────────────────────┤
│  Core (ClojureScript/re-frame)      │  ← Packages can't touch this
│  - events/, WASM, rendering         │
└─────────────────────────────────────┘
```

## Directory Structure (Current)

```
packages/editor-cljs/src/lexicon/
├── dired.cljs              # PACKAGE - uses ONLY lexicon.lisp
├── lisp.cljs               # PUBLIC API - the boundary
├── package_loader.cljs     # Loads packages (correct dependency direction)
├── core.cljs               # Entry point, imports package_loader
├── events.cljs             # Core events (NEVER imports packages)
├── events/                 # Core event handlers
│   ├── buffer.cljs
│   ├── edit.cljs
│   ├── window.cljs
│   ├── keymap.cljs
│   ├── mode.cljs
│   ├── isearch.cljs        # Core feature using lexicon.lisp primitives
│   ├── hooks.cljs
│   ├── text_properties.cljs
│   └── command.cljs
└── ...
```

## Dependency Direction (CRITICAL)

```
lexicon.core
    ↓
lexicon.package_loader    ← packages loaded HERE (not in events.cljs!)
    ↓
lexicon.dired             ← package at top-level
    ↓
lexicon.lisp              ← PUBLIC API (only thing packages can use)
```

**Rule: Core NEVER imports packages. Package loading happens through package_loader.cljs**

## What IS Core

These belong in `events/` as re-frame handlers:

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
11. **Isearch** - Incremental search (uses lisp primitives for buffer access)

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
4. **Loading**: Added to `package_loader.cljs` (not events.cljs!)
5. **Auto-register**: Packages call their registration function on load

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

;; Auto-register on load
(register-dired-package!)
```

## Implementation Status

### Completed
- [x] Documented core vs packages boundary
- [x] Created `lexicon.dired` as a proper package
- [x] Added filesystem primitives to `lexicon.lisp`
- [x] Refactored isearch to use `lexicon.lisp` primitives
- [x] Established correct dependency direction via `package_loader.cljs`

### Future
- [ ] Lint rule: packages can only import `lexicon.lisp`
- [ ] Move more features to packages as appropriate
- [ ] Add more primitives to `lexicon.lisp` as needed
