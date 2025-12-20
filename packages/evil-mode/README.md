# Evil-mode Package

Vim/Evil-style modal editing for Lexicon.

## Status

⚠️ **NOT YET FUNCTIONAL** - Extracted from core during Phase 0 refactoring.

This package contains all the Evil-mode (Vim emulation) code that was previously mixed into the core editor. It needs to be refactored to work as an external package that interacts with core via clean APIs.

## Contents

- `fsm/` - Finite State Machine for modal editing (Normal, Insert, Visual modes)
- `command/` - Operator-motion composition system (Vim's "verb-noun" grammar)
- `keymaps/` - Evil-specific keymaps (hjkl navigation, etc.)
- `macros.clj` - Metaprogramming DSL (`def-evil-motion`, `def-evil-operator`, etc.)

## Architecture

Evil-mode will be implemented as a **minor mode** that can be toggled on/off without affecting core Emacs functionality.

### Future API (Phase 6)

```clojure
;; User's init file
(lexicon.packages/load-package! :evil-mode)

;; Enable evil-mode
(evil-mode 1)

;; Disable evil-mode
(evil-mode -1)
```

### Implementation Plan (Phase 6)

1. **Package initialization**
   - `evil/core.cljs` - Package entry point with `initialize!` function
   - Register Evil commands via `lexicon.core.command/register-command!`

2. **FSM as internal detail**
   - Modal state machine (Normal/Insert/Visual) is internal to evil-mode
   - Not exposed to core or other packages

3. **Keymaps**
   - Evil normal mode → Minor mode keymap
   - Evil insert mode → Minor mode keymap
   - Evil visual mode → Minor mode keymap

4. **Operator-motion composition**
   - `dw` (delete word), `ciw` (change inner word), etc.
   - Implemented via Evil's internal command dispatcher

## Dependencies on Core

Evil-mode requires these APIs from core (to be implemented in Phase 1-5):

- **Buffer API**: `insert`, `delete`, `get-text`, `point`, `set-point`
- **Command API**: `register-command!`, `execute-command`
- **Keymap API**: `define-keymap`, `lookup-key`
- **Mode API**: `define-minor-mode`, `enable-minor-mode`, `disable-minor-mode`
- **Hook API**: `add-hook`, `remove-hook`

## Current State

**Extracted:** 2025-12-20 (Phase 0)
- All Evil code moved from `editor-cljs/src/lexicon/` to `evil-mode/src/lexicon/evil/`
- Core no longer imports Evil macros or FSM code
- Evil-mode does not yet load or function

**Next Steps:** Wait for Phase 6 (after core is complete)
1. Implement package loading system
2. Refactor Evil to use core APIs
3. Test Evil as external package
4. Ensure Evil can be disabled without breaking core

## Philosophy

Evil-mode demonstrates that Lexicon's package system works: complex features like modal editing should live in userspace, not core. This separation ensures core remains minimal and maintainable.

---

**See:** [/docs/ROADMAP.md](/docs/ROADMAP.md) Phase 6 for implementation timeline
