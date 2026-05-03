---
name: emacs-study
description: Research how GNU Emacs implements a specific feature by reading Emacs source code. Produces structured analysis that can inform Lexicon implementation.
argument-hint: [feature e.g. "completion-styles", "buffer-local-variables", "org-mode-folding"]
allowed-tools: Read, Glob, Grep, WebSearch, WebFetch
---

# Emacs Source Study

Research how GNU Emacs implements a specific feature by systematically reading Emacs source code. Produce structured findings that can inform Lexicon's implementation.

---

## Source Locations

- **Emacs source:** `~/projects/emacs-source/`
  - C core: `src/*.c` (DEFUNs, buffer internals, redisplay)
  - Lisp layer: `lisp/*.el` (commands, modes, completion)
  - Documentation: `doc/*.texi`

---

## Process

1. **Identify the feature** from the argument
2. **Find relevant source files** using grep/glob
3. **Read source code** systematically, not superficially
4. **Trace execution paths** -- follow function calls through C and Lisp layers
5. **Identify key data structures** -- what state does the feature maintain?
6. **Document the API contract** -- functions, arguments, return values
7. **Note edge cases** that Emacs handles explicitly
8. **Identify dependencies** -- what other features/subsystems does this rely on?

---

## Where to Look First

| Feature Area | Start Here |
|-------------|-----------|
| Completion | `lisp/minibuffer.el`, `lisp/simple.el` |
| Buffers | `src/buffer.c`, `src/insdel.c` |
| Windows | `src/window.c`, `lisp/window.el` |
| Keymaps | `src/keymap.c`, `lisp/subr.el` |
| Modes | `lisp/progmodes/`, `lisp/textmodes/` |
| Display | `src/xdisp.c`, `src/dispnew.c` |
| Search | `src/search.c`, `lisp/isearch.el` |
| Kill ring | `lisp/simple.el` (kill-region, yank) |
| Undo | `src/undo.c`, `lisp/simple.el` |
| Text properties | `src/textprop.c`, `src/intervals.c` |
| Overlays | `src/buffer.c` (overlay functions) |
| Font-lock | `lisp/font-lock.el`, `lisp/font-core.el` |
| org-mode | `lisp/org/` |
| Package system | `lisp/emacs-lisp/package.el` |

---

## Output Format

```
## Feature: [name]

### Source Files
- [file:line] -- [what this file contributes]

### Key Functions
- `function-name` (file:line) -- [what it does, args, return value]

### Data Structures
- [describe state maintained by this feature]

### Execution Flow
1. [step-by-step trace of how the feature works]

### Dependencies
- [what other subsystems this relies on]

### Edge Cases
- [explicit handling Emacs does that might be missed]

### Relevance to Lexicon
- [what Lexicon should replicate]
- [what can be simplified for the browser context]
- [what's impossible without a backend]
```

---

## Rules

- Read source code -- don't guess or rely on documentation alone
- Cite specific file paths and line numbers
- Trace actual execution, don't hand-wave
- Distinguish between essential behavior and implementation artifacts
- Note when Emacs behavior is C-level vs Lisp-level (affects what Lexicon needs)
- If the feature spans multiple files, map the full dependency graph
- This is read-only research -- never modify any files
