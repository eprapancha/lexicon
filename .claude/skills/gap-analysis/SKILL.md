---
name: gap-analysis
description: Compare Emacs behavior against Lexicon's current implementation to identify gaps, missing features, and incorrect behavior. Use after studying Emacs source or when planning a new feature.
argument-hint: [feature e.g. "completion-system", "kill-ring", "window-management"]
allowed-tools: Read, Glob, Grep
---

# Gap Analysis

Compare what Emacs does against what Lexicon currently does for a specific feature. Produce a precise gap report categorizing what's correct, what's missing, and what's wrong.

---

## Lexicon Codebase Locations

All paths relative to `/home/nixos/projects/lexicon/`:

- **Core events:** `packages/editor-cljs/src/lexicon/core/events/`
- **Completion:** `packages/editor-cljs/src/lexicon/core/completion/`
- **Public API:** `packages/editor-cljs/src/lexicon/lisp.cljs`
- **State schema:** `packages/editor-cljs/src/lexicon/core/db.cljs`
- **Minibuffer:** `packages/editor-cljs/src/lexicon/core/minibuffer.cljs`
- **Views:** `packages/editor-cljs/src/lexicon/core/views.cljs`
- **Modes:** `packages/editor-cljs/src/lexicon/core/modes/`
- **WASM engine:** `packages/lexicon-engine/wasm/src/`
- **External packages:** `~/projects/lexpkgs/`
- **Tests:** `e2e_tests/lexicon/`

---

## Process

1. **Identify the feature** from the argument
2. **Read Emacs source** (or use `/emacs-study` findings if available)
3. **Search Lexicon's codebase** for corresponding implementations
4. **Compare behavior** -- does Lexicon's version match Emacs semantics?
5. **Categorize each gap** as foundation, API surface, surface, or incorrect
6. **Prioritize** -- what blocks everything else vs what can be done later?

---

## Gap Categories

### Foundation Gap
Core infrastructure is missing or wrong. Must be fixed before the feature can be built. Examples:
- Missing event handler that other features depend on
- State schema doesn't support required data
- `lexicon.lisp` missing a primitive

### API Surface Gap
A function needed by external packages doesn't exist in `lexicon.lisp/sci-namespace`. Always check BOTH:
1. Does the function exist in `lexicon.lisp`?
2. Is it registered in the `sci-namespace` map?

### Surface Gap
Foundation exists but specific behavior is missing or wrong. Can be built on existing infrastructure.

### Incorrect Behavior
Lexicon does something but it's semantically wrong compared to Emacs. These are bugs.

---

## Output Format

```
## Gap Analysis: [feature]

### Summary
- Foundation gaps: N
- API surface gaps: N
- Surface gaps: N
- Incorrect behaviors: N
- Already correct: N

### Foundation Gaps (must fix first)
1. **[gap name]**
   - Emacs: [what Emacs does]
   - Lexicon: [what Lexicon does or doesn't do, citing file:line]
   - Impact: [what breaks if this isn't fixed]

### API Surface Gaps
1. **[gap name]**
   - Function needed: `function-name`
   - Emacs equivalent: `emacs-function-name`
   - In lisp.cljs: YES/NO
   - In sci-namespace: YES/NO

### Surface Gaps
1. **[gap name]**
   - Emacs: [expected behavior]
   - Lexicon: [current behavior or absence]

### Incorrect Behaviors
1. **[bug name]**
   - Expected: [Emacs behavior]
   - Actual: [Lexicon behavior, file:line]

### Already Correct
- [list of things that work]

### Recommended Implementation Order
1. [foundation gap] -- blocks everything
2. [surface gaps] -- can be parallelized
```

---

## Rules

- Check every function/behavior, don't skip because the happy path works
- Cite specific file paths and line numbers in Lexicon
- Foundation vs surface categorization must be accurate
- The recommended order must reflect real dependencies, not just severity
- This is read-only analysis -- never modify any files
