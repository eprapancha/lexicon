---
name: design-spec
description: Design an implementation specification for a Lexicon feature. Produces detailed specs with file paths, state changes, API additions, and acceptance criteria.
argument-hint: [feature e.g. "consult-line", "evil-normal-mode", "tree-sitter-highlighting"]
allowed-tools: Read, Glob, Grep
---

# Design Specification

Design an implementation plan for a Lexicon feature. Read the codebase, understand existing patterns, and produce a detailed spec that can be followed without making architectural decisions.

---

## Governing Documents

Read these before every design decision:
- `CLAUDE.md` -- engineering standards, state ownership map
- `docs/ARCHITECTURE.org` -- system architecture, app-db schema
- `docs/ARCHITECTURE_BOUNDARY.org` -- core vs package boundary
- `docs/EMACS_COMPATIBILITY_CONTRACT.org` -- semantic guarantees

---

## Critical Rules

### State Ownership
Every state key has exactly one owner. Check `CLAUDE.md` ownership map. New state keys must have explicit ownership assigned.

### Core vs Package Boundary
- Packages import ONLY `lexicon.lisp`
- If a package needs something not in `lexicon.lisp`, the spec must include adding the primitive AND registering it in `sci-namespace`

### External Packages Run in SCI
External packages can ONLY call functions in `lexicon.lisp/sci-namespace`. New API surface for packages requires:
1. The `lexicon.lisp` function (signature, behavior)
2. The `sci-namespace` registration entry

### Minibuffer Architecture
The minibuffer uses a flat `:minibuffer` map, NOT a stack.

---

## Process

1. **Understand the feature** from the argument and any prior research
2. **Read existing code** at every file that will be affected
3. **Study Emacs source** if needed (`~/projects/emacs-source/`)
4. **Design the implementation** following existing patterns
5. **Produce the spec** in the output format below

---

## Output Format

```
## Spec: [feature name]

### Goal
[One sentence: what this achieves]

### Files to Modify
- `path/to/file.cljs` -- [what changes]

### Files to Create (if any)
- `path/to/new.cljs` -- [purpose]

### State Changes
- New key: `[:path :to :key]` -- owned by [module], type: [type]
- Modified key: `[:path :to :key]` -- [what changes]

### Event Handlers
- `:event/name` -- [what it does, input, output]

### Public API Changes (lexicon.lisp)
- `(function-name args)` -- [what it does, return value]
- sci-namespace registration: YES/NO

### Implementation Notes
- [specific guidance]
- [edge cases to handle]
- [things NOT to do]

### Acceptance Criteria
- [ ] [testable assertion]
- [ ] `bb lint` passes
- [ ] No state ownership violations

### Verification
[Commands to run to verify the implementation]
```

---

## Rules

- File paths must be exact, not approximate
- State ownership must be explicit for every new or modified key
- Don't design for hypothetical future requirements
- Follow existing patterns (study neighboring code)
- If the feature should be a core change vs external package, state which and why
- This is read-only design -- never modify any files
