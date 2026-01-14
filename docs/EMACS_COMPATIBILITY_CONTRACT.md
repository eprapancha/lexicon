# Emacs Compatibility Contract

**Version:** 1.0
**Status:** Normative
**Last Updated:** 2026-01-15

---

## Purpose

This document defines what it means for Lexicon to be "Emacs-compatible."

If code behavior and this contract disagree, **the contract wins** until this document is explicitly updated through documented review.

This is not a feature roadmap. This is a binding specification.

---

## 1. Scope of Compatibility

### 1.1 What Lexicon Guarantees

Lexicon targets **behavioral semantics of interactive editing**, not internal implementation details.

Lexicon SHALL behave like Emacs for:

- Buffer manipulation (insertion, deletion, movement)
- Point and mark semantics
- Kill ring and region operations
- Command execution lifecycle
- Keybinding resolution and dispatch
- Minibuffer interaction patterns
- Buffer-local variable semantics
- Hook execution order and guarantees
- Marker behavior under text modification

### 1.2 What Lexicon Does NOT Guarantee

Lexicon explicitly does NOT target:

- Emacs Lisp bytecode compatibility
- C-level internal APIs (e.g., `Lisp_Object` structures)
- Emacs frame/window geometry primitives
- Terminal escape sequence handling
- Native subprocess management
- Redisplay timing guarantees
- Exact error symbol hierarchy
- Font rendering internals
- Process filters and sentinels
- Direct filesystem access patterns identical to Emacs

### 1.3 Runtime Constraints

Lexicon operates under browser/WASM constraints:

- No direct filesystem access (uses browser APIs)
- No native processes (async operations via browser APIs)
- Single-threaded JavaScript event loop
- Memory managed by JavaScript runtime

These constraints are **first-class design inputs**, not limitations to be hidden.

---

## 2. Execution Model Invariants

### 2.1 Single Semantic Thread

Lexicon MUST maintain the illusion of single-threaded execution for all editor state.

This means:

- Commands execute atomically from the editor's perspective
- No concurrent mutations to buffer contents, point, or mark
- State transitions are sequential and deterministic

### 2.2 Commands Run to Completion

A command MUST run to completion before the next command begins.

Partial execution is not permitted.

This guarantee enables:

- Predictable `this-command` and `last-command` semantics
- Hook execution without race conditions
- Undo boundaries aligned with command boundaries

### 2.3 Asynchronous Work Resumes Via Command Enqueueing

Async operations (file loading, network requests, timers) MUST resume by enqueueing new commands onto the command queue.

They SHALL NOT directly mutate editor state.

### 2.4 Command Lifecycle Phases

Every command execution SHALL proceed through these phases:

1. **Pre-command hooks** - Execute in registration order
2. **Command execution** - Runs to completion
3. **Post-command hooks** - Execute in registration order
4. **State cleanup** - Prefix args cleared, transient state reset

This order is guaranteed and non-negotiable.

---

## 3. Dynamic Scoping Commitment

### 3.1 Dynamic Variables Are Guaranteed

Lexicon guarantees dynamic scoping semantics equivalent to Emacs for editor state.

The following variables MUST be dynamically bound during command execution:

- `this-command` - Currently executing command
- `last-command` - Previously executed command
- `current-prefix-arg` - Prefix argument during command execution
- `current-buffer` - Buffer in which command is executing (when implemented)
- `point` - Current point position (when directly accessible)
- `mark-active` - Whether mark is active

### 3.2 Buffer-Local Variables Use Dynamic Lookup

Buffer-local variables MUST use dynamic lookup order:

1. Check buffer-local binding in current buffer
2. Fall back to global binding
3. Return nil if unbound

This lookup MUST happen at access time, not definition time.

### 3.3 Lexical Purity Is Not a Goal

Lexicon does NOT optimize for lexical scoping or functional purity.

Elisp-style code and SCI evaluation contexts rely on dynamic scope.

This is a conscious design choice, not a compromise.

---

## 4. Core Abstractions Guaranteed by Lexicon

### 4.1 Buffers

**What they are:**
Buffers are containers for text with associated metadata and local state.

**Guarantees:**
- Every buffer has a unique identity
- Buffers persist until explicitly killed
- Buffer contents can be modified via insertion, deletion, and replacement
- Buffers have a current point position (cursor)
- Buffers can have buffer-local variables

**Non-guarantees:**
- Lexicon does NOT guarantee identical buffer object structure to Emacs
- Lexicon does NOT guarantee binary compatibility with Emacs buffer APIs

---

### 4.2 Point and Mark

**What they are:**
Point is the current cursor position. Mark is a saved position for region operations.

**Guarantees:**
- Point is always a valid position within the buffer (1 to buffer-size + 1)
- Point moves in response to insertion and deletion commands
- Mark, when set, defines a region from mark to point
- Region operations (kill, copy) operate on text between mark and point

**Non-guarantees:**
- Lexicon does NOT guarantee mark ring implementation details
- Lexicon does NOT guarantee transient-mark-mode behavior (may differ)

---

### 4.3 Markers

**What they are:**
Markers represent semantic positions within a buffer that move in response to text changes.

**Guarantees:**
- Markers are NOT simple numeric positions
- Markers move in response to text insertion and deletion in a manner consistent with Emacs
- Markers inserted before a position move when text is inserted at that position
- Markers are invalidated when their buffer is killed

**Non-guarantees:**
- Lexicon does NOT guarantee binary marker API compatibility
- Lexicon does NOT guarantee insertion-type behavior matches Emacs in all edge cases

---

### 4.4 Buffer-Local Variables

**What they are:**
Variables that can have different values in different buffers.

**Guarantees:**
- Variables can be made buffer-local via `make-local-variable`
- Variables can be made automatically buffer-local in all buffers via `make-variable-buffer-local`
- Buffer-local values are independent across buffers
- Killing a buffer releases its buffer-local bindings

**Non-guarantees:**
- Lexicon does NOT guarantee identical default-value behavior for all variables
- Lexicon does NOT guarantee terminal-local or frame-local variables

---

### 4.5 Hooks

**What they are:**
Hooks are lists of functions called at specific points in editor operation.

**Guarantees:**
- Hooks execute in registration order
- Hooks can be added, removed, and queried
- Pre-command and post-command hooks are guaranteed
- Mode hooks (on enable/disable) are guaranteed

**Non-guarantees:**
- Lexicon does NOT guarantee all Emacs hook names exist
- Lexicon does NOT guarantee hook depth limits match Emacs exactly

---

### 4.6 Commands

**What they are:**
Commands are functions that can be invoked interactively via keybindings or M-x.

**Guarantees:**
- Commands are first-class values with metadata (docstring, interactive spec)
- Commands can be registered, unregistered, and introspected
- Interactive specs define how commands receive arguments
- Prefix arguments (C-u) are available to commands via interactive spec codes "P" and "p"

**Non-guarantees:**
- Lexicon does NOT guarantee all Emacs interactive spec codes are supported
- Lexicon does NOT guarantee command remapping semantics

---

### 4.7 Minibuffer

**What it is:**
The minibuffer is a control plane for command input, completion, and user interaction.

**Guarantees:**
- The minibuffer is NOT just a text input widget
- The minibuffer supports recursive activation (depth tracking)
- Commands can activate the minibuffer while the minibuffer is active
- The minibuffer has a stack-based state architecture
- Completion candidates can be displayed and navigated

**Non-guarantees:**
- Lexicon does NOT guarantee identical minibuffer window behavior
- Lexicon does NOT guarantee all Emacs completion styles
- Lexicon does NOT guarantee exact completing-read API parity

---

### 4.8 Keymaps

**What they are:**
Keymaps define bindings from key sequences to commands.

**Guarantees:**
- Keymaps can be global, mode-local, or transient
- Key lookup follows precedence: transient → mode → global
- Prefix keys and key sequences are supported
- Keymaps can be activated and deactivated dynamically

**Non-guarantees:**
- Lexicon does NOT guarantee keymap inheritance semantics match Emacs
- Lexicon does NOT guarantee menu-bar or tool-bar keymap support

---

### 4.9 Kill Ring

**What it is:**
The kill ring stores killed (cut) text for yanking (pasting).

**Guarantees:**
- Killed text is added to the kill ring
- Yank retrieves the most recent kill
- Kill ring can be cycled (yank-pop)

**Non-guarantees:**
- Lexicon does NOT guarantee kill ring size limits match Emacs
- Lexicon does NOT guarantee interprogram-paste behavior

---

## 5. Compatibility Is Measured by Tests

### 5.1 Source of Truth

Emacs compatibility is validated by **semantic tests derived from Emacs test suite**, not by:

- Visual similarity
- Anecdotal behavior
- User expectations
- Implementation details

### 5.2 Test Categories

Tests are classified into:

- **Category A (Pure Semantic):** Buffer, point, kill/yank, command lifecycle
- **Category B (Semantic + UI):** Minibuffer, completion, windows
- **Category C (Implementation-Specific):** Redisplay, frames, processes

Lexicon SHALL prioritize Category A, then Category B.

Category C tests are informative but not binding.

### 5.3 Test Harness

Lexicon maintains a dedicated test namespace `test/lexicon/emacs/` that:

- Re-expresses Emacs semantics in Lexicon's test language
- Uses editor-facing APIs (commands, public functions)
- Tests what packages will actually use
- Documents gaps explicitly when tests fail

---

## 6. Explicit Non-Goals

### 6.1 These Are NOT Compatibility Targets

Lexicon explicitly does NOT target:

- **Frame geometry:** Browser controls window management
- **Terminal emulation:** Not applicable in browser context
- **Byte-compiled elisp:** Source-level compatibility only
- **Exact error symbols:** Error messages are informative, not contractual
- **Process model:** No native subprocesses
- **Redisplay internals:** Rendering is browser-delegated
- **Font rendering:** Browser handles fonts
- **Exact keymap inheritance:** Simplified precedence model
- **All interactive spec codes:** Subset supported, documented explicitly

### 6.2 Why These Are Non-Goals

These are excluded because:

- They are browser/WASM constrained
- They are implementation details, not semantic behaviors
- They do not affect package compatibility
- They would increase complexity without proportional value

---

## 7. Amendment Process

### 7.1 When This Contract Can Change

This contract can be amended when:

- New Emacs-semantic guarantees are added
- Existing guarantees are found to be unimplementable
- Test-driven development reveals necessary clarifications

### 7.2 When This Contract CANNOT Change

This contract MUST NOT be amended to:

- Retroactively justify existing implementation choices
- Avoid difficult implementation work
- Accommodate shortcuts

### 7.3 Amendment Procedure

1. Propose change in GitHub issue tagged `contract-amendment`
2. Document rationale and impact
3. Update this document with version bump
4. Update affected tests
5. Merge only after explicit approval

---

## 8. Relationship to Implementation

### 8.1 Contract Supersedes Code

When implementation behavior contradicts this contract:

- The contract is correct
- The implementation is wrong
- File a bug against the implementation

### 8.2 Contract Is Not Exhaustive

This contract does NOT specify every behavior.

For behaviors not specified here:

- Tests derived from Emacs are authoritative
- Implementation may choose Emacs-compatible behavior
- Divergence must be documented

---

## 9. Future Compatibility Layers

### 9.1 Elisp Compatibility

When elisp compatibility is added via SCI or other means:

- Dynamic scoping guarantees in Section 3 apply
- Core abstractions in Section 4 must be accessible
- Execution model in Section 2 must be preserved

### 9.2 Package Compatibility

Packages (Vertico, Magit, Org) rely on:

- Behavioral semantics defined in this contract
- Tests validating those semantics
- Explicit non-goals preventing false assumptions

This contract is written to support package compatibility, not user-facing features.

---

## 10. Summary

Lexicon is Emacs-compatible when:

1. ✅ Behavioral semantics match Emacs (tested)
2. ✅ Execution model is single-threaded and atomic
3. ✅ Dynamic scoping is guaranteed
4. ✅ Core abstractions behave as specified
5. ✅ Non-goals are respected
6. ✅ Tests derived from Emacs pass

Lexicon is NOT Emacs-compatible when:

1. ❌ Only visual similarity is achieved
2. ❌ Implementation details are copied without semantic understanding
3. ❌ Tests are written to match current behavior instead of Emacs semantics

---

**End of Contract**

This document is the authoritative definition of Emacs compatibility for Lexicon.
