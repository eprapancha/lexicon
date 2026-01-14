# Emacs Compatibility Contract

## Status

**Normative / Binding**

If code behavior and this contract disagree, **this contract wins**.
Changes to this document require explicit review and justification.

---

## 1. Scope of Compatibility

Lexicon guarantees **behavioral semantics of interactive editing** equivalent to GNU Emacs, subject to the constraints and exclusions explicitly stated in this document.

### 1.1 What “Emacs-compatible” means in Lexicon

Lexicon guarantees that:

* Interactive editor behavior (commands, buffers, motion, killing/yanking, hooks, minibuffer interaction) behaves **semantically** like Emacs.
* Compatibility is defined in terms of **observable outcomes**, not internal APIs or implementation details.
* Editor-facing behavior that is specified in this document **must match Emacs semantics exactly**, not approximately.

Lexicon does **not** guarantee:

* Binary compatibility with Emacs Lisp
* Compatibility with Emacs internal C APIs
* Exact redisplay timing or frame geometry behavior
* Terminal escape semantics or TTY behavior

Browser and WASM constraints are first-class and explicitly acknowledged.

---

## 2. Execution Model Invariants

Lexicon guarantees a **single semantic execution model** consistent with Emacs.

### 2.1 Single Semantic Thread

* Editor state mutation occurs on a single semantic thread.
* There is no concurrent mutation of editor state.
* Asynchronous operations resume editor interaction only via command scheduling.

### 2.2 Atomic Command Execution

* Commands run to completion.
* No other command may interleave during execution.
* All observable state transitions occur within command boundaries.

### 2.3 Command Lifecycle Phases

Each command executes with the following guaranteed ordering:

1. Pre-command hooks
2. Interactive argument resolution
3. Command body execution
4. Post-command hooks
5. Command cleanup

This ordering is guaranteed and relied upon by editor semantics.

### 2.4 Prefix Argument Semantics

* Prefix arguments (e.g. `C-u`) are visible during:

  * interactive argument resolution
  * command execution
  * pre-command hooks
  * post-command hooks
* Prefix arguments are cleared **only after** post-command hooks complete.

---

## 3. Dynamic Scoping Commitment

Dynamic scoping is a **core design invariant** of Lexicon.

### 3.1 Dynamic Variables

Lexicon guarantees dynamic binding semantics equivalent to Emacs for editor state, including but not limited to:

* `current-buffer`
* `point`
* `mark`
* `this-command`
* `last-command`
* minibuffer-related dynamic variables

### 3.2 Scope Visibility

* Commands, hooks, and interactive evaluation observe the same dynamic bindings.
* Dynamic bindings are visible to all code executing within the same command invocation.

### 3.3 Dynamic Scope Isolation

* Dynamic bindings must not leak outside a command invocation.
* Nested commands (e.g. minibuffer recursion) receive isolated dynamic environments.
* Concurrent editor operations must not observe each other’s dynamic bindings.

### 3.4 Runtime Evaluation

Runtime evaluation performed within the editor must respect all active dynamic bindings and command context.

---

## 4. Core Abstractions Guaranteed

The following abstractions are guaranteed to exist with the specified semantics.

### 4.1 Buffers

* Buffers are mutable text containers with identity.
* Buffers support buffer-local variables.
* Buffer-local bindings override global bindings dynamically when the buffer is current.

### 4.2 Point and Mark

* Each buffer maintains a point and an optional mark.
* Motion commands update point and mark consistently with Emacs semantics.
* Region semantics (`point`–`mark`) are preserved.

### 4.3 Markers

* Markers represent semantic positions within a buffer.
* Markers move in response to text insertion and deletion in a manner consistent with Emacs.
* Markers are not simple numeric positions.
* Lexicon does not guarantee binary compatibility with Emacs marker APIs.

### 4.4 Commands

* Commands are first-class callable editor actions.
* Commands may be invoked interactively or programmatically.
* Interactive commands participate fully in the command lifecycle and hook system.

### 4.5 Hooks

* Hooks are ordered lists of functions.
* Hooks may be global or buffer-local.
* Hooks execute under the same dynamic bindings as the associated command.

#### Hook Error Semantics

* Errors thrown by hooks propagate after all remaining hooks of the same phase run.
* Post-command hooks run even if pre-command hooks or command bodies signal errors.

### 4.6 Kill Ring

* Killed text is added to the kill ring.
* Successive adjacent kills within a command append into a single kill-ring entry.
* Yank retrieves the most recent kill-ring entry.
* Yank-pop cycles through the kill ring only if the previous command was a yank.

### 4.7 Minibuffer

The minibuffer is a **control plane**, not merely a UI component.

Lexicon guarantees that:

* The minibuffer supports recursive activation.
* Each minibuffer recursion level has isolated dynamic bindings.
* Nested minibuffer invocations are supported.
* `C-g` aborts only the current minibuffer level.

### 4.8 Completion and `completing-read`

* Completion is defined as a protocol, not a UI.
* Candidate generation, narrowing, and selection semantics are preserved.
* Multiple completion UIs may exist, but all must obey the same semantic contract.

### 4.9 Interactive Argument Specifications

Lexicon guarantees support for the following interactive specifiers:

* `"p"` — numeric prefix argument
* `"P"` — raw prefix argument
* `"r"` — region (point and mark)
* `"d"` — point
* `"s"` — string (minibuffer input)

Additional specifiers may be added only by explicit amendment.

---

## 5. Explicit Non-Goals

Lexicon does **not** guarantee:

* Frame or window geometry parity
* Redisplay timing equivalence
* Emacs Lisp bytecode compatibility
* Native process or terminal semantics
* Binary compatibility with Emacs packages

Any divergence from Emacs behavior not listed in this section must be documented explicitly.

---

## 6. Compatibility Measurement

Emacs compatibility is measured **only** by semantic tests.

* Tests are derived from the Emacs test suite.
* Tests assert observable editor behavior, not implementation details.
* Passing semantic tests is the primary measure of correctness.
* Known divergences must be documented and justified.

---

## 7. Change Control

This contract is expected to evolve, but:

* Changes must be explicit.
* Changes must be reviewed.
* Changes must be justified in terms of semantic impact.

Silent drift is not permitted.

---

## Closing Statement

This document defines the **semantic foundation** upon which Lexicon is built.
All higher-level features, packages, and extensions depend on the guarantees made here.

**This contract is the editor’s constitution.**

