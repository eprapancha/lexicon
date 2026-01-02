# Lexicon Execution Model Specification

**Version:** 1.0.0
**Status:** Design Document
**Last Updated:** 2026-01-02

## Table of Contents

1. [Overview](#overview)
2. [Dynamic Execution Context](#dynamic-execution-context)
3. [Command Invocation](#command-invocation)
4. [Interactive Specifications](#interactive-specifications)
5. [Prefix Arguments](#prefix-arguments)
6. [Keyboard Macros](#keyboard-macros)
7. [Recursive Editing](#recursive-editing)
8. [Execution Context API](#execution-context-api)
9. [Implementation Guidelines](#implementation-guidelines)

---

## Overview

Lexicon's execution model defines **how commands execute** and **what context they receive**. The model follows Emacs conventions while adapting to ClojureScript's functional paradigm.

### Key Concepts

1. **Dynamic Context**: Commands execute with dynamic bindings (like Emacs' dynamic scope)
2. **Command Loop**: Central loop handling input → command → hooks → undo
3. **Interactive Specifications**: Declarative argument collection
4. **Prefix Arguments**: Universal argument system (`C-u`)
5. **Keyboard Macros**: Recording and replaying command sequences
6. **Recursive Editing**: Nested command loops (minibuffer)

### Design Principles

1. **Emacs-compatible**: Match Emacs execution semantics
2. **Functional**: Use ClojureScript idioms (dynamic vars, not global state)
3. **Composable**: Commands can call other commands
4. **Debuggable**: Execution context visible for troubleshooting
5. **Testable**: Mock context for unit tests

### Comparison to Emacs

| Feature | Emacs | Lexicon |
|---------|-------|---------|
| Command loop | C main loop | ClojureScript event loop |
| Dynamic scope | `let` + `defvar` | `binding` + dynamic vars |
| Interactive spec | `(interactive "...")` | `:interactive` metadata |
| Prefix arg | `C-u` | `C-u` ✅ |
| `this-command` | Dynamic var | Dynamic var ✅ |
| Keyboard macros | Built-in | Planned |
| Recursive edit | `recursive-edit` | Planned |

---

## Dynamic Execution Context

### Context Variables

During command execution, several **dynamic variables** are bound:

```clojure
;; Core dynamic context (always bound)
(def ^:dynamic *current-command* nil)      ;; Currently executing command ID
(def ^:dynamic *current-buffer* nil)       ;; Active buffer ID
(def ^:dynamic *prefix-arg* nil)           ;; Universal argument value
(def ^:dynamic *this-command-keys* nil)    ;; Key sequence that invoked command
(def ^:dynamic *last-command* nil)         ;; Previous command ID
(def ^:dynamic *real-last-command* nil)    ;; Last non-internal command
(def ^:dynamic *current-window* nil)       ;; Active window ID
```

### Context Lifetime

Context variables are bound **per command invocation**:

```clojure
(defn execute-command [command-id args]
  (binding [*current-command* command-id
            *current-buffer* (get-current-buffer)
            *this-command-keys* (get-key-sequence)
            *last-command* @previous-command
            *prefix-arg* @prefix-arg-value]

    ;; Execute command with context
    (try
      (apply-command command-id args)

      (finally
        ;; Save for next invocation
        (reset! previous-command command-id)
        (reset! prefix-arg-value nil)))))
```

### Accessing Context

Commands access context via dynamic vars:

```clojure
(defn my-command []
  (println "Executing:" *current-command*)
  (println "In buffer:" *current-buffer*)
  (println "Prefix arg:" *prefix-arg*)
  (println "Keys pressed:" *this-command-keys*))
```

### Context in Nested Calls

When commands call other commands, context is **preserved**:

```clojure
(defn outer-command []
  (println "Outer:" *current-command*)  ;; => :outer-command
  (inner-command))

(defn inner-command []
  (println "Inner:" *current-command*)  ;; => :outer-command (same context!)
  )

;; To create NEW context for inner command:
(defn outer-command-with-new-context []
  (call-interactively :inner-command))  ;; Creates fresh context
```

---

## Command Invocation

### Command Lifecycle

```
User Input (key press)
         ↓
    Keymap Lookup (find command)
         ↓
    Prefix Argument Handling
         ↓
    :before-command-hook
         ↓
    Interactive Argument Collection
         ↓
    Command Execution (with dynamic context)
         ↓
    :after-command-hook
         ↓
    Undo Boundary Insertion
         ↓
    UI Update
```

### Invocation Methods

#### 1. `execute-command` (Programmatic)

Direct command invocation with explicit arguments:

```clojure
(execute-command :self-insert-command \a)
(execute-command :forward-char 5)
(execute-command :save-buffer)
```

**Signature**:
```clojure
(execute-command command-id & args)
```

**Behavior**:
- No interactive argument collection
- No prefix argument handling
- Runs hooks
- Creates undo boundary

---

#### 2. `call-interactively` (Interactive)

Invokes command as if user pressed keys:

```clojure
(call-interactively :query-replace)
;; Prompts for arguments in minibuffer
```

**Signature**:
```clojure
(call-interactively command-id)
```

**Behavior**:
- Collects arguments via `:interactive` spec
- Honors prefix argument
- Runs hooks
- Creates undo boundary

---

#### 3. Keymap Dispatch (User Input)

Normal user interaction:

```
User presses: C-x C-f
         ↓
Keymap lookup: :find-file
         ↓
call-interactively :find-file
         ↓
Interactive spec: "FFind file: "
         ↓
Prompt in minibuffer
         ↓
Execute with filename argument
```

---

### Command Metadata

Commands are registered with metadata:

```clojure
(define-command :forward-char
  {:interactive true
   :interactive-spec "p"  ;; Prefix as number
   :doc "Move point forward N characters"
   :group :editing}

  (fn [n]
    (set-point (+ (point) n))))
```

**Metadata Fields**:

| Field | Type | Description |
|-------|------|-------------|
| `:interactive` | bool | Can be called interactively? |
| `:interactive-spec` | string | Argument collection spec |
| `:doc` | string | Documentation |
| `:group` | keyword | Command category |
| `:repeatable?` | bool | Can repeat with `.` (future) |

---

## Interactive Specifications

### Purpose

Interactive specs define **how to collect arguments** from the user.

### Spec Format

Spec is a **string** with **code characters**:

```clojure
:interactive-spec "p"       ;; Prefix argument as number
:interactive-spec "P"       ;; Prefix argument as raw value
:interactive-spec "r"       ;; Region start and end
:interactive-spec "b"       ;; Buffer name (with completion)
:interactive-spec "f"       ;; File name
:interactive-spec "s"       ;; String from minibuffer
```

### Code Characters

| Code | Meaning | Example |
|------|---------|---------|
| `p` | Prefix arg as number (default 1) | `"p"` → `(forward-char 5)` after `C-u 5` |
| `P` | Prefix arg as raw value | `"P"` → `(universal-argument 4)` after `C-u` |
| `r` | Region start and end positions | `"r"` → `(delete-region 10 50)` |
| `b` | Buffer name (with completion) | `"bSwitch to buffer: "` |
| `B` | Buffer name (may not exist) | `"BKill buffer: "` |
| `f` | File name (existing) | `"fFind file: "` |
| `F` | File name (may not exist) | `"FSave as: "` |
| `s` | String from minibuffer | `"sSearch: "` |
| `n` | Number from minibuffer | `"nRepeat count: "` |
| `c` | Single character | `"cInsert character: "` |
| `k` | Key sequence | `"kDescribe key: "` |
| `x` | Lisp expression | `"xEvaluate: "` |

### Multi-argument Specs

Concatenate codes with prompts:

```clojure
:interactive-spec "sSearch for: \nsReplace with: "
;; Collects two strings:
;; 1. "Search for: " → arg1
;; 2. "Replace with: " → arg2

;; Command receives: (query-replace arg1 arg2)
```

**Newline** (`\n`) separates prompts.

### Example Interactive Specs

#### `forward-char`

```clojure
{:interactive-spec "p"}
;; Prefix arg as number (default 1)
;; C-u 5 M-x forward-char → (forward-char 5)
```

#### `query-replace`

```clojure
{:interactive-spec "sQuery replace: \nsQuery replace %s with: "}
;; Prompt for search string
;; Prompt for replacement (showing search string in %s)
;; Command receives: (query-replace "foo" "bar")
```

#### `delete-region`

```clojure
{:interactive-spec "r"}
;; Region start and end
;; If region active: (delete-region 10 50)
;; If region inactive: error "No region active"
```

#### `kill-buffer`

```clojure
{:interactive-spec "bKill buffer: "}
;; Prompt for buffer name with completion
;; Default: current buffer
;; Command receives: (kill-buffer "buffer-name")
```

---

## Prefix Arguments

### Universal Argument (`C-u`)

Emacs-style prefix argument system:

```
C-u         → prefix-arg = 4
C-u C-u     → prefix-arg = 16
C-u C-u C-u → prefix-arg = 64
C-u 5       → prefix-arg = 5
C-u - 3     → prefix-arg = -3
```

### Prefix Argument States

```clojure
;; No prefix
*prefix-arg* = nil

;; C-u (once)
*prefix-arg* = 4

;; C-u C-u
*prefix-arg* = 16

;; C-u 5
*prefix-arg* = 5

;; C-u -
*prefix-arg* = -1

;; C-u - 3
*prefix-arg* = -3
```

### Using Prefix Arguments

#### In Interactive Spec

```clojure
(define-command :forward-char
  {:interactive-spec "p"}  ;; "p" = prefix as number, default 1

  (fn [n]
    (set-point (+ (point) n))))

;; Usage:
;; M-x forward-char        → n = 1
;; C-u M-x forward-char    → n = 4
;; C-u 5 M-x forward-char  → n = 5
```

#### In Command Body

```clojure
(define-command :insert-greeting
  {:interactive true}

  (fn []
    (let [count (or *prefix-arg* 1)]
      (dotimes [i count]
        (insert! "Hello\n")))))

;; Usage:
;; M-x insert-greeting     → Insert "Hello" once
;; C-u 3 M-x insert-greeting → Insert "Hello" 3 times
```

### Prefix Argument Behavior

| Spec | No prefix | `C-u` | `C-u 5` |
|------|-----------|-------|---------|
| `p` | 1 | 4 | 5 |
| `P` | `nil` | `[4]` | `5` |

**`p` vs `P`**:
- `p`: Always a number (default 1)
- `P`: Raw value (nil, [4], [16], or number)

### Resetting Prefix Argument

Prefix arg is **consumed** after command:

```clojure
;; User presses: C-u 5 M-x forward-char
*prefix-arg* = 5  ;; During forward-char execution

;; After forward-char completes
*prefix-arg* = nil  ;; Reset for next command
```

---

## Keyboard Macros

### Concept

**Keyboard macros** record and replay sequences of commands:

```
Start recording: C-x (
Execute commands: forward-word, capitalize-word
Stop recording: C-x )
Replay macro: C-x e
```

### Macro Recording

While recording:
1. All commands are logged
2. Arguments captured
3. Stored as command sequence

```clojure
{:macro-recording? true
 :macro-commands [{:command :forward-word :args [1]}
                  {:command :capitalize-word :args []}
                  {:command :forward-word :args [1]}]}
```

### Macro Playback

Replay by executing commands in sequence:

```clojure
(defn execute-macro [macro]
  (doseq [{:keys [command args]} (:commands macro)]
    (apply execute-command command args)))
```

### Nested Macros

Macros can **call other macros** (recursive):

```clojure
;; Macro 1: capitalize-word + forward-word
;; Macro 2: execute-macro-1 10 times
```

### Macro API (Future)

```clojure
(start-kbd-macro)           ;; C-x (
(end-kbd-macro)             ;; C-x )
(call-last-kbd-macro)       ;; C-x e
(call-last-kbd-macro n)     ;; C-u 5 C-x e (repeat 5 times)
(name-last-kbd-macro 'name) ;; Save macro with name
```

---

## Recursive Editing

### Concept

**Recursive editing** creates a **nested command loop** (e.g., minibuffer):

```
Main command loop
    ↓
User presses: M-x query-replace
    ↓
Minibuffer opens (recursive edit level 1)
    ↓
User presses: C-r (isearch in minibuffer)
    ↓
Isearch buffer opens (recursive edit level 2)
    ↓
User presses: C-g (abort isearch)
    ↓
Back to minibuffer (level 1)
    ↓
User enters replacement string
    ↓
Back to main buffer (level 0)
```

### Recursive Edit Levels

Track recursion depth:

```clojure
(def ^:dynamic *recursive-edit-level* 0)

(defn recursive-edit []
  (binding [*recursive-edit-level* (inc *recursive-edit-level*)]
    (command-loop)))  ;; Nested loop
```

### Aborting Recursive Edit

`C-g` (keyboard-quit) exits current level:

```clojure
(defn keyboard-quit []
  (if (> *recursive-edit-level* 0)
    (throw (ex-info "Quit" {:type :quit :level *recursive-edit-level*}))
    (message "Quit")))
```

### Use Cases

1. **Minibuffer**: User input during command
2. **Query-replace**: Confirm each replacement
3. **Ediff**: Merge conflicts interactively
4. **Debugger**: Inspect state mid-command

---

## Execution Context API

### `current-buffer`

Returns ID of current buffer.

**Signature**:
```clojure
(current-buffer)
```

**Returns**: Buffer ID (string)

**Example**:
```clojure
(defn my-command []
  (let [buf (current-buffer)]
    (insert! buf "Hello")))
```

---

### `set-buffer`

Temporarily switches current buffer (for code scope only).

**Signature**:
```clojure
(set-buffer buffer-id)
```

**Use Case**: Execute operations in different buffer

**Example**:
```clojure
(defn append-to-messages []
  (let [original-buffer (current-buffer)]
    (set-buffer "*Messages*")
    (goto-char (point-max))
    (insert! "Log message\n")
    (set-buffer original-buffer)))  ;; Restore
```

**Better alternative**: Use `with-current-buffer`

---

### `with-current-buffer`

Executes body with temporary buffer switch.

**Signature**:
```clojure
(with-current-buffer buffer-id & body)
```

**Example**:
```clojure
(with-current-buffer "*Messages*"
  (goto-char (point-max))
  (insert! "Log message\n"))
;; Automatically restores previous buffer
```

---

### `save-excursion`

Executes body and restores point and mark.

**Signature**:
```clojure
(save-excursion & body)
```

**Example**:
```clojure
(defn count-lines-in-buffer []
  (save-excursion
    (goto-char (point-min))
    (let [count (count-matches "\n")]
      count)))
;; Point restored to original position
```

---

### `save-restriction`

Executes body and restores narrowing (future feature).

**Signature**:
```clojure
(save-restriction & body)
```

**Use Case**: Temporarily narrow to region, then restore

---

### `this-command-keys`

Returns key sequence that invoked current command.

**Signature**:
```clojure
(this-command-keys)
```

**Returns**: Vector of key events

**Example**:
```clojure
(defn describe-key-briefly []
  (let [keys (this-command-keys)]
    (message "%s runs %s" keys *current-command*)))

;; User presses: C-x C-f
;; Message: "[Ctrl+x Ctrl+f] runs :find-file"
```

---

### `prefix-numeric-value`

Converts prefix argument to number.

**Signature**:
```clojure
(prefix-numeric-value prefix-arg)
```

**Returns**: Number (default 1)

**Example**:
```clojure
(prefix-numeric-value nil)   ;; => 1
(prefix-numeric-value 4)     ;; => 4
(prefix-numeric-value [4])   ;; => 4
(prefix-numeric-value -3)    ;; => -3
```

---

## Implementation Guidelines

### For Core Developers

#### 1. Command Loop Structure

```clojure
(defn command-loop []
  (loop []
    (try
      ;; Get input
      (let [key-event (read-key-event)
            command (lookup-key key-event)]

        ;; Execute command
        (when command
          (execute-command-with-hooks command))

        ;; Recur
        (recur))

      (catch :quit e
        ;; User pressed C-g
        (message "Quit")
        (recur)))))
```

#### 2. Command Execution with Context

```clojure
(defn execute-command-with-hooks [command-id]
  (binding [*current-command* command-id
            *current-buffer* (get-current-buffer-id)
            *prefix-arg* @prefix-arg-state
            *this-command-keys* @key-sequence
            *last-command* @previous-command]

    ;; Before hook
    (run-hooks :before-command-hook
      {:command-id command-id
       :buffer-id *current-buffer*
       :prefix-arg *prefix-arg*})

    ;; Execute
    (try
      (let [args (collect-interactive-args command-id)
            result (apply-command command-id args)]

        ;; After hook
        (run-hooks :after-command-hook
          {:command-id command-id
           :result result
           :buffer-id *current-buffer*})

        result)

      (finally
        ;; Cleanup
        (reset! previous-command command-id)
        (reset! prefix-arg-state nil)
        (undo-boundary)))))
```

#### 3. Interactive Argument Collection

```clojure
(defn collect-interactive-args [command-id]
  (let [spec (get-interactive-spec command-id)]
    (when spec
      (parse-interactive-spec spec *prefix-arg*))))

(defn parse-interactive-spec [spec prefix-arg]
  (case spec
    "p" [(or prefix-arg 1)]
    "P" [prefix-arg]
    "r" (if (region-active?)
          [(region-beginning) (region-end)]
          (throw (ex-info "No region active" {})))
    ;; ... more cases
    ))
```

---

### For Package Developers

#### 1. Using Dynamic Context

```clojure
(defn my-command []
  ;; Access context
  (println "Command:" *current-command*)
  (println "Buffer:" *current-buffer*)
  (println "Prefix:" *prefix-arg*)

  ;; Use Core API (automatically uses context)
  (insert! "Hello"))
```

#### 2. Calling Other Commands

```clojure
(defn my-macro-command []
  ;; Execute sub-commands
  (execute-command :forward-word)
  (execute-command :capitalize-word)
  (execute-command :forward-word))
```

#### 3. Interactive Commands

```clojure
(define-command :insert-n-times
  {:interactive-spec "sText: \nnCount: "}

  (fn [text count]
    (dotimes [i count]
      (insert! text))))

;; Usage: M-x insert-n-times
;; Prompt 1: "Text: " → "Hello"
;; Prompt 2: "Count: " → 5
;; Inserts "Hello" 5 times
```

#### 4. Prefix Argument Patterns

```clojure
;; Pattern 1: Repeat command N times
(define-command :insert-char
  {:interactive-spec "cInsert character: \np"}

  (fn [char count]
    (dotimes [i count]
      (insert! (str char)))))

;; Pattern 2: Different behavior based on prefix
(define-command :delete-thing
  {:interactive true}

  (fn []
    (cond
      (nil? *prefix-arg*)      (delete-char)      ;; No prefix: delete char
      (= *prefix-arg* 4)       (delete-word)      ;; C-u: delete word
      (= *prefix-arg* 16)      (delete-line)      ;; C-u C-u: delete line
      :else                    (delete-char *prefix-arg*))))  ;; C-u N: delete N chars
```

---

## Migration from Current Implementation

### Current State (Phase 6)

Lexicon currently has:
- Basic command execution
- No formal context variables
- No interactive specs (hardcoded argument collection)
- No prefix argument support
- No keyboard macros

### Migration Path (Phase 7.6)

**Week 1: Dynamic Context**
1. Define dynamic vars (`*current-command*`, etc.)
2. Update command execution to bind context
3. Expose context via Core API
4. Update existing commands to use context

**Week 2: Interactive Specs**
1. Design interactive spec parser
2. Implement code character handlers
3. Add minibuffer argument collection
4. Update command metadata

**Week 3: Prefix Arguments**
1. Implement `C-u` key handling
2. Add prefix argument state tracking
3. Update interactive specs to honor prefix
4. Add numeric argument display in mode-line

**Week 4: Testing**
1. Unit tests for context binding
2. Integration tests for interactive specs
3. Prefix argument tests
4. Command invocation tests

---

## Future Enhancements

### 1. Keyboard Macros (Phase 8)

Full macro support:
- Recording: `C-x (`
- Playback: `C-x e`
- Named macros
- Macro editing

### 2. Recursive Editing

Nested command loops:
- Minibuffer as recursive edit
- `C-]` to abort
- Mode-line indicator: `[1]`, `[2]`

### 3. Advice System

Wrap commands with advice (before/after/around):
```clojure
(add-advice :forward-char :before log-command)
```

### 4. Command History

Track command invocation history:
```clojure
(command-history)  ;; Last 100 commands
(repeat-complex-command)  ;; M-x (redo last command)
```

---

## References

1. **Emacs Lisp Manual**: [Command Loop](https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Loop.html)
2. **Emacs Lisp Manual**: [Defining Commands](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Commands.html)
3. **Emacs Lisp Manual**: [Interactive Codes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html)
4. **Lexicon Core API**: [docs/core/core-api.md](./core-api.md)
5. **Lexicon Hook System**: [docs/core/hooks.md](./hooks.md)

---

## Appendix A: Interactive Spec Examples

### Navigation Commands

```clojure
;; forward-char
{:interactive-spec "p"}  ;; Prefix as number
;; C-u 5 M-x forward-char → (forward-char 5)

;; goto-line
{:interactive-spec "nGoto line: "}  ;; Number from minibuffer
;; M-x goto-line → "Goto line: " → 42 → (goto-line 42)

;; goto-char
{:interactive-spec "nGoto char: "}
;; M-x goto-char → "Goto char: " → 100 → (goto-char 100)
```

### Editing Commands

```clojure
;; delete-region
{:interactive-spec "r"}  ;; Region start and end
;; M-x delete-region (with active region 10-50) → (delete-region 10 50)

;; kill-region
{:interactive-spec "r"}
;; Same as delete-region

;; upcase-region
{:interactive-spec "r"}
;; Same as delete-region
```

### File Commands

```clojure
;; find-file
{:interactive-spec "FFind file: "}  ;; File name (may not exist)
;; M-x find-file → "Find file: " → "/path/to/file.txt" → (find-file "/path/to/file.txt")

;; save-buffer
{:interactive true}  ;; No arguments
;; M-x save-buffer → (save-buffer)

;; write-file
{:interactive-spec "FWrite file: "}  ;; New file name
;; M-x write-file → "Write file: " → "/new/path.txt" → (write-file "/new/path.txt")
```

### Buffer Commands

```clojure
;; switch-to-buffer
{:interactive-spec "bSwitch to buffer: "}  ;; Buffer name with completion
;; M-x switch-to-buffer → "Switch to buffer: " → "*scratch*" → (switch-to-buffer "*scratch*")

;; kill-buffer
{:interactive-spec "bKill buffer: "}
;; M-x kill-buffer → "Kill buffer: " → "foo.txt" → (kill-buffer "foo.txt")
```

### Search Commands

```clojure
;; isearch-forward
{:interactive-spec "P"}  ;; Prefix as raw (for regex toggle)
;; M-x isearch-forward → (isearch-forward nil)
;; C-u M-x isearch-forward → (isearch-forward [4])

;; query-replace
{:interactive-spec "sQuery replace: \nsQuery replace %s with: "}
;; M-x query-replace
;; → "Query replace: " → "foo"
;; → "Query replace foo with: " → "bar"
;; → (query-replace "foo" "bar")
```

---

## Appendix B: Dynamic Context Example

### Full Example: Command with Context

```clojure
(define-command :describe-context
  {:interactive true
   :doc "Show current execution context"}

  (fn []
    (let [msg (str "Context:\n"
                   "  Command: " *current-command* "\n"
                   "  Buffer: " *current-buffer* "\n"
                   "  Prefix: " *prefix-arg* "\n"
                   "  Keys: " *this-command-keys* "\n"
                   "  Last: " *last-command* "\n"
                   "  Level: " *recursive-edit-level*)]
      (message msg))))

;; Example output after C-u 5 M-x describe-context:
;; Context:
;;   Command: :describe-context
;;   Buffer: buffer-123
;;   Prefix: 5
;;   Keys: [Meta+x "describe-context" Return]
;;   Last: :forward-char
;;   Level: 0
```

---

**End of Execution Model Specification**
