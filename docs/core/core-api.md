# Lexicon Core API Contract

**Version:** 1.0.0
**Status:** Draft
**Last Updated:** 2026-01-02

---

## Purpose

This document defines the **Core API** - the **only supported interface** between:
- Lexicon editor core
- External packages (Vertico, Orderless, Consult, Evil-mode)
- Future tooling (macros, LSP integration, scripting)

**Anything not in this API is explicitly unstable and internal.**

Packages that depend on Core API internals **will break**. Packages that use only this API **will remain stable**.

---

## Design Principles

### 1. Editor-First, UI-Agnostic
Core APIs are usable without DOM, re-frame, or browser context. A package should be testable in a REPL without loading the UI.

### 2. Semantic, Not Mechanical
APIs express **editor concepts** (buffer, command, mode), not implementation details (engine calls, atoms, re-frame events).

**Good:** `(insert-text! "hello")`
**Bad:** `(dispatch [:edit/insert {:text "hello"}])`

### 3. Explicit Side Effects
Effectful operations are clearly marked (with `!` suffix) and centrally mediated. Pure functions have no side effects.

### 4. Stable Naming
Once published, API names are not casually changed. Breaking changes require major version bump and migration path.

### 5. Emacs Alignment
When in doubt, **do what Emacs does**. Core API mirrors Emacs Lisp functions where applicable.

---

## Core Domains

The Core API is organized into five domains:

1. **Buffers** - Text storage and buffer lifecycle
2. **Commands** - Named, invokable editor actions
3. **Keymaps** - Key sequence → command bindings
4. **Modes** - Major/minor mode system
5. **Hooks** - Event interception and extension points

---

## 1. Buffer API

### Concepts

A **buffer** is the primary unit of editable state in Lexicon. Buffers:
- Have unique identity (buffer ID)
- Contain editable text
- Have markers (point, mark, overlays)
- Have buffer-local variables
- Exist independently of windows (multiple windows can show same buffer)

### Buffer Lifecycle

#### `(create-buffer name)`
Creates a new buffer with given name.

**Parameters:**
- `name` (string) - Buffer name (e.g., "*scratch*", "file.txt")

**Returns:** Buffer ID (keyword or string)

**Guarantees:**
- Buffer names are unique (creating duplicate appends `<2>`, `<3>`, etc.)
- New buffers start in `fundamental-mode`
- New buffers have empty content
- New buffers have point at position 0

**Example:**
```clojure
(def buf (create-buffer "my-buffer.txt"))
;; => :buffer/uuid-1234
```

---

#### `(get-buffer buffer-id)`
Retrieves buffer by ID.

**Parameters:**
- `buffer-id` - Buffer identifier

**Returns:** Buffer object (map with metadata) or `nil` if not found

**Buffer object structure:**
```clojure
{:id :buffer/uuid-1234
 :name "my-buffer.txt"
 :major-mode :fundamental-mode
 :minor-modes #{:line-number-mode}
 :modified? false
 :read-only? false
 :file-path nil  ; or "/path/to/file.txt"
 :engine-id 42}  ; internal - do not depend on
```

**Guarantees:**
- Returns same buffer object for same ID
- Buffer object is immutable snapshot (does not update)

---

#### `(current-buffer)`
Returns the active buffer ID.

**Returns:** Buffer ID of currently active buffer

**Guarantees:**
- There is always a current buffer (never returns `nil`)
- Newly created buffers become current buffer

**Example:**
```clojure
(current-buffer)  ;; => :buffer/uuid-1234
```

---

#### `(switch-to-buffer buffer-id)`
Switches to specified buffer (makes it current).

**Parameters:**
- `buffer-id` - Buffer to switch to

**Side effects:**
- Changes current buffer
- Fires `buffer-switch` hook
- Updates window to show buffer (if in window context)

**Guarantees:**
- After call, `(current-buffer)` returns `buffer-id`
- Previous buffer remains in buffer list

---

#### `(kill-buffer buffer-id)`
Destroys buffer and frees resources.

**Parameters:**
- `buffer-id` - Buffer to destroy

**Side effects:**
- Removes buffer from buffer list
- Fires `kill-buffer-hook`
- If buffer was current, switches to another buffer
- Cleans up engine resources

**Guarantees:**
- After call, `(get-buffer buffer-id)` returns `nil`
- Cannot kill the last buffer (creates `*scratch*` if needed)

---

#### `(list-buffers)`
Returns list of all live buffers.

**Returns:** Sequence of buffer IDs

**Example:**
```clojure
(list-buffers)  ;; => (:buffer/scratch :buffer/file1 :buffer/file2)
```

---

### Buffer Content Operations

#### `(point buffer-id)`
Returns cursor position in buffer.

**Parameters:**
- `buffer-id` - Buffer to query (defaults to current buffer if omitted)

**Returns:** Integer position (0-indexed, byte offset)

**Guarantees:**
- `0 <= point <= (buffer-size buffer-id)`
- Point is always valid (never points beyond buffer end)

**Example:**
```clojure
(point)  ;; => 42 (current buffer)
(point :buffer/other)  ;; => 100 (specific buffer)
```

---

#### `(set-point! buffer-id position)`
Moves cursor to position.

**Parameters:**
- `buffer-id` - Buffer to modify
- `position` - New cursor position (integer)

**Side effects:**
- Updates point marker
- Fires `before-change` and `after-change` hooks (if position changes)

**Guarantees:**
- Position is clamped to valid range `[0, buffer-size]`
- Invalid positions do not crash (clamped to nearest valid position)

**Example:**
```clojure
(set-point! (current-buffer) 100)
```

---

#### `(buffer-text buffer-id &optional start end)`
Returns text from buffer.

**Parameters:**
- `buffer-id` - Buffer to read from
- `start` (optional) - Start position (default: 0)
- `end` (optional) - End position (default: buffer-size)

**Returns:** String containing text in range `[start, end)`

**Guarantees:**
- Read-only operation (no side effects)
- Returns UTF-8 string
- Positions clamped to valid range

**Example:**
```clojure
(buffer-text (current-buffer))  ;; entire buffer
(buffer-text (current-buffer) 10 20)  ;; 10 chars starting at position 10
```

---

#### `(buffer-size buffer-id)`
Returns buffer size in bytes.

**Parameters:**
- `buffer-id` - Buffer to measure

**Returns:** Integer (number of bytes in buffer)

**Guarantees:**
- Always >= 0
- Empty buffer returns 0

---

#### `(insert! text &optional position)`
Inserts text at position (or point).

**Parameters:**
- `text` - String to insert
- `position` (optional) - Where to insert (default: current point)

**Side effects:**
- Modifies buffer content
- Updates point (moves after inserted text)
- Updates all markers after insertion point
- Marks buffer as modified
- Fires `before-change` and `after-change` hooks
- Records undo information

**Guarantees:**
- Inserted text is UTF-8
- Point moves to end of inserted text
- Markers after insertion point shift right
- Buffer size increases by `(count text)`

**Example:**
```clojure
(insert! "hello")  ;; insert at point
(insert! "world" 100)  ;; insert at specific position
```

---

#### `(delete-region! start end)`
Deletes text in range.

**Parameters:**
- `start` - Start position (inclusive)
- `end` - End position (exclusive)

**Side effects:**
- Modifies buffer content
- Updates point (if in deleted range, moves to start)
- Updates all markers in/after deleted range
- Marks buffer as modified
- Fires `before-change` and `after-change` hooks
- Records undo information
- Deleted text added to kill ring

**Guarantees:**
- `start <= end` (swapped if reversed)
- Positions clamped to valid range
- Buffer size decreases by `(- end start)`
- Markers in deleted range are deleted or moved to `start`

**Example:**
```clojure
(delete-region! 10 20)  ;; delete 10 chars starting at position 10
```

---

### Buffer State

#### `(buffer-modified? buffer-id)`
Returns whether buffer has unsaved changes.

**Returns:** Boolean

**Guarantees:**
- `true` after any edit operation
- `false` after `(set-buffer-unmodified! buffer-id)`
- `false` for new buffers

---

#### `(set-buffer-modified! buffer-id modified?)`
Marks buffer as modified or unmodified.

**Parameters:**
- `buffer-id` - Buffer to update
- `modified?` - Boolean

**Side effects:**
- Updates modified flag
- Updates mode line indicator

**Use case:** Called after saving file to mark buffer as unmodified.

---

#### `(buffer-read-only? buffer-id)`
Returns whether buffer is read-only.

**Returns:** Boolean

**Guarantees:**
- Read-only buffers reject edit operations (`insert!`, `delete-region!`)
- Special buffers (`*Help*`, `*Messages*`) are read-only

---

#### `(set-buffer-read-only! buffer-id read-only?)`
Marks buffer as read-only or writable.

**Parameters:**
- `buffer-id` - Buffer to update
- `read-only?` - Boolean

**Side effects:**
- Updates read-only flag
- Fires `change-major-mode-hook` (if mode enforces read-only)

---

#### `(buffer-file-name buffer-id)`
Returns file path associated with buffer.

**Returns:** String (file path) or `nil` if buffer not visiting file

**Example:**
```clojure
(buffer-file-name (current-buffer))  ;; => "/home/user/file.txt"
```

---

#### `(set-buffer-file-name! buffer-id path)`
Associates buffer with file path.

**Parameters:**
- `buffer-id` - Buffer to update
- `path` - File path (string) or `nil`

**Side effects:**
- Updates file association
- Updates buffer name (if not explicitly set)

---

## 2. Command API

### Concepts

A **command** is a named, invokable editor action. Commands:
- Have unique identity (command keyword)
- Are callable interactively (via `M-x` or keybinding)
- May have metadata (docstring, interactive spec, undo behavior)
- Execute in command context (with dynamic bindings)

### Command Registration

#### `(define-command command-id metadata fn)`
Registers a command.

**Parameters:**
- `command-id` (keyword) - Unique command identifier (e.g., `:forward-char`)
- `metadata` (map) - Command metadata
  - `:interactive` (boolean) - Can be called via `M-x`?
  - `:doc` (string) - Documentation string
  - `:repeatable` (boolean) - Can be repeated with `.` (Evil) or `C-x z`?
  - `:undo` (keyword) - Undo behavior (`:group`, `:individual`, `:none`)
- `fn` (function) - Command implementation (0-arity or takes args)

**Side effects:**
- Registers command in global command registry
- Command becomes available to `M-x` (if interactive)
- Command can be bound to keybindings

**Guarantees:**
- Redefining command overwrites previous definition
- Commands persist until unregistered

**Example:**
```clojure
(define-command
  :my-package/hello
  {:interactive true
   :doc "Print hello message"
   :repeatable false
   :undo :none}
  (fn []
    (message "Hello from my package!")))
```

---

#### `(unregister-command! command-id)`
Removes command from registry.

**Parameters:**
- `command-id` - Command to remove

**Side effects:**
- Command no longer callable via `M-x`
- Keybindings to command become unbound

**Use case:** Package cleanup on unload

---

#### `(execute-command command-id & args)`
Executes a command programmatically.

**Parameters:**
- `command-id` - Command to execute
- `args` - Arguments to pass to command function

**Side effects:**
- Runs command in command execution context
- Fires `before-command` and `after-command` hooks
- Records undo boundary (if command modifies buffer)
- Updates `last-command` state

**Guarantees:**
- Command executes with dynamic bindings (`*current-command*`, etc.)
- Errors in command are caught and reported (do not crash editor)
- Hooks can cancel command execution (by returning `nil`)

**Example:**
```clojure
(execute-command :forward-char)
(execute-command :insert {:text "hello"})
```

---

#### `(call-interactively command-id)`
Executes command as if called by user (reads args from minibuffer if needed).

**Parameters:**
- `command-id` - Command to execute

**Side effects:**
- Prompts user for arguments (if command has interactive spec)
- Executes command with user-provided args
- Same as `execute-command` after arg collection

**Use case:** Implementing `M-x` or custom command palettes

---

#### `(command-metadata command-id)`
Returns command metadata.

**Parameters:**
- `command-id` - Command to query

**Returns:** Metadata map or `nil` if command not registered

**Example:**
```clojure
(command-metadata :forward-char)
;; => {:interactive true :doc "Move point forward one character" ...}
```

---

#### `(list-commands &optional filter)`
Returns list of all registered commands.

**Parameters:**
- `filter` (optional) - Filter predicate (e.g., `:interactive true`)

**Returns:** Sequence of command IDs

**Example:**
```clojure
(list-commands)  ;; all commands
(list-commands :interactive)  ;; only interactive commands (for M-x)
```

---

## 3. Keymap API

### Concepts

A **keymap** maps key sequences to commands. Keymaps:
- Are hierarchical (local → major mode → minor modes → global)
- Support prefix keys (e.g., `C-x` is a keymap, not a command)
- Have deterministic precedence rules (Emacs-compatible)

### Keymap Definition

#### `(define-keymap name bindings)`
Creates or updates a keymap.

**Parameters:**
- `name` (keyword) - Keymap identifier (e.g., `:global-map`, `:text-mode-map`)
- `bindings` (map) - Key sequence → command bindings
  - Keys are strings (e.g., `"C-f"`, `"C-x C-s"`)
  - Values are command keywords or nested keymaps

**Side effects:**
- Creates keymap if not exists
- Merges bindings into existing keymap

**Example:**
```clojure
(define-keymap :my-mode-map
  {"C-c C-c" :my-mode/execute
   "C-c C-k" :my-mode/cancel
   "C-c h"   {:prefix true
              "a" :my-mode/help-a
              "b" :my-mode/help-b}})
```

---

#### `(define-key keymap key-sequence command)`
Binds key sequence to command in keymap.

**Parameters:**
- `keymap` - Keymap name (keyword)
- `key-sequence` - Key string (e.g., `"C-x C-s"`)
- `command` - Command keyword or nested keymap

**Side effects:**
- Updates keymap binding
- Overwrites previous binding (if exists)

**Example:**
```clojure
(define-key :global-map "C-c g" :my-command)
(define-key :text-mode-map "C-c t" :text-specific-command)
```

---

#### `(lookup-key key-sequence &optional keymap)`
Looks up command bound to key sequence.

**Parameters:**
- `key-sequence` - Key string to look up
- `keymap` (optional) - Keymap to search (defaults to current context keymaps)

**Returns:** Command keyword, keymap (if prefix), or `nil` (if unbound)

**Guarantees:**
- Searches keymaps in precedence order (local → major → minor → global)
- Returns first matching binding
- Prefix keys return keymap object (for multi-key sequences)

**Example:**
```clojure
(lookup-key "C-f")  ;; => :forward-char (from global-map)
(lookup-key "C-x")  ;; => {:prefix true ...} (prefix keymap)
(lookup-key "C-x C-s")  ;; => :save-buffer
```

---

### Keymap Precedence

Lexicon follows **Emacs keymap precedence** exactly:

1. **Text/overlay properties at point** (highest precedence)
2. **Active minor mode keymaps** (reverse order of activation)
3. **Buffer's major mode keymap**
4. **Global keymap** (lowest precedence, always active)

**Guarantees:**
- Local bindings override global bindings
- Minor modes can override major mode
- First match wins (no ambiguity)

---

## 4. Mode API

### Concepts

**Modes** define buffer behavior. Lexicon has two types:

- **Major modes:** Mutually exclusive per buffer (e.g., `text-mode`, `clojure-mode`)
- **Minor modes:** Composable, multiple per buffer (e.g., `line-number-mode`, `evil-mode`)

Modes:
- Register keymaps
- Register hooks
- Set buffer-local variables
- Do NOT contain state (state is buffer-local)

### Major Mode

#### `(define-major-mode mode-id spec)`
Defines a major mode.

**Parameters:**
- `mode-id` (keyword) - Mode identifier (e.g., `:text-mode`)
- `spec` (map) - Mode specification
  - `:parent` (keyword) - Parent mode (inherits bindings/hooks)
  - `:keymap` (map) - Mode-specific keybindings
  - `:hooks` (map) - Hook functions (`:on-enable`, `:on-disable`)
  - `:doc` (string) - Documentation

**Side effects:**
- Registers mode in mode registry
- Creates mode keymap (if `:keymap` provided)

**Example:**
```clojure
(define-major-mode :my-text-mode
  {:parent :text-mode
   :keymap {"C-c C-c" :my-text-mode/execute}
   :hooks {:on-enable (fn [buf] (message "My text mode enabled"))
           :on-disable (fn [buf] (message "My text mode disabled"))}
   :doc "My custom text mode"})
```

---

#### `(major-mode buffer-id)`
Returns buffer's current major mode.

**Parameters:**
- `buffer-id` - Buffer to query

**Returns:** Mode keyword (e.g., `:fundamental-mode`)

**Guarantees:**
- Every buffer has exactly one major mode
- New buffers default to `:fundamental-mode`

---

#### `(set-major-mode! buffer-id mode-id)`
Changes buffer's major mode.

**Parameters:**
- `buffer-id` - Buffer to modify
- `mode-id` - New major mode

**Side effects:**
- Disables previous major mode (runs `:on-disable` hook)
- Enables new major mode (runs `:on-enable` hook)
- Updates buffer's active keymap
- Fires `change-major-mode-hook`

**Guarantees:**
- Only one major mode active at a time
- Mode transition is atomic (never half-enabled)

---

### Minor Mode

#### `(define-minor-mode mode-id spec)`
Defines a minor mode.

**Parameters:**
- `mode-id` (keyword) - Mode identifier (e.g., `:evil-mode`)
- `spec` (map) - Mode specification (same as major mode)

**Side effects:**
- Registers mode in mode registry
- Creates mode keymap (if `:keymap` provided)

**Example:**
```clojure
(define-minor-mode :my-minor-mode
  {:keymap {"C-c m" :my-minor-mode/toggle}
   :hooks {:on-enable (fn [buf] (message "Minor mode ON"))
           :on-disable (fn [buf] (message "Minor mode OFF"))}
   :doc "My custom minor mode"})
```

---

#### `(minor-modes buffer-id)`
Returns set of active minor modes in buffer.

**Parameters:**
- `buffer-id` - Buffer to query

**Returns:** Set of mode keywords (e.g., `#{:line-number-mode :evil-mode}`)

---

#### `(enable-minor-mode! buffer-id mode-id)`
Activates a minor mode in buffer.

**Parameters:**
- `buffer-id` - Buffer to modify
- `mode-id` - Minor mode to enable

**Side effects:**
- Adds mode to buffer's active minor modes
- Runs mode's `:on-enable` hook
- Activates mode's keymap
- Fires `mode-enable` hook (global)

**Guarantees:**
- Enabling already-active mode is a no-op
- Multiple minor modes can be active simultaneously

---

#### `(disable-minor-mode! buffer-id mode-id)`
Deactivates a minor mode in buffer.

**Parameters:**
- `buffer-id` - Buffer to modify
- `mode-id` - Minor mode to disable

**Side effects:**
- Removes mode from buffer's active minor modes
- Runs mode's `:on-disable` hook
- Deactivates mode's keymap
- Fires `mode-disable` hook (global)

---

## 5. Hook API

### Concepts

**Hooks** allow packages to observe and intercept editor activity. Hooks:
- Are named extension points (e.g., `before-command`, `after-change`)
- Receive context (map with event data)
- Can modify context or cancel execution (return `nil`)
- Are isolated (one hook cannot break another)

See `docs/core/hooks.md` for detailed hook specification.

### Hook Registration

#### `(add-hook hook-id fn &options)`
Registers a hook function.

**Parameters:**
- `hook-id` (keyword) - Hook to register on (e.g., `:before-command`)
- `fn` (function) - Hook function (receives context, returns modified context or `nil`)
- `options` (map, optional)
  - `:priority` (integer) - Execution order (higher runs first, default: 0)
  - `:buffer-local` (boolean) - Hook only runs in current buffer (default: false)
  - `:id` (keyword) - Unique hook identifier (for removal)

**Side effects:**
- Adds hook to hook registry
- Hook runs at appropriate phase

**Example:**
```clojure
(add-hook :before-command
  (fn [ctx]
    (when (= (:command ctx) :dangerous-command)
      (message "Warning: dangerous command!")
      ctx))  ;; return context to continue
  {:priority 100 :id :my-package/warn-hook})
```

---

#### `(remove-hook hook-id hook-fn-or-id)`
Unregisters a hook function.

**Parameters:**
- `hook-id` - Hook to remove from
- `hook-fn-or-id` - Hook function reference or `:id` from registration

**Side effects:**
- Removes hook from registry
- Hook no longer runs

**Use case:** Package cleanup on unload

---

### Standard Hooks

Lexicon defines 6 standard hooks (see `hooks.md` for full spec):

- `:before-command` - Before executing any command
- `:after-command` - After executing any command
- `:before-change` - Before buffer edit
- `:after-change` - After buffer edit
- `:buffer-switch` - When switching buffers
- `:mode-enable` - When mode enabled
- `:mode-disable` - When mode disabled

**Guarantees:**
- Hooks run in priority order (highest first)
- Hook failures are isolated (reported but don't break other hooks)
- Hooks run synchronously (no concurrency)

---

## 6. Message & Echo Area

#### `(message format & args)`
Displays message in echo area (minibuffer).

**Parameters:**
- `format` - Format string (e.g., `"Saved %s"`)
- `args` - Arguments to format string

**Side effects:**
- Updates echo area text
- Message auto-clears after timeout (5 seconds default)

**Example:**
```clojure
(message "Buffer saved: %s" (buffer-file-name (current-buffer)))
```

---

## 7. Utility Functions

#### `(region-active?)`
Returns whether region is active (mark set and distinct from point).

**Returns:** Boolean

---

#### `(region-bounds)`
Returns region start and end positions.

**Returns:** Map `{:start N :end M}` or `nil` if region not active

**Guarantees:**
- `:start` <= `:end` (automatically ordered)

---

#### `(beginning-of-line &optional n)`
Moves point to beginning of line.

**Parameters:**
- `n` (optional) - Line offset (default: 0, current line)

**Side effects:**
- Updates point

---

#### `(end-of-line &optional n)`
Moves point to end of line.

**Parameters:**
- `n` (optional) - Line offset (default: 0, current line)

**Side effects:**
- Updates point

---

## Non-Goals (Out of Scope)

The Core API explicitly does **NOT** cover:

- **UI rendering** - How buffers are displayed (DOM, canvas, etc.)
- **re-frame events** - Internal state management
- **Engine internals** - WASM gap buffer implementation
- **Networking** - HTTP requests, WebSocket connections
- **File system access** - Browser File System Access API (separate layer)
- **Persistence** - Local storage, IndexedDB (separate layer)

These are **host concerns**, not editor concerns.

---

## Versioning & Stability Policy

### Version 1.0.0 (This Document)

**Status:** Draft (not yet stable)

**Stability guarantees:**
- Once marked "Stable", breaking changes require major version bump
- Deprecations announced 1 version before removal
- New APIs can be added in minor versions

### Breaking Change Policy

A breaking change is:
- Removing a function
- Changing function signature (parameters, return type)
- Changing function behavior (semantics)
- Changing guarantees (relaxing or strengthening)

Breaking changes:
- Require major version bump (1.x.x → 2.0.0)
- Require migration guide
- Require deprecation period (1 version warning)

### Adding New APIs

New APIs can be added in minor versions (1.0.x → 1.1.0) if:
- They do not break existing APIs
- They are documented before release
- They follow Core API design principles

---

## Implementation Notes (For Core Developers)

### How to Expose Core API

1. **Create `lexicon.core.api` namespace**
   - All public functions live here
   - Docstrings match this specification
   - No re-frame or internal implementation leakage

2. **Wrap internal functions**
   - Core API calls internal implementation (re-frame events, WASM bridge)
   - Core API is the **only** public interface
   - Mark internal namespaces as `^:internal` (linter should warn)

3. **Test Core API**
   - Unit tests for every Core API function
   - Tests use ONLY Core API (not internals)
   - Tests are also package examples

**Example structure:**
```clojure
;; packages/editor-cljs/src/lexicon/core/api.cljs
(ns lexicon.core.api
  "Lexicon Core API - The only supported interface for packages"
  (:require [lexicon.internal.buffer :as buffer]    ;; internal
            [lexicon.internal.command :as command]  ;; internal
            [re-frame.core :as rf]))                ;; internal

(defn create-buffer
  "Creates a new buffer with given name.
   See docs/core/core-api.md for specification."
  [name]
  ;; Internal implementation
  (let [buf-id (buffer/generate-id)]
    (rf/dispatch [:buffer/create {:id buf-id :name name}])
    buf-id))

;; ... etc for all Core API functions
```

---

## Examples

### Example 1: Simple Package Using Core API

```clojure
(ns my-package.core
  (:require [lexicon.core.api :as api]))

(defn initialize! []
  ;; Define a command
  (api/define-command
    :my-package/insert-date
    {:interactive true
     :doc "Insert current date at point"}
    (fn []
      (let [date (.toLocaleDateString (js/Date.))]
        (api/insert! date))))

  ;; Bind command to key
  (api/define-key :global-map "C-c d" :my-package/insert-date)

  ;; Add hook
  (api/add-hook :after-save-hook
    (fn [ctx]
      (api/message "File saved!")
      ctx)
    {:id :my-package/save-notification}))

(defn cleanup! []
  ;; Cleanup on unload
  (api/unregister-command! :my-package/insert-date)
  (api/remove-hook :after-save-hook :my-package/save-notification))
```

### Example 2: Modal Editing Package (Evil-mode style)

```clojure
(ns evil.core
  (:require [lexicon.core.api :as api]))

(defn normal-mode! [buffer-id]
  ;; Switch to normal mode
  (api/enable-minor-mode! buffer-id :evil-normal-mode)
  (api/disable-minor-mode! buffer-id :evil-insert-mode))

(defn insert-mode! [buffer-id]
  ;; Switch to insert mode
  (api/disable-minor-mode! buffer-id :evil-normal-mode)
  (api/enable-minor-mode! buffer-id :evil-insert-mode))

(defn initialize! []
  ;; Define normal mode
  (api/define-minor-mode :evil-normal-mode
    {:keymap {"h" :evil/backward-char
              "l" :evil/forward-char
              "i" :evil/enter-insert-mode}})

  ;; Define insert mode
  (api/define-minor-mode :evil-insert-mode
    {:keymap {"<escape>" :evil/enter-normal-mode}})

  ;; Define commands
  (api/define-command :evil/enter-insert-mode {}
    (fn [] (insert-mode! (api/current-buffer))))

  (api/define-command :evil/enter-normal-mode {}
    (fn [] (normal-mode! (api/current-buffer)))))
```

---

## Next Steps

1. **Review this specification** - Ensure it covers package needs
2. **Implement `lexicon.core.api` namespace** - Wrap existing internal functions
3. **Write Core API tests** - Validate behavior matches spec
4. **Validate with test package** - Create minimal package using only Core API
5. **Mark as Stable** - Once implementation matches spec, freeze API

---

**Status:** Draft - ready for review
**Last Updated:** 2026-01-02
**Next Review:** After Phase 7.2 (events.cljs refactoring)
