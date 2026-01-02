# Lexicon Hook System Specification

**Version:** 1.0.0
**Status:** Design Document
**Last Updated:** 2026-01-02

## Table of Contents

1. [Overview](#overview)
2. [Hook Lifecycle Model](#hook-lifecycle-model)
3. [Standard Hooks](#standard-hooks)
4. [Hook Entry Structure](#hook-entry-structure)
5. [Context Model](#context-model)
6. [Isolation and Error Handling](#isolation-and-error-handling)
7. [Priority and Ordering](#priority-and-ordering)
8. [Hook API Reference](#hook-api-reference)
9. [Usage Examples](#usage-examples)
10. [Implementation Guidelines](#implementation-guidelines)

---

## Overview

The Lexicon hook system provides **extension points** throughout the editor lifecycle, enabling packages to observe and react to events without modifying core code. Hooks are the primary mechanism for:

- **Observing state changes** (buffer switches, mode changes)
- **Reacting to user actions** (before/after command execution)
- **Intercepting edit operations** (before/after text changes)
- **Customizing behavior** (font-lock, completion, etc.)

### Design Principles

1. **Non-invasive**: Hooks observe; they don't directly modify core behavior
2. **Isolated**: Hook failures don't crash the editor or affect other hooks
3. **Ordered**: Hooks run in predictable priority order
4. **Composable**: Multiple packages can register hooks for the same event
5. **Debuggable**: Hook execution is traceable for troubleshooting

### Comparison to Emacs

Lexicon's hook system closely follows Emacs Lisp hooks with some enhancements:

| Feature | Emacs | Lexicon |
|---------|-------|---------|
| Hook registration | `add-hook` | `add-hook` (via Core API) |
| Removal | `remove-hook` | `remove-hook` (via Core API) |
| Priority | Manual ordering | Built-in priority system |
| Error isolation | Optional (`debug-on-error`) | Always isolated |
| Context passing | Dynamic scope | Explicit context argument |
| Async support | No | Yes (future) |

---

## Hook Lifecycle Model

### Hook Execution Flow

```
User Action / State Change
         ↓
    Trigger Hook
         ↓
  Collect Hook Entries (sorted by priority)
         ↓
    For each entry:
      ┌─────────────────┐
      │ Try             │
      │   Execute fn    │
      │ Catch           │
      │   Log error     │
      │   Continue      │
      └─────────────────┘
         ↓
    Return Results
```

### Hook States

Hooks exist in one of three states:

1. **Registered**: Added to hook registry, available for execution
2. **Active**: Currently executing (prevents re-entrancy)
3. **Disabled**: Temporarily disabled (e.g., during recursive edits)

### Re-entrancy Protection

Hooks are **not re-entrant**. If a hook function triggers the same hook (e.g., `:before-change-hook` inserting text), the nested invocation is skipped to prevent infinite loops.

```clojure
;; Example: infinite loop prevented
(add-hook :before-change-hook
  (fn [context]
    ;; This insert triggers :before-change-hook again
    ;; but nested call is skipped
    (insert! "prefix: ")))
```

---

## Standard Hooks

Lexicon provides **six standard hooks** modeled after Emacs:

### 1. `:before-command-hook`

**Trigger**: Immediately before any interactive command executes

**Context**:
```clojure
{:command-id   :self-insert-command
 :args         [\a]
 :buffer-id    "buffer-123"
 :point        42
 :prefix-arg   nil}
```

**Use Cases**:
- Recording command history
- Pre-command state snapshots
- Command logging/telemetry

**Example**:
```clojure
(add-hook :before-command-hook
  (fn [ctx]
    (log/debug "Executing command:" (:command-id ctx))))
```

---

### 2. `:after-command-hook`

**Trigger**: Immediately after any interactive command completes (success or failure)

**Context**:
```clojure
{:command-id   :self-insert-command
 :args         [\a]
 :buffer-id    "buffer-123"
 :point        43          ;; Updated point
 :result       nil
 :error        nil         ;; or exception if command failed
 :duration-ms  2.3}
```

**Use Cases**:
- Updating UI (cursor, mode-line)
- Triggering auto-save
- Font-lock (syntax highlighting)
- Completion popups

**Example**:
```clojure
(add-hook :after-command-hook
  (fn [ctx]
    ;; Rehighlight visible region after edit
    (when (editing-command? (:command-id ctx))
      (font-lock-fontify-buffer (:buffer-id ctx)))))
```

---

### 3. `:before-change-hook`

**Trigger**: Before any text modification in a buffer

**Context**:
```clojure
{:buffer-id  "buffer-123"
 :start      10           ;; Start position of change
 :end        15           ;; End position of change
 :old-text   "hello"      ;; Text being replaced/deleted
 :new-text   "world"      ;; Text being inserted (or "" for deletion)
 :command-id :self-insert-command  ;; May be nil for non-command changes
 :point      10}
```

**Use Cases**:
- Validating edits (read-only regions)
- Recording pre-change state for undo
- Syntax validation
- Marker adjustment preparation

**Important**: `:before-change-hook` can **observe** but **cannot prevent** the change. To prevent edits, use buffer's `:is-read-only?` flag.

**Example**:
```clojure
(add-hook :before-change-hook
  (fn [ctx]
    ;; Log changes in read-only mode
    (when (buffer-read-only? (:buffer-id ctx))
      (log/warn "Attempted edit in read-only buffer"))))
```

---

### 4. `:after-change-hook`

**Trigger**: After any text modification in a buffer

**Context**:
```clojure
{:buffer-id  "buffer-123"
 :start      10           ;; Start position of change
 :end        15           ;; New end position after change
 :old-text   "hello"      ;; Text that was replaced/deleted
 :new-text   "world"      ;; Text that was inserted
 :old-length 5            ;; Length of old-text
 :new-length 5            ;; Length of new-text
 :command-id :self-insert-command
 :point      15}
```

**Use Cases**:
- Syntax highlighting (font-lock)
- Indentation adjustment
- Bracket matching
- Auto-pairing
- Modified flag update

**Example**:
```clojure
(add-hook :after-change-hook
  (fn [ctx]
    ;; Auto-close brackets
    (when (= (:new-text ctx) "(")
      (save-excursion
        (insert! ")")
        (backward-char)))))
```

---

### 5. `:buffer-switch-hook`

**Trigger**: When the current buffer changes (via `switch-to-buffer`, window switching, etc.)

**Context**:
```clojure
{:old-buffer-id "buffer-123"
 :new-buffer-id "buffer-456"
 :window-id     "window-1"    ;; Which window switched
 :reason        :user-command  ;; :user-command, :automatic, :focus
 :point         0}             ;; Point in new buffer
```

**Use Cases**:
- Updating mode-line
- Loading buffer-local settings
- Activating buffer-specific modes
- Updating window decorations

**Example**:
```clojure
(add-hook :buffer-switch-hook
  (fn [ctx]
    ;; Enable flyspell-mode for text buffers
    (when (text-mode? (:new-buffer-id ctx))
      (enable-minor-mode! (:new-buffer-id ctx) :flyspell-mode))))
```

---

### 6. `:mode-hook`

**Trigger**: When a major or minor mode is enabled/disabled

**Context**:
```clojure
{:buffer-id  "buffer-123"
 :mode-id    :clojure-mode
 :mode-type  :major          ;; :major or :minor
 :action     :enable         ;; :enable or :disable
 :old-mode   :fundamental-mode  ;; Only for major mode switches
 :point      42}
```

**Use Cases**:
- Installing mode-specific keymaps
- Registering buffer-local hooks
- Configuring mode-specific variables
- Setting up syntax tables

**Example**:
```clojure
(add-hook :mode-hook
  (fn [ctx]
    (when (and (= (:mode-id ctx) :clojure-mode)
               (= (:action ctx) :enable))
      ;; Enable paredit-mode automatically in clojure-mode
      (enable-minor-mode! (:buffer-id ctx) :paredit-mode))))
```

---

## Hook Entry Structure

Each hook is a **priority queue** of **hook entries**. An entry is a map with:

```clojure
{:id         :my-package/syntax-highlight  ;; Unique identifier
 :priority   50                             ;; Lower = earlier (0-100)
 :fn         (fn [ctx] ...)                 ;; Hook function
 :enabled?   true                           ;; Can be temporarily disabled
 :package-id :my-package                    ;; Owner package
 :doc        "Highlight syntax after edits" ;; Description
 :added-at   #inst "2026-01-02T10:00:00"}  ;; Timestamp
```

### Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `:id` | keyword | Yes | Unique identifier (namespaced recommended) |
| `:priority` | int (0-100) | No | Execution order (default: 50) |
| `:fn` | function | Yes | Hook function `(fn [context] ...)` |
| `:enabled?` | boolean | No | Enabled state (default: true) |
| `:package-id` | keyword | No | Owner package for bulk removal |
| `:doc` | string | No | Human-readable description |

### Priority Ranges

| Range | Purpose | Examples |
|-------|---------|----------|
| 0-19 | **Critical** (must run first) | Undo recording, marker adjustment |
| 20-39 | **High** | Font-lock, syntax validation |
| 40-59 | **Normal** (default) | Auto-pairing, indentation |
| 60-79 | **Low** | Telemetry, logging |
| 80-100 | **Deferred** (run last) | UI updates, animations |

---

## Context Model

### Context Structure

Every hook receives a **context map** containing:

1. **Core fields** (always present):
   - `:buffer-id` - Current buffer
   - `:point` - Current cursor position
   - `:timestamp` - When hook triggered (milliseconds since epoch)

2. **Hook-specific fields** (vary by hook type):
   - See [Standard Hooks](#standard-hooks) for details

3. **Dynamic context** (optional):
   - `:*current-command*` - Currently executing command
   - `:*prefix-arg*` - Universal argument value
   - `:*this-command-keys*` - Key sequence that invoked command

### Context Immutability

Contexts are **immutable**. Hook functions receive a read-only snapshot. To modify editor state, use Core API functions (`insert!`, `delete-region!`, etc.), not context mutation.

```clojure
;; ❌ WRONG: Mutating context does nothing
(add-hook :after-command-hook
  (fn [ctx]
    (assoc ctx :point 100)))  ;; No effect!

;; ✅ CORRECT: Use Core API
(add-hook :after-command-hook
  (fn [ctx]
    (set-point! (:buffer-id ctx) 100)))
```

### Context Debugging

During development, enable context logging:

```clojure
(rf/dispatch [:hooks/enable-logging :after-change-hook])

;; Now see context for every :after-change-hook execution
;; Console: "🪝 Hook :after-change-hook context: {:buffer-id ...}"
```

---

## Isolation and Error Handling

### Error Isolation

Hook failures **never crash the editor**. Each hook function is wrapped in a try-catch:

```clojure
(defn run-hook-entry [entry context]
  (try
    ((:fn entry) context)
    (catch js/Error e
      (log/error "Hook failed:" (:id entry) e)
      ;; Continue to next hook
      nil)))
```

### Error Reporting

When a hook fails:

1. **Error logged** to console with stack trace
2. **User notified** (optional, based on severity)
3. **Hook disabled** after N consecutive failures (future)
4. **Other hooks continue** unaffected

**Example error log**:
```
❌ Hook failed: :font-lock/fontify-region
   in :after-change-hook
   Error: Cannot read property 'length' of undefined
   at font-lock.cljs:123
   Context: {:buffer-id "buffer-456", :start 10, :end 15}
```

### Debugging Hooks

To debug hook execution:

```clojure
;; Enable hook tracing
(rf/dispatch [:hooks/enable-tracing true])

;; Console output:
;; 🪝 :after-command-hook triggered (3 entries)
;; 🪝   → :font-lock/fontify [priority 30] (2.1ms) ✅
;; 🪝   → :my-package/logger [priority 70] (0.3ms) ✅
;; 🪝   → :telemetry/track [priority 90] (1.2ms) ❌ Error: timeout
```

---

## Priority and Ordering

### Execution Order

Hooks execute in **priority order** (lowest first):

```clojure
;; These hooks run in this order:
(add-hook :after-change-hook
  {:id :undo/record :priority 10 :fn record-undo})     ;; 1st

(add-hook :after-change-hook
  {:id :font-lock/highlight :priority 30 :fn highlight})  ;; 2nd

(add-hook :after-change-hook
  {:id :my-package/log :priority 50 :fn log-change})   ;; 3rd (default)

(add-hook :after-change-hook
  {:id :telemetry/track :priority 80 :fn track})       ;; 4th
```

### Tie-breaking

If multiple hooks have the same priority, they execute in **insertion order** (first added runs first).

```clojure
(add-hook :after-change-hook {:id :a :priority 50 :fn fa})  ;; Runs 1st
(add-hook :after-change-hook {:id :b :priority 50 :fn fb})  ;; Runs 2nd
```

### Priority Guidelines

**When to use low priority (0-19)**:
- Operations that **must** happen before others (undo recording)
- State that other hooks depend on

**When to use high priority (80-100)**:
- Expensive operations (network calls, animations)
- Non-critical features (telemetry, optional UI updates)

**Default (50)**: Most hooks should use default priority unless there's a specific ordering requirement.

---

## Hook API Reference

### `add-hook`

Registers a hook function.

**Signature**:
```clojure
(add-hook hook-id fn-or-entry & options)
```

**Parameters**:
- `hook-id` (keyword): Hook to attach to (`:after-command-hook`, etc.)
- `fn-or-entry` (function or map): Hook function or full entry map
- `options` (optional): Additional options as keyword args

**Options** (when passing function):
- `:id` (keyword): Unique identifier (default: auto-generated)
- `:priority` (int): Execution priority (default: 50)
- `:package-id` (keyword): Owner package
- `:doc` (string): Description

**Returns**: Hook entry ID (keyword)

**Examples**:
```clojure
;; Simple function
(add-hook :after-command-hook
  (fn [ctx] (log/debug "Command executed")))

;; With options
(add-hook :after-command-hook
  (fn [ctx] (highlight-syntax ctx))
  :id :my-package/highlight
  :priority 30
  :doc "Syntax highlighting")

;; Full entry map
(add-hook :after-change-hook
  {:id :paredit/auto-pair
   :priority 40
   :fn (fn [ctx] (auto-pair-brackets ctx))
   :package-id :paredit
   :doc "Auto-close brackets"})
```

---

### `remove-hook`

Unregisters a hook function.

**Signature**:
```clojure
(remove-hook hook-id hook-fn-or-id)
```

**Parameters**:
- `hook-id` (keyword): Hook to remove from
- `hook-fn-or-id` (function or keyword): Function or entry ID to remove

**Returns**: `true` if removed, `false` if not found

**Examples**:
```clojure
;; Remove by ID
(remove-hook :after-command-hook :my-package/highlight)

;; Remove by function reference
(def my-fn (fn [ctx] ...))
(add-hook :after-change-hook my-fn :id :my-fn)
(remove-hook :after-change-hook my-fn)  ;; Works
```

---

### `enable-hook`

Enables a previously disabled hook entry.

**Signature**:
```clojure
(enable-hook hook-id entry-id)
```

**Parameters**:
- `hook-id` (keyword): Hook containing the entry
- `entry-id` (keyword): Entry to enable

**Returns**: `true` if enabled, `false` if not found

---

### `disable-hook`

Temporarily disables a hook entry (without removing it).

**Signature**:
```clojure
(disable-hook hook-id entry-id)
```

**Parameters**:
- `hook-id` (keyword): Hook containing the entry
- `entry-id` (keyword): Entry to disable

**Returns**: `true` if disabled, `false` if not found

**Use Case**: Temporarily disable expensive hooks during batch operations:
```clojure
(disable-hook :after-change-hook :font-lock/fontify)
(dotimes [i 1000]
  (insert! "line\n"))  ;; No syntax highlighting during bulk insert
(enable-hook :after-change-hook :font-lock/fontify)
(font-lock-fontify-buffer)  ;; Highlight once at end
```

---

### `list-hooks`

Returns all hooks or entries for a specific hook.

**Signature**:
```clojure
(list-hooks)              ;; All hooks
(list-hooks hook-id)      ;; Entries for specific hook
```

**Returns**:
- `(list-hooks)` → `{:after-command-hook [...], :after-change-hook [...], ...}`
- `(list-hooks :after-command-hook)` → `[{:id :a :priority 10 ...}, {:id :b :priority 50 ...}]`

**Example**:
```clojure
;; Debugging: show all after-command hooks
(doseq [entry (list-hooks :after-command-hook)]
  (println (:id entry) "-" (:priority entry) "-" (:doc entry)))

;; Output:
;; :undo/record - 10 - Record undo information
;; :font-lock/fontify - 30 - Syntax highlighting
;; :telemetry/track - 80 - Usage tracking
```

---

### `clear-hooks`

Removes all hook entries (or all entries for a specific hook).

**Signature**:
```clojure
(clear-hooks)              ;; Clear ALL hooks (dangerous!)
(clear-hooks hook-id)      ;; Clear specific hook
```

**Use Case**: Testing, package unloading

**Example**:
```clojure
;; During tests
(defn setup []
  (clear-hooks :after-change-hook)  ;; Clean slate
  (add-hook :after-change-hook test-fn))
```

---

## Usage Examples

### Example 1: Auto-save on Idle

Trigger auto-save after 5 seconds of inactivity:

```clojure
(ns my-package.auto-save
  (:require [lexicon.core.api :as api]))

(def idle-timer (atom nil))

(defn schedule-auto-save [buffer-id]
  ;; Cancel previous timer
  (when @idle-timer
    (js/clearTimeout @idle-timer))

  ;; Schedule new auto-save
  (reset! idle-timer
    (js/setTimeout
      (fn []
        (when (buffer-modified? buffer-id)
          (save-buffer buffer-id)
          (message "Auto-saved %s" (buffer-name buffer-id))))
      5000)))  ;; 5 seconds

(defn initialize! []
  (api/add-hook :after-command-hook
    (fn [ctx]
      (schedule-auto-save (:buffer-id ctx)))
    :id :auto-save/schedule
    :priority 90  ;; Low priority (run last)
    :doc "Schedule auto-save after idle"))
```

---

### Example 2: Syntax Highlighting (Font-lock)

Re-highlight visible region after text changes:

```clojure
(ns font-lock.core
  (:require [lexicon.core.api :as api]))

(defn fontify-region [buffer-id start end]
  ;; Apply syntax faces to buffer region
  (let [text (buffer-substring buffer-id start end)
        tokens (tokenize text)  ;; Parse into tokens
        faces (map token->face tokens)]
    (apply-faces buffer-id start faces)))

(defn fontify-after-change [ctx]
  (let [{:keys [buffer-id start end]} ctx]
    ;; Expand region to whole lines for context
    (let [line-start (line-beginning-position buffer-id start)
          line-end (line-end-position buffer-id end)]
      (fontify-region buffer-id line-start line-end))))

(defn initialize! []
  (api/add-hook :after-change-hook
    fontify-after-change
    :id :font-lock/fontify
    :priority 30  ;; High priority (run early)
    :doc "Syntax highlighting"))
```

---

### Example 3: Command Logger

Log all commands to console:

```clojure
(ns dev-tools.logger
  (:require [lexicon.core.api :as api]))

(defn log-command [ctx]
  (let [{:keys [command-id args duration-ms error]} ctx]
    (if error
      (js/console.error
        (str "❌ Command failed: " command-id)
        {:args args :error error})
      (js/console.log
        (str "✅ " command-id " (" duration-ms "ms)")
        {:args args}))))

(defn initialize! []
  (api/add-hook :after-command-hook
    log-command
    :id :dev-tools/log-commands
    :priority 90  ;; Low priority (logging is not critical)
    :doc "Log all commands to console"))
```

---

### Example 4: Read-only Region Enforcement

Prevent edits in protected regions:

```clojure
(ns my-package.read-only-regions
  (:require [lexicon.core.api :as api]))

(defn protected-region? [buffer-id pos]
  ;; Check if position is in a protected region
  ;; (e.g., has text property :read-only)
  (get-text-property buffer-id pos :read-only))

(defn check-read-only [ctx]
  (let [{:keys [buffer-id start end]} ctx]
    (when (or (protected-region? buffer-id start)
              (protected-region? buffer-id end))
      ;; Log warning (can't prevent in :before-change-hook)
      (message "Warning: Editing protected region")
      ;; In future: could throw to abort transaction
      )))

(defn initialize! []
  (api/add-hook :before-change-hook
    check-read-only
    :id :my-package/check-read-only
    :priority 10  ;; Critical (run first)
    :doc "Warn when editing protected regions"))
```

---

### Example 5: Mode-specific Configuration

Enable paredit-mode automatically in Lisp buffers:

```clojure
(ns paredit.core
  (:require [lexicon.core.api :as api]))

(defn setup-paredit-for-lisp [ctx]
  (let [{:keys [buffer-id mode-id action]} ctx]
    (when (and (#{:clojure-mode :emacs-lisp-mode :scheme-mode} mode-id)
               (= action :enable))
      ;; Enable paredit-mode
      (api/enable-minor-mode! buffer-id :paredit-mode)
      (message "Paredit enabled for %s" mode-id))))

(defn initialize! []
  (api/add-hook :mode-hook
    setup-paredit-for-lisp
    :id :paredit/auto-enable
    :priority 50
    :doc "Auto-enable paredit in Lisp modes"))
```

---

### Example 6: Batch Operations with Hooks Disabled

Disable expensive hooks during bulk operations:

```clojure
(ns my-package.bulk-insert
  (:require [lexicon.core.api :as api]))

(defn insert-1000-lines []
  ;; Disable syntax highlighting during bulk insert
  (api/disable-hook :after-change-hook :font-lock/fontify)

  ;; Bulk insert
  (dotimes [i 1000]
    (api/insert! (str "Line " i "\n")))

  ;; Re-enable and highlight once
  (api/enable-hook :after-change-hook :font-lock/fontify)
  (font-lock-fontify-buffer))
```

---

## Implementation Guidelines

### For Core Developers

1. **Hook Triggering**:
   - Call `run-hooks` at appropriate points
   - Construct complete context maps
   - Never skip hooks unless necessary

   ```clojure
   ;; Example: trigger :after-change-hook
   (defn apply-edit! [buffer-id start end text]
     (let [old-text (buffer-substring buffer-id start end)]
       ;; Apply edit...
       (run-hooks :after-change-hook
         {:buffer-id buffer-id
          :start start
          :end (+ start (count text))
          :old-text old-text
          :new-text text
          :command-id *current-command*
          :point (point buffer-id)})))
   ```

2. **Context Construction**:
   - Include all relevant information
   - Use consistent field names
   - Add timestamp for debugging

3. **Performance**:
   - Don't trigger hooks in tight loops
   - Batch changes when possible
   - Provide hook-disabling mechanism

4. **Error Handling**:
   - Wrap hook execution in try-catch
   - Log errors with full context
   - Continue execution after failures

5. **Testing**:
   - Test hook execution order
   - Test error isolation
   - Test re-entrancy protection
   - Mock hooks in unit tests

---

### For Package Developers

1. **Hook Function Design**:
   - Keep functions **pure** when possible
   - Use Core API for side effects
   - Don't mutate context
   - Return quickly (avoid blocking)

   ```clojure
   ;; ✅ GOOD: Fast, uses Core API
   (defn my-hook [ctx]
     (when (editing-command? (:command-id ctx))
       (api/insert! " ")))  ;; Fast operation

   ;; ❌ BAD: Slow, blocks editor
   (defn bad-hook [ctx]
     (js/fetch "/api/log")  ;; Blocks!
     (Thread/sleep 1000))   ;; Very bad!
   ```

2. **Priority Selection**:
   - Use default (50) unless you have ordering requirements
   - Document why you chose non-default priority
   - Don't overuse critical priorities (0-19)

3. **Hook Registration**:
   - Always provide `:id` (namespaced)
   - Provide `:doc` for discoverability
   - Specify `:package-id` for bulk removal
   - Unregister hooks on package unload

   ```clojure
   (defn initialize! []
     (api/add-hook :after-command-hook my-fn
       :id :my-package/my-hook
       :priority 50
       :package-id :my-package
       :doc "Does something useful"))

   (defn shutdown! []
     (api/remove-hook :after-command-hook :my-package/my-hook))
   ```

4. **Error Handling**:
   - Let hook system catch errors (don't catch unless necessary)
   - Log your own errors for debugging
   - Fail gracefully

5. **Testing Hooks**:
   - Test in isolation
   - Mock Core API functions
   - Test with multiple priority scenarios

   ```clojure
   (deftest my-hook-test
     (with-redefs [api/insert! (fn [text] (swap! calls conj text))]
       (my-hook {:buffer-id "test" :command-id :self-insert-command})
       (is (= ["inserted text"] @calls))))
   ```

---

## Migration from Current Implementation

### Current State (Phase 6)

Lexicon currently has **ad-hoc hook support**:
- Some re-frame events act as hooks (`:editor/after-command`)
- No unified hook registry
- No priority system
- Limited error isolation

### Migration Path (Phase 7.3)

**Week 1: Hook Infrastructure**
1. Create hook registry in app-db
2. Implement `add-hook`, `remove-hook`, `run-hooks`
3. Add priority queue management
4. Add error isolation wrapper

**Week 2: Standard Hooks**
1. Implement `:before-command-hook` / `:after-command-hook`
2. Implement `:before-change-hook` / `:after-change-hook`
3. Implement `:buffer-switch-hook`
4. Implement `:mode-hook`

**Week 3: Integration**
1. Replace ad-hoc hooks with standard hooks
2. Migrate existing hook-like behavior
3. Update Core API documentation
4. Add hook debugging tools

**Week 4: Testing**
1. Unit tests for each hook type
2. Integration tests for hook interactions
3. Error isolation tests
4. Performance tests (1000+ hooks)

---

## Future Enhancements

### Async Hooks (Phase 8+)

Support asynchronous hook functions:

```clojure
(api/add-hook-async :after-save-hook
  (fn [ctx]
    (-> (js/fetch "/api/backup" {:method "POST" :body ...})
        (.then #(message "Backup complete"))
        (.catch #(message "Backup failed")))))
```

### Conditional Hooks

Hooks that only run under certain conditions:

```clojure
(api/add-hook :after-change-hook my-fn
  :when (fn [ctx] (= (:buffer-id ctx) "specific-buffer")))
```

### Hook Profiling

Built-in performance profiling:

```clojure
(api/profile-hooks :after-command-hook)
;; Output:
;; :font-lock/fontify - 50 calls - avg 2.3ms - total 115ms
;; :telemetry/track   - 50 calls - avg 0.1ms - total 5ms
```

### Hook Composition

Combine multiple hooks:

```clojure
(api/compose-hooks :my-package/combined
  [:font-lock/fontify :paredit/check-balance])
```

---

## References

1. **Emacs Lisp Manual**: [Hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks.html)
2. **Emacs Lisp Manual**: [Standard Hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html)
3. **Lexicon Core API**: [docs/core/core-api.md](./core-api.md)
4. **Lexicon Execution Model**: [docs/core/execution-model.md](./execution-model.md)

---

## Appendix A: Hook Quick Reference

| Hook | Trigger | Common Use Cases |
|------|---------|------------------|
| `:before-command-hook` | Before command | Logging, state snapshots |
| `:after-command-hook` | After command | UI updates, font-lock, auto-save |
| `:before-change-hook` | Before text edit | Validation, undo prep |
| `:after-change-hook` | After text edit | Syntax highlight, indentation |
| `:buffer-switch-hook` | Buffer switch | Mode-line, buffer-local setup |
| `:mode-hook` | Mode enable/disable | Keymap install, mode config |

---

## Appendix B: Priority Guidelines Summary

| Priority | Range | Purpose | Examples |
|----------|-------|---------|----------|
| Critical | 0-19 | Must run first | Undo, marker adjustment |
| High | 20-39 | Important features | Font-lock, validation |
| Normal | 40-59 | Default | Auto-pairing, indentation |
| Low | 60-79 | Optional features | Logging, telemetry |
| Deferred | 80-100 | Run last | Animations, analytics |

---

## Appendix C: Error Messages

Common hook errors and their meanings:

| Error | Meaning | Solution |
|-------|---------|----------|
| `Hook not found: :foo-hook` | Invalid hook ID | Use standard hooks or define custom |
| `Hook entry not found: :my-id` | Entry doesn't exist | Check ID spelling, may be removed |
| `Re-entrant hook detected` | Hook triggered itself | Review hook logic, may be intentional |
| `Hook timeout after 5000ms` | Hook took too long | Optimize hook, move to async |
| `Priority must be 0-100` | Invalid priority | Use valid range |

---

**End of Hook System Specification**
