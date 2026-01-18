# Debugging E2E Tests in Lexicon

## Overview

This guide documents the debugging approach that successfully fixed the undo system bugs. This methodology is critical for debugging issues in headless browser tests where traditional debugging tools are limited.

## The Challenge

E2E tests run in a **headless browser** environment, which creates unique debugging challenges:

1. **No browser DevTools** - Can't use breakpoints or step through code
2. **Console logs are hidden** - `console.log()` doesn't appear in test output
3. **Events are asynchronous** - Hard to trace event flow through re-frame
4. **Compilation warnings can be silent** - Undeclared vars fail silently

## The Solution: Messages Buffer Logging

### Why It Works

The E2E test fixture automatically captures and displays `window.editorState.messagesBuffer` on test failures. This buffer is populated by Lexicon's logging system (`lexicon.log`), which:

1. **Always succeeds** - Never throws, never blocks
2. **Persists across page loads** - History is maintained
3. **Is visible in test output** - Automatically printed on failure
4. **Works with re-frame** - Safe to call from event handlers

### How To Use It

#### Step 1: Import the logging module

```clojure
(ns your.namespace
  (:require [lexicon.log :as log]))
```

#### Step 2: Add debug logging at key decision points

```clojure
(rf/reg-event-fx
  :your-event
  (fn [{:keys [db]} [_ arg]]
    (log/debug (str "🔍 Event called with arg: " arg))

    (if (some-condition? db)
      (do
        (log/debug "✅ Condition true - taking branch A")
        {:fx [[:dispatch [:branch-a]]]})
      (do
        (log/debug "❌ Condition false - taking branch B")
        {:fx [[:dispatch [:branch-b]]]}))))
```

#### Step 3: Run the test and examine Messages buffer

```bash
bb test:e2e your.test.namespace
```

The test output will show:
```
*Messages* buffer contents:
--------------------------------------------------------------------------------
[HH:MM:SS] 🔍 Event called with arg: foo
[HH:MM:SS] ✅ Condition true - taking branch A
```

### Best Practices

1. **Use emoji prefixes** for visual scanning:
   - 🔍 = Investigation/tracing
   - ✅ = Success/expected path
   - ❌ = Error/unexpected path
   - ⏪ = Undo operations
   - 🔧 = Setup/initialization

2. **Log at decision points**, not just entry/exit:
   ```clojure
   ;; BAD - too generic
   (log/debug "Function called")

   ;; GOOD - shows actual values and decisions
   (log/debug (str "Keymap lookup: key=" key " found=" command))
   ```

3. **Include relevant state** in log messages:
   ```clojure
   (log/debug (str "Undo stack size: " (count stack) " buffer: " buffer-id))
   ```

4. **Remove debug logs** after fixing the issue - they add noise to production

### Available Log Levels

```clojure
(log/debug "Detailed diagnostic info")  ; Use this for debugging
(log/info "Important events")           ; Use sparingly
(log/warn "Unexpected but handled")
(log/error "Failures")
```

**For E2E debugging, use `log/debug`** - it shows in Messages buffer but won't clutter production logs.

## Real Example: Debugging Undo

### The Problem

Undo wasn't working - C-/ appeared to do nothing.

### The Investigation

#### Step 1: Verify keyboard events are received

```clojure
(defn handle-keydown [event]
  (let [key-str (events/key-event-to-string event)]
    (log/debug (str "🔍 handle-keydown: key-str = " key-str))
    ...))
```

**Result**: Confirmed C-/ was being received

#### Step 2: Verify command dispatch

```clojure
(rf/reg-event-fx
  :handle-key-sequence
  (fn [{:keys [db]} [_ key-str]]
    (log/debug (str "🔍 Keymap: key=" key-str " command=" command-name))
    ...))
```

**Result**: Confirmed :undo command was being dispatched

#### Step 3: Verify undo event is called

```clojure
(rf/reg-event-fx
  :undo
  (fn [{:keys [db]} [_]]
    (log/debug (str "🔍 Undo called, stack size: " stack-size))
    ...))
```

**Result**: Event WAS called, stack had entries!

#### Step 4: Verify undo entries are applied

```clojure
(defn apply-undo-entry [buffer-state entry]
  (log/debug (str "🔍 apply-undo-entry: " (pr-str entry)))
  ...
  (log/debug "✅ WASM op applied successfully"))
```

**Result**: Found the bug! Entry had `:text nil`, causing JavaScript error

### The Fix

The undo entry wasn't storing the deleted text. Fixed by adding `:text` field when recording insert operations.

## Common Pitfalls

### 1. Undeclared Vars

If you see debug logs disappearing, check for compilation warnings:

```
WARNING: Use of undeclared Var your.namespace/message
```

**Solution**: Import the function or use the correct namespace

### 2. Logging from Wrong Context

Don't use `log/debug` from:
- Outside event handlers (timing issues)
- Inside `doseq` that discards results
- Before logging system is initialized

### 3. Too Much Logging

Don't log:
- Inside tight loops
- Every keystroke in production
- Large data structures without `pr-str`

## Alternative: Console Logging (Not Recommended)

You CAN use `(.log js/console "message")` but:
- ❌ Not captured by E2E test output
- ❌ Requires manually checking browser console
- ❌ Lost in headless mode
- ✅ Use ONLY for debugging in regular browser

## Summary

**The Golden Rule**: When debugging E2E test failures, use `lexicon.log/debug` to write diagnostic messages. The Messages buffer will show you exactly what happened.

This approach:
- ✅ Works in headless browsers
- ✅ Captured automatically by test fixture
- ✅ Shows exact event flow
- ✅ Reveals state at decision points
- ✅ Fast iteration (no recompilation for log changes)
