---
name: write-test
description: Author E2E tests for Lexicon following project conventions. Use when creating new tests, fixing existing tests, or when asked to write test code.
argument-hint: [feature-name or test-file-path]
allowed-tools: Read, Glob, Grep, Write, Edit, Bash(bb lint*)
---

# Lexicon E2E Test Authoring Guide

When writing E2E tests for Lexicon, follow these guidelines strictly. Tests that violate these rules will fail `bb lint`.

---

## 1. Test Location Decision

**First, determine where the test belongs:**

| Test Type | Location | Can Use eval-lisp? |
|-----------|----------|-------------------|
| User-visible behavior via keyboard | `e2e_tests/lexicon/ui/<category>/` | NO |
| Lisp API for package developers | `e2e_tests/lexicon/lisp/` | YES |

**UI test categories:**
- `editing/` - Undo, kill ring, text manipulation
- `buffers/` - Buffer operations, switching, identity
- `windows/` - Window splitting, navigation
- `minibuffer/` - Minibuffer, completions, prompts
- `modes/` - Major/minor modes, text properties
- `files/` - Dired, file operations, persistence
- `search/` - Isearch, occur, grep

---

## 2. Critical Rules (Lint-Enforced)

These rules are enforced by `bb lint`. Violations will fail CI.

### Rule 1: NO eval-lisp in UI tests

```clojure
;; WRONG - bypasses user input validation
(h/eval-lisp "(insert \"text\")")
(e/js-execute driver "window.evalLisp('(insert \"x\")')")

;; CORRECT - simulates real user input
(h/type-text "text")
```

### Rule 2: NO e/fill or e/fill-active

```clojure
;; WRONG - bypasses keyboard event system
(e/fill driver {:css ".input"} "text")
(e/fill-active driver "text")

;; CORRECT - uses keyboard simulation
(h/type-text "text")
(h/type-in-minibuffer "command-name")
```

### Rule 3: NO direct state access

```clojure
;; WRONG - accesses internal state directly
@rfdb/app-db
(get-in @rfdb/app-db [:buffers :current])

;; CORRECT - uses read-only query helpers
(h/get-buffer-text*)
(h/get-point*)
```

### Rule 4: Placeholder tests MUST have ^:skip

```clojure
;; WRONG - placeholder without skip tag
(deftest test-future-feature
  (is true "PENDING: needs implementation"))

;; CORRECT - properly marked as skipped
(deftest ^:skip test-future-feature
  (testing "Feature description"
    (is true "PENDING: feature-name - needs implementation")))
```

---

## 3. Test Template

Use this template for new test files:

```clojure
(ns lexicon.ui.<category>.<feature>-test
  "Description of what this test file covers.

  Tests <feature> functionality:
  - Behavior A
  - Behavior B
  - Edge case C

  Related: Issue #N
  Emacs source: lisp/foo.el, src/bar.c"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Feature: <Feature Name>
;; =============================================================================

(deftest test-<feature>-<behavior>
  (testing "User does X and sees Y"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Arrange: Set up initial state
    (h/type-text "initial content")
    (Thread/sleep 100)

    ;; Act: Perform the action being tested
    (h/press-ctrl "k")
    (Thread/sleep 50)

    ;; Assert: Verify expected outcome
    (is (= "" (h/get-buffer-text*))
        "Kill line should remove all text")))
```

**Namespace conventions:**
- File: `e2e_tests/lexicon/ui/editing/undo_test.clj` (underscores)
- Namespace: `lexicon.ui.editing.undo-test` (hyphens)

---

## 4. Helper Function Reference

### Text Input
```clojure
(h/type-text "string")           ;; Type text (auto-detects buffer vs minibuffer)
(h/type-in-minibuffer "text")    ;; Explicitly type in minibuffer
```

### Key Presses
```clojure
(h/press-key "Enter")            ;; Special keys: Enter, Backspace, ArrowLeft, etc.
(h/press-ctrl "x")               ;; Ctrl+key (C-x)
(h/press-meta "x")               ;; Alt/Meta+key (M-x)
(h/press-shift "right")          ;; Shift+key for selection
(h/press-ctrl-x "2")             ;; Two-key sequences (C-x 2)
```

### Minibuffer Operations
```clojure
(h/press-minibuffer-enter)       ;; Confirm minibuffer input
(h/press-arrow-down-in-minibuffer) ;; Navigate completions
(h/press-tab-in-minibuffer)      ;; Trigger completion
(h/run-mx-command "cmd")         ;; Full M-x workflow
```

### Mark/Selection
```clojure
(h/set-mark)                     ;; Set mark with C-SPC
```

### Buffer State (Read-Only)
```clojure
(h/get-buffer-text*)             ;; Current buffer content
(h/get-point*)                   ;; Cursor position (integer)
(h/get-cursor-position)          ;; Cursor as {:row N :col N}
(h/buffer-substring* start end)  ;; Text between positions
```

### Position Predicates
```clojure
(h/bobp*)                        ;; At beginning of buffer?
(h/eobp*)                        ;; At end of buffer?
(h/bolp*)                        ;; At beginning of line?
(h/eolp*)                        ;; At end of line?
(h/char-after*)                  ;; Character at point
(h/char-before*)                 ;; Character before point
```

### UI State
```clojure
(h/get-echo-area-text)           ;; Echo area message
(h/get-minibuffer-text)          ;; Minibuffer prompt
(h/minibuffer-visible?)          ;; Is minibuffer showing?
(h/get-current-buffer-name)      ;; Current buffer name
(h/buffer-exists? "name")        ;; Does buffer exist?
```

### Completions
```clojure
(h/completions-buffer-visible?)  ;; Is *Completions* showing?
(h/get-icomplete-display)        ;; Icomplete inline text
```

### Kill Ring
```clojure
(h/get-current-kill*)            ;; Most recent kill
(h/get-kill-ring-length*)        ;; Kill ring size
```

### Windows
```clojure
(h/get-window-count*)            ;; Number of windows
(h/get-selected-window-id*)      ;; Selected window ID
```

### Debugging
```clojure
(h/get-messages-buffer)          ;; Fetch *Messages* contents
(h/print-messages-buffer)        ;; Print for debugging
```

---

## 5. Async Handling

Always include `Thread/sleep` between operations to allow async propagation:

```clojure
;; Fast operations (keypresses)
(h/press-ctrl "f")
(Thread/sleep 30)

;; Medium operations (text input)
(h/type-text "hello")
(Thread/sleep 100)

;; Slow operations (buffer switch, command execution)
(h/run-mx-command "text-mode")
(Thread/sleep 200)
```

---

## 6. Debugging E2E Tests

When tests fail, use the Messages buffer for debugging:

**In application code (event handlers):**
```clojure
(require '[lexicon.log :as log])
(log/debug "Event called with:" data)
```

**Messages are automatically printed on test failure.**

**Emoji prefixes for scanning:**
- `"🔍 Investigating: ..."` - Investigation
- `"✅ Success: ..."` - Success path
- `"❌ Error: ..."` - Error path
- `"⏪ Undo: ..."` - Undo operations

---

## 7. Pre-Commit Checklist

Before finalizing tests, verify:

- [ ] Tests are in correct folder (`ui/` for keyboard tests, `lisp/` for API tests)
- [ ] No `eval-lisp` or `evalLisp` in `ui/` tests
- [ ] No `e/fill` or `e/fill-active` (use keyboard simulation)
- [ ] Uses proper helpers from `test_helpers.clj`
- [ ] Has descriptive test names (test-<feature>-<behavior>)
- [ ] Has clear assertion messages explaining expected behavior
- [ ] Includes appropriate `Thread/sleep` for async operations
- [ ] Skipped tests have `^:skip` tag and PENDING message
- [ ] Run `bb lint` and fix any violations

---

## 8. Running Tests

```bash
# Run specific test file
bb test:e2e ui.editing.undo-test

# Run tests matching pattern
bb test:e2e "ui.minibuffer.*"

# Run and capture output
bb test:e2e ui.editing.undo-test 2>&1 | tee /tmp/e2e-undo.log

# Run lint checks
bb lint
```

---

## 9. Common Patterns

### Testing keyboard navigation
```clojure
(deftest test-forward-char
  (testing "C-f moves cursor forward"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "hello")
    (h/press-ctrl "a")  ;; Go to beginning
    (Thread/sleep 50)

    (h/press-ctrl "f")
    (Thread/sleep 30)

    (is (= 1 (h/get-point*)) "C-f should move to position 1")))
```

### Testing minibuffer commands
```clojure
(deftest test-mx-command
  (testing "M-x executes command"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (is (h/minibuffer-visible?) "M-x should open minibuffer")

    (h/type-in-minibuffer "text-mode")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Verify command executed
    (let [mode-line (h/get-mode-line-text)]
      (is (str/includes? mode-line "Text")
          "Mode line should show Text mode"))))
```

### Testing undo
```clojure
(deftest test-undo-insert
  (testing "C-/ undoes insertion"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text "hello")
    (Thread/sleep 100)
    (is (= "hello" (h/get-buffer-text*)))

    (h/press-ctrl "/")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Undo should remove inserted text")))
```

### Testing kill/yank
```clojure
(deftest test-kill-line
  (testing "C-k kills to end of line"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text "hello world")
    (h/press-ctrl "a")
    (Thread/sleep 50)

    (h/press-ctrl "k")
    (Thread/sleep 100)

    (is (= "" (h/get-buffer-text*))
        "Kill line should remove all text")
    (is (= "hello world" (h/get-current-kill*))
        "Killed text should be in kill ring")))
```
