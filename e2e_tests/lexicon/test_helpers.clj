(ns lexicon.test-helpers
  "Shared E2E test helpers for Lexicon tests.

  Provides utilities for:
  - Auto-printing *Messages* buffer on test failure (Issue #84)
  - Common browser interactions
  - State inspection"
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [cheshire.core :as json]))

;; Forward declarations for functions used before they're defined
(declare press-ctrl-x press-key)

;; =============================================================================
;; Message Buffer Helpers (Issue #84)
;; =============================================================================

(defn get-messages-buffer
  "Fetch *Messages* buffer contents from window.editorState.

  Returns the text from buffer 2 (*Messages*), which contains all logged messages."
  [driver]
  (try
    (e/js-execute driver "
      const state = window.editorState;
      return state && state.messagesBuffer ? state.messagesBuffer : 'No messages logged yet';
    ")
    (catch Exception e
      (str "Failed to fetch *Messages* buffer: " (.getMessage e)))))

(defn print-messages-buffer
  "Print the *Messages* buffer contents to test output.

  Useful for debugging failed tests - shows all logged messages."
  [driver]
  (when-let [messages (get-messages-buffer driver)]
    (println "\n" (str/join "" (repeat 80 "=")) "\n")
    (println "*Messages* buffer contents:")
    (println (str/join "" (repeat 80 "-")))
    (println messages)
    (println (str/join "" (repeat 80 "=")) "\n")))

;; =============================================================================
;; Enhanced Assertions (Issue #84)
;; =============================================================================

(defmacro is-with-messages
  "Enhanced assertion that prints *Messages* buffer on failure.

  Usage:
    (is-with-messages driver (= expected actual) \"assertion message\")

  If the assertion fails, automatically prints the *Messages* buffer
  to help with debugging."
  [driver form & [msg]]
  `(let [result# ~form]
     (when-not result#
       (println "\n⚠️  Assertion failed, printing *Messages* buffer:")
       (print-messages-buffer ~driver))
     (is result# ~msg)))

;; =============================================================================
;; Automatic Test Failure Reporting
;; =============================================================================

;; Track whether we've seen a failure in this test run
(def ^:dynamic *test-failed?* false)

(defn report-with-messages
  "Custom test report function that prints *Messages* buffer on failures.

  Wraps clojure.test's default :fail and :error reporting to automatically
  dump the *Messages* buffer when tests fail."
  [driver original-report]
  (fn [m]
    ;; Call the original reporter first
    (original-report m)

    ;; Track failures and print messages
    (case (:type m)
      :fail
      (do
        (set! *test-failed?* true)
        (println "\n⚠️  Test failed - automatically printing *Messages* buffer:")
        (print-messages-buffer driver))

      :error
      (do
        (set! *test-failed?* true)
        (println "\n⚠️  Test error - automatically printing *Messages* buffer:")
        (print-messages-buffer driver))

      ;; For other event types, do nothing extra
      nil)))

;; =============================================================================
;; Common Test Fixtures
;; =============================================================================

(defn start-driver
  "Start a headless Firefox driver"
  []
  (e/firefox {:headless true}))

(defn stop-driver
  "Stop the driver and clean up"
  [driver]
  (when driver
    (e/quit driver)))

(defn with-driver-and-messages
  "Standard E2E test fixture that:
   - Starts/stops Firefox driver
   - Binds *driver* dynamic var
   - Sets up automatic *Messages* buffer printing on test failure

   Usage in test file:
     (def ^:dynamic *driver* nil)
     (use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))"
  [driver-var f]
  (let [driver (start-driver)
        ;; Save the original reporter before we replace it
        original-report (deref #'clojure.test/report)]
    (try
      ;; Use push-thread-bindings since we're binding a var passed as parameter
      (push-thread-bindings {driver-var driver
                             #'*test-failed?* false
                             #'clojure.test/report (report-with-messages driver original-report)})
      (f)
      (finally
        (pop-thread-bindings)
        (stop-driver driver)))))

;; =============================================================================
;; Common Test Helpers
;; =============================================================================

(defn get-buffer-text
  "Get complete buffer text via window.editorState"
  [driver]
  (e/js-execute driver "
    const state = window.editorState;
    return state ? state.buffer : '';
  "))

(defn get-point
  "Get current point position"
  [driver]
  (e/js-execute driver "
    const state = window.editorState;
    return state ? state.point : 0;
  "))

(defn wait-for-editor-ready
  "Wait for editor to be ready (with configurable timeout)"
  ([driver] (wait-for-editor-ready driver 10))
  ([driver timeout-secs]
   (e/wait-visible driver {:css ".editor-wrapper"} {:timeout timeout-secs})))

(defn click-editor
  "Click the editor to focus it"
  [driver]
  (e/click driver {:css ".editor-wrapper"}))

(defn setup-test
  "Standard test setup: navigate to app and wait for editor"
  [driver app-url]
  (e/go driver app-url)
  (wait-for-editor-ready driver)
  (click-editor driver)
  (Thread/sleep 200))

;; =============================================================================
;; Internal Driver Management (for simplified test API)
;; =============================================================================

(def ^:dynamic *driver*
  "Internal WebDriver instance for simplified API.
  Test files using with-driver fixture don't need their own *driver*."
  nil)

(def default-app-url "http://localhost:8080/index.html")

(defn with-driver
  "Simplified E2E test fixture. Manages driver internally.

   Usage in test file:
     (use-fixtures :once h/with-driver)

   No *driver* var needed in test files."
  [f]
  (let [driver (start-driver)
        original-report (deref #'clojure.test/report)]
    (try
      (binding [*driver* driver
                *test-failed?* false
                clojure.test/report (report-with-messages driver original-report)]
        (f))
      (finally
        (stop-driver driver)))))

;; =============================================================================
;; Simplified Helpers (use internal *driver*)
;; =============================================================================

(defn get-buffer-text*
  "Get buffer text using internal driver."
  []
  (get-buffer-text *driver*))

(defn get-point*
  "Get point using internal driver."
  []
  (get-point *driver*))

(defn setup-test*
  "Standard test setup using internal driver."
  []
  (e/go *driver* default-app-url)
  (wait-for-editor-ready *driver*)
  (click-editor *driver*)
  (Thread/sleep 200))

(defn clear-buffer
  "Clear buffer for a fresh test using keyboard commands.
   Uses C-x h (mark-whole-buffer) then Backspace to delete."
  []
  ;; C-x h: mark-whole-buffer (select all)
  (press-ctrl-x "h")
  (Thread/sleep 100)
  ;; Backspace: delete selection (requires delete-selection-mode)
  (press-key "Backspace")
  (Thread/sleep 100))

;; =============================================================================
;; Keyboard Simulation (exact JS dispatch from basic_editing_test.clj)
;; =============================================================================

(defn type-text
  "Type text by sending keys to the appropriate input element.
   When minibuffer is active (e.g., during isearch), types into #minibuffer-input.
   Otherwise types into .hidden-input for normal buffer editing.
   Carefully manages focus and state to ensure all characters are delivered."
  [text]
  ;; Ensure app is ready after previous operations
  (Thread/sleep 50)
  ;; Check if minibuffer is active (has visible input)
  (let [minibuffer-active? (e/js-execute *driver* "
    const mb = document.querySelector('#minibuffer-input');
    return mb !== null && mb.offsetParent !== null;
  ")
        target-selector (if minibuffer-active? "#minibuffer-input" ".hidden-input")]
    ;; Focus and clear the target input
    (e/js-execute *driver* (str "
      const input = document.querySelector('" target-selector "');
      if (input) {
        input.focus();
        if (input.tagName === 'TEXTAREA') {
          input.value = '';
          input.selectionStart = 0;
          input.selectionEnd = 0;
        }
      }
    "))
    (Thread/sleep 50)
    ;; Type each character with keyboard events
    (doseq [ch text]
      (let [char-str (str ch)
            script (str "
              const input = document.querySelector('" target-selector "');
              if (input) {
                input.focus();
                const char = '" char-str "';
                // Dispatch keydown event
                const keydownEvent = new KeyboardEvent('keydown', {
                  key: char,
                  code: 'Key' + char.toUpperCase(),
                  bubbles: true,
                  cancelable: true
                });
                input.dispatchEvent(keydownEvent);
                // Update input value and dispatch input event
                // Choose setter based on actual element type (hidden-input is a textarea)
                const isTextArea = input.tagName === 'TEXTAREA';
                const prototype = isTextArea ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
                const nativeInputValueSetter = Object.getOwnPropertyDescriptor(prototype, 'value').set;
                if (nativeInputValueSetter) {
                  nativeInputValueSetter.call(input, input.value + char);
                } else {
                  input.value = input.value + char;
                }
                input.dispatchEvent(new Event('input', {bubbles: true}));
                // Dispatch keyup event
                const keyupEvent = new KeyboardEvent('keyup', {
                  key: char,
                  code: 'Key' + char.toUpperCase(),
                  bubbles: true,
                  cancelable: true
                });
                input.dispatchEvent(keyupEvent);
              }
            ")]
        (e/js-execute *driver* script)
        (Thread/sleep 20)))))

(defn press-key
  "Press a special key (Enter, Backspace, ArrowLeft, etc.)
   When minibuffer is active, dispatches to #minibuffer-input.
   Otherwise dispatches to .hidden-input for normal buffer editing."
  [key-name]
  ;; Check if minibuffer is active
  (let [minibuffer-active? (e/js-execute *driver* "
    const mb = document.querySelector('#minibuffer-input');
    return mb !== null && mb.offsetParent !== null;
  ")
        target-selector (if minibuffer-active? "#minibuffer-input" ".hidden-input")
        script (str "
    const input = document.querySelector('" target-selector "');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: '" key-name "',
        code: '" key-name "',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-ctrl
  "Press Ctrl+key combination (e.g., 'f' for C-f)"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-meta
  "Press Meta/Alt+key combination (e.g., 'f' for M-f)"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-shift
  "Press Shift+key for selection"
  [key]
  (let [key-name (if (keyword? key) (name key) key)
        key-code (case key-name
                   "right" "ArrowRight"
                   "left" "ArrowLeft"
                   "up" "ArrowUp"
                   "down" "ArrowDown"
                   (str "Key" (str/upper-case key-name)))
        actual-key (case key-name
                     "right" "ArrowRight"
                     "left" "ArrowLeft"
                     "up" "ArrowUp"
                     "down" "ArrowDown"
                     key-name)
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" actual-key "',
      code: '" key-code "',
      shiftKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-ctrl-x
  "Press C-x prefix followed by another key (e.g., C-x C-s, C-x 2)"
  [key]
  (press-ctrl "x")
  (Thread/sleep 50)
  (if (= (count key) 1)
    ;; Single char like "2", "3", "o"
    (let [script (str "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: '" key "',
        code: 'Key" (str/upper-case key) "',
        bubbles: true
      });
      input.dispatchEvent(event);
    ")]
      (e/js-execute *driver* script)
      (Thread/sleep 10))
    ;; Special key like "Enter"
    (press-key key)))

(defn press-minibuffer-enter
  "Press Enter in the minibuffer"
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn get-echo-area-text
  "Get text from the echo area"
  []
  (try
    (e/get-element-text *driver* {:css ".echo-area"})
    (catch Exception _ "")))

(defn type-in-minibuffer
  "Type text into the minibuffer input using keyboard simulation"
  [text]
  ;; Type each character with keyboard events
  (doseq [ch text]
    (let [char-str (str ch)
          script (str "
            const input = document.querySelector('.minibuffer-input');
            if (input) {
              input.focus();
              const char = '" char-str "';
              // Dispatch keydown event
              const keydownEvent = new KeyboardEvent('keydown', {
                key: char,
                code: 'Key' + char.toUpperCase(),
                bubbles: true,
                cancelable: true
              });
              input.dispatchEvent(keydownEvent);
              // Update input value and dispatch input event
              // Choose setter based on actual element type
              const isTextArea = input.tagName === 'TEXTAREA';
              const prototype = isTextArea ? HTMLTextAreaElement.prototype : HTMLInputElement.prototype;
              const nativeInputValueSetter = Object.getOwnPropertyDescriptor(prototype, 'value').set;
              if (nativeInputValueSetter) {
                nativeInputValueSetter.call(input, input.value + char);
              } else {
                input.value = input.value + char;
              }
              input.dispatchEvent(new Event('input', {bubbles: true}));
              // Dispatch keyup event
              const keyupEvent = new KeyboardEvent('keyup', {
                key: char,
                code: 'Key' + char.toUpperCase(),
                bubbles: true,
                cancelable: true
              });
              input.dispatchEvent(keyupEvent);
            }
          ")]
      (e/js-execute *driver* script)
      (Thread/sleep 20)))
  (Thread/sleep 50))

(defn get-minibuffer-text
  "Get text from the minibuffer prompt"
  []
  (try
    (e/get-element-text *driver* {:css ".minibuffer-prompt"})
    (catch Exception _ "")))

(defn minibuffer-visible?
  "Check if minibuffer input is visible"
  []
  (try
    (e/visible? *driver* {:css ".minibuffer-input"})
    (catch Exception _ false)))

(defn set-mark
  "Set mark with C-SPC"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: ' ',
      code: 'Space',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 20))

(defn execute-command
  "Execute a command via M-x (e.g., 'line-number-mode')"
  [command-name]
  (press-meta "x")
  (Thread/sleep 100)
  (type-in-minibuffer command-name)
  (Thread/sleep 100)  ;; Wait for typing to complete and completion to update
  (press-minibuffer-enter)
  (Thread/sleep 200))

(def run-mx-command
  "Alias for execute-command"
  execute-command)

(defn get-cursor-position
  "Get current cursor position as {:row N :col N}"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return null;
    const point = state.point;
    const buffer = state.buffer;
    let row = 0;
    let col = 0;
    let pos = 0;
    const lines = buffer.split('\\n');
    for (let i = 0; i < lines.length; i++) {
      if (pos + lines[i].length >= point) {
        row = i;
        col = point - pos;
        break;
      }
      pos += lines[i].length + 1;
    }
    return {row: row, col: col};
  "]
    (e/js-execute *driver* script)))

;; =============================================================================
;; Buffer Predicates (for state verification)
;; =============================================================================

(defn bobp*
  "Beginning of buffer predicate - true if point is at position 0"
  []
  (= 0 (get-point*)))

(defn eobp*
  "End of buffer predicate - true if point is at buffer end"
  []
  (let [point (get-point*)
        buffer (get-buffer-text*)]
    (= point (count buffer))))

(defn bolp*
  "Beginning of line predicate - true if point is at line start"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return true;
    const point = state.point;
    const buffer = state.buffer;
    return point === 0 || buffer[point - 1] === '\\n';
  "]
    (e/js-execute *driver* script)))

(defn eolp*
  "End of line predicate - true if point is at line end"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return true;
    const point = state.point;
    const buffer = state.buffer;
    return point >= buffer.length || buffer[point] === '\\n';
  "]
    (e/js-execute *driver* script)))

(defn char-after*
  "Get character after point, or nil if at buffer end"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return null;
    const point = state.point;
    const buffer = state.buffer;
    return point < buffer.length ? buffer[point] : null;
  "]
    (e/js-execute *driver* script)))

(defn char-before*
  "Get character before point, or nil if at buffer start"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return null;
    const point = state.point;
    const buffer = state.buffer;
    return point > 0 ? buffer[point - 1] : null;
  "]
    (e/js-execute *driver* script)))

(defn line-beginning-position*
  "Get position of current line start"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return 0;
    const point = state.point;
    const buffer = state.buffer;
    let pos = point;
    while (pos > 0 && buffer[pos - 1] !== '\\n') {
      pos--;
    }
    return pos;
  "]
    (e/js-execute *driver* script)))

(defn line-end-position*
  "Get position of current line end"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return 0;
    const point = state.point;
    const buffer = state.buffer;
    let pos = point;
    while (pos < buffer.length && buffer[pos] !== '\\n') {
      pos++;
    }
    return pos;
  "]
    (e/js-execute *driver* script)))

(defn buffer-substring*
  "Get substring from buffer between start and end positions"
  [start end]
  (let [script (str "
    const state = window.editorState;
    if (!state) return '';
    return state.buffer.substring(" start ", " end ");
  ")]
    (e/js-execute *driver* script)))

(defn region-beginning*
  "Get region start position (mark or point, whichever is smaller)"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return 0;
    const mark = state.mark;
    const point = state.point;
    if (mark === null || mark === undefined) return point;
    return Math.min(mark, point);
  "]
    (e/js-execute *driver* script)))

(defn region-end*
  "Get region end position (mark or point, whichever is larger)"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return 0;
    const mark = state.mark;
    const point = state.point;
    if (mark === null || mark === undefined) return point;
    return Math.max(mark, point);
  "]
    (e/js-execute *driver* script)))

;; =============================================================================
;; Window Helpers (for state verification)
;; =============================================================================

(defn get-window-count*
  "Get number of windows"
  []
  (let [script "
    const state = window.editorState;
    if (!state || !state.windowList) return 1;
    return state.windowList.length;
  "]
    (e/js-execute *driver* script)))

(defn get-selected-window-id*
  "Get ID of currently selected window"
  []
  (let [script "
    const state = window.editorState;
    if (!state) return null;
    return state.selectedWindow || null;
  "]
    (e/js-execute *driver* script)))

(defn press-ctrl-x
  "Press C-x followed by another key (Emacs prefix command)"
  [key]
  (press-ctrl "x")
  (Thread/sleep 50)
  (press-key key))

(defn get-current-kill*
  "Get the most recent kill from kill ring"
  []
  (let [script "
    const state = window.editorState;
    if (!state || !state.killRing || state.killRing.length === 0) return null;
    return state.killRing[0];
  "]
    (e/js-execute *driver* script)))

(defn get-kill-ring-length*
  "Get the number of entries in kill ring"
  []
  (let [script "
    const state = window.editorState;
    if (!state || !state.killRing) return 0;
    return state.killRing.length;
  "]
    (e/js-execute *driver* script)))

;; =============================================================================
;; Minibuffer Completion Helpers (Issue #137)
;; =============================================================================

(defn press-arrow-down-in-minibuffer
  "Press ArrowDown in the minibuffer to cycle to next completion"
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowDown',
        code: 'ArrowDown',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-arrow-up-in-minibuffer
  "Press ArrowUp in the minibuffer to cycle to previous completion"
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowUp',
        code: 'ArrowUp',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-tab-in-minibuffer
  "Press Tab in the minibuffer for completion"
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Tab',
        code: 'Tab',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn get-minibuffer-input-value
  "Get the current value in the minibuffer input field"
  []
  (try
    (e/get-element-value *driver* {:css ".minibuffer-input"})
    (catch Exception _ "")))

(defn completions-buffer-visible?
  "Check if *Completions* buffer window is visible"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return false;
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '*Completions*') return true;
      }
      return false;
    "]
      (e/js-execute *driver* script))
    (catch Exception _ false)))

(defn get-window-count
  "Get the number of visible windows"
  []
  (try
    (let [script "
      const windows = document.querySelectorAll('.window-pane');
      return windows.length || 1;
    "]
      (e/js-execute *driver* script))
    (catch Exception _ 1)))

(defn get-current-buffer-name
  "Get the current buffer name from editorState"
  []
  (try
    (e/js-execute *driver* "
      const state = window.editorState;
      if (state && state.bufferName) return state.bufferName;
      return '';
    ")
    (catch Exception _ "")))

(defn get-minibuffer-prompt
  "Get the minibuffer prompt text (alias for get-minibuffer-text)"
  []
  (get-minibuffer-text))

(defn get-icomplete-display
  "Get the icomplete display text (inline completion candidates).
   Returns the text showing candidates like '{candidate1 | candidate2}'
   or nil if icomplete is not displaying anything."
  []
  (try
    (let [script "
      const display = document.querySelector('.icomplete-display');
      return display ? display.textContent : null;
    "]
      (e/js-execute *driver* script))
    (catch Exception _ nil)))

(defn get-icomplete-state
  "Get icomplete state from editorState for debugging.
   Returns a map with :enabled, :index, and :cachedCount."
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.icomplete) return null;
      return JSON.stringify({
        enabled: state.icomplete['enabled?'],
        index: state.icomplete.index || 0,
        cachedCount: state.icomplete.cachedCompletionsCount || 0,
        lastInput: state.icomplete.lastInput || null
      });
    "
          result (e/js-execute *driver* script)]
      (when result
        (json/parse-string result true)))
    (catch Exception _ nil)))

(defn get-minibuffer-full-content
  "Get the full minibuffer line content including prompt, input, and icomplete display."
  []
  (try
    (let [script "
      const line = document.querySelector('.minibuffer-input-line');
      return line ? line.textContent : '';
    "]
      (e/js-execute *driver* script))
    (catch Exception _ "")))

(defn clear-minibuffer-input
  "Clear the minibuffer input field"
  []
  (try
    (e/js-execute *driver* "
      const input = document.querySelector('.minibuffer-input');
      if (input) {
        input.value = '';
        input.dispatchEvent(new Event('input', { bubbles: true }));
      }
    ")
    (Thread/sleep 50)
    (catch Exception _ nil)))

(defn get-occur-highlights
  "Get occur highlight data from the current *Occur* buffer.
   Returns a vector of highlight maps with :line and :ranges.
   Each range is [start-col end-col] in the occur buffer.
   Returns nil if not in an *Occur* buffer or no highlights."
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return null;
      // Find the *Occur* buffer
      for (const [id, buf] of Object.entries(state.buffers)) {
        if (buf.name === '*Occur*' && buf.occurHighlights) {
          return JSON.stringify(buf.occurHighlights);
        }
      }
      return null;
    "
          result (e/js-execute *driver* script)]
      (when result
        (json/parse-string result true)))
    (catch Exception _ nil)))

(defn get-occur-match-elements
  "Get all DOM elements with the 'occur-match' CSS class.
   Returns a vector of maps with :text (the element's text content).
   This tests the rendering layer to verify CSS classes are applied."
  []
  (try
    (let [script "
      const elements = document.querySelectorAll('.occur-match');
      return Array.from(elements).map(el => ({
        text: el.textContent,
        tagName: el.tagName.toLowerCase()
      }));
    "
          result (e/js-execute *driver* script)]
      (vec result))
    (catch Exception _ [])))

(defn buffer-exists?
  "Check if a buffer with the given name exists.
   Returns true if the buffer exists, false otherwise."
  [buffer-name]
  (try
    (let [script (str "
      const state = window.editorState;
      if (!state || !state.buffers) return false;
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '" buffer-name "') return true;
      }
      return false;
    ")]
      (e/js-execute *driver* script))
    (catch Exception _ false)))
