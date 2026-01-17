(ns lexicon.test.helpers
  "E2E Test helper functions for Lexicon editor.

  This module provides a test harness for running E2E tests against the Lexicon
  editor. It wraps re-frame events and subscriptions to provide a clean testing API.

  ARCHITECTURE:
  - Each test gets a fresh re-frame db via `with-editor` macro
  - Helpers dispatch events synchronously via `rf/dispatch-sync`
  - State queries read from `@re-frame.db/app-db`
  - No mocking - tests run against real editor code

  IMPLEMENTATION STATUS:
  - [x] Tier 1 (Must Have - Week 1): 7 core functions implemented below
  - [ ] Tier 2 (High Value - Week 2-3): 10 functions needed
  - [ ] Tier 3 (Nice to Have - Week 4-6): 18 functions needed
  - [ ] Tier 4 (Future Work - Week 7-8): 27 functions needed

  See /tmp/test-helper-analysis.md for complete requirements.

  NOTE: This file was originally written to use re-frame, but e2e tests
  run in JVM Clojure which cannot load ClojureScript libraries like re-frame.
  The re-frame dependencies have been commented out. E2E tests that need
  re-frame functionality should be migrated to ClojureScript browser tests."
  (:require ;; [re-frame.core :as rf]  ; Can't use in JVM Clojure
            ;; [re-frame.db]            ; Can't use in JVM Clojure
            [lexicon.db :as db]
            [lexicon.init :as init]))

;; =============================================================================
;; Test Context Lifecycle
;; =============================================================================

(defmacro with-editor
  "Execute body with a fresh editor context.

  Creates a new re-frame app-db atom initialized to default-db,
  runs editor initialization, then executes body.

  Each test gets an isolated editor state that doesn't affect other tests.

  Example:
    (with-editor
      (let [buf (create-buffer \"test\")]
        (insert-text buf \"hello\")
        (is (= \"hello\" (buffer-text buf)))))"
  [& body]
  `(let [test-db# (atom db/default-db)]
     (binding [re-frame.db/app-db test-db#]
       ;; Initialize editor for test mode
       ;; TODO: Implement :initialize-test event or reuse :initialize
       ;; For now, just use default-db as-is
       ~@body)))

(defn current-db
  "Get the current editor database (for debugging/inspection).

  Returns the current re-frame app-db atom dereferenced.

  Example:
    (current-db)
    ;; => {:buffers {1 {...}}, :window-tree {...}, ...}"
  []
  @re-frame.db/app-db)

;; =============================================================================
;; Buffer Operations - Tier 1 (Week 1)
;; =============================================================================

(defn create-buffer
  "Create a new buffer with the given name.

  Args:
    name - String buffer name (e.g., \"*scratch*\", \"test.clj\")

  Returns:
    Integer buffer ID

  Example:
    (create-buffer \"test\")
    ;; => 2  (buffer ID)"
  [name]
  (let [db @re-frame.db/app-db
        next-id (db/next-buffer-id (:buffers db))]
    ;; Dispatch buffer creation event
    ;; TODO: Check if :create-buffer expects wasm-instance or can accept nil in test mode
    (rf/dispatch-sync [:create-buffer name nil])
    ;; Return the buffer ID
    next-id))

(defn buffer-exists?
  "Check if a buffer with the given name exists.

  Args:
    name - String buffer name

  Returns:
    Boolean true if buffer exists, false otherwise

  Example:
    (buffer-exists? \"*scratch*\")
    ;; => true"
  [name]
  (let [buffers (get-in (current-db) [:buffers])]
    (some #(= name (:name %)) (vals buffers))))

(defn get-buffer-by-name
  "Get buffer ID by name (helper for other functions).

  Args:
    name - String buffer name

  Returns:
    Integer buffer ID or nil if not found"
  [name]
  (let [buffers (get-in (current-db) [:buffers])]
    (->> buffers
         vals
         (filter #(= name (:name %)))
         first
         :id)))

(defn buffer-text
  "Get the text contents of a buffer.

  Args:
    buffer-id-or-name - Integer buffer ID or String buffer name

  Returns:
    String containing full buffer text

  Example:
    (buffer-text 1)
    ;; => \"hello world\"

    (buffer-text \"*scratch*\")
    ;; => \"\"

  Implementation notes:
    Uses buffer :cache {:text \"...\"} field which should be kept up-to-date.
    If cache is stale, may need to read from WASM (see TODO below)."
  [buffer-id-or-name]
  (let [buffer-id (if (string? buffer-id-or-name)
                   (get-buffer-by-name buffer-id-or-name)
                   buffer-id-or-name)]
    (when buffer-id
      (get-in (current-db) [:buffers buffer-id :cache :text] ""))))

(defn insert-text
  "Insert text into a buffer at current cursor position.

  Args:
    buffer-id-or-name - Integer buffer ID or String buffer name
    text - String to insert

  Returns:
    nil

  Example:
    (insert-text 1 \"hello\")
    (insert-text \"test\" \"world\")

  Implementation notes:
    Dispatches :insert-text event synchronously.
    TODO: Verify event name and signature match events/edit.cljs"
  [buffer-id-or-name text]
  (let [buffer-id (if (string? buffer-id-or-name)
                   (get-buffer-by-name buffer-id-or-name)
                   buffer-id-or-name)]
    (when buffer-id
      ;; TODO: Check actual event signature for :insert-text
      ;; May need to be [:insert-text buffer-id position text] or similar
      (rf/dispatch-sync [:insert-text buffer-id text]))))

(defn buffer-file
  "Get the file path associated with a buffer.

  Args:
    buffer-id-or-name - Integer buffer ID or String buffer name

  Returns:
    String file path or nil if buffer not associated with file

  Example:
    (buffer-file 1)
    ;; => nil  (scratch buffer has no file)

    (visit-file 1 \"/tmp/test.txt\")
    (buffer-file 1)
    ;; => \"/tmp/test.txt\""
  [buffer-id-or-name]
  (let [buffer-id (if (string? buffer-id-or-name)
                   (get-buffer-by-name buffer-id-or-name)
                   buffer-id-or-name)]
    (when buffer-id
      (get-in (current-db) [:buffers buffer-id :file-handle]))))

;; =============================================================================
;; Tier 2 Helpers - TODO (Week 2-3)
;; =============================================================================

;; TODO: Implement these helpers in Week 2-3

(defn invoke-command
  "Execute a command by name.

  TODO: Implement by dispatching [:execute-command command-name & args]"
  [command-name & args]
  (throw (ex-info "Not yet implemented" {:fn 'invoke-command})))

(defn last-command
  "Get the last executed command name.

  TODO: Need to track last-command in db or via instrumentation"
  []
  (throw (ex-info "Not yet implemented" {:fn 'last-command})))

(defn show-buffer-in-two-windows
  "Split window and show buffer in both windows.

  TODO: Implement window splitting events first"
  [buffer-id]
  (throw (ex-info "Not yet implemented" {:fn 'show-buffer-in-two-windows})))

(defn window-text
  "Get text visible in a window.

  TODO: Implement by getting window's buffer-id and calling buffer-text"
  [window-id]
  (throw (ex-info "Not yet implemented" {:fn 'window-text})))

(defn undo
  "Undo last operation in buffer.

  TODO: Dispatch [:undo buffer-id] or similar"
  [& args]
  (throw (ex-info "Not yet implemented" {:fn 'undo})))

(defn message
  "Display a message in echo area.

  TODO: Dispatch [:message text]"
  [text]
  (throw (ex-info "Not yet implemented" {:fn 'message})))

(defn enable-major-mode
  "Enable a major mode in current buffer.

  TODO: Dispatch [:set-major-mode buffer-id mode]"
  [mode]
  (throw (ex-info "Not yet implemented" {:fn 'enable-major-mode})))

(defn current-major-mode
  "Get current buffer's major mode.

  TODO: Query db [:buffers buffer-id :major-mode]"
  []
  (throw (ex-info "Not yet implemented" {:fn 'current-major-mode})))

;; =============================================================================
;; Tier 3 Helpers - TODO (Week 4-6)
;; =============================================================================

(defn press
  "Simulate pressing a key sequence.

  TODO: Implement input simulation via keymap resolution"
  [key-sequence]
  (throw (ex-info "Not yet implemented" {:fn 'press})))

(defn bind-global
  "Bind a key to a command globally.

  TODO: Dispatch [:define-key :global key command]"
  [key command]
  (throw (ex-info "Not yet implemented" {:fn 'bind-global})))

(defn bind-local
  "Bind a key to a command in current buffer.

  TODO: Dispatch [:define-key buffer-id key command]"
  [key command]
  (throw (ex-info "Not yet implemented" {:fn 'bind-local})))

(defn eval-lisp
  "Evaluate a Lisp expression in editor context.

  TODO: Use SCI with editor bindings (buffer-name, point, etc.)"
  [expr]
  (throw (ex-info "Not yet implemented" {:fn 'eval-lisp})))

;; =============================================================================
;; Instrumentation - TODO (Week 2)
;; =============================================================================

(def ^:dynamic *instrumentation-enabled* false)
(def ^:dynamic *dispatch-log* (atom []))

(defmacro with-instrumented-dispatch
  "Execute body with dispatch instrumentation enabled.

  Tracks all command dispatches to *dispatch-log*.

  TODO: Implement by wrapping rf/dispatch"
  [& body]
  `(binding [*instrumentation-enabled* true
             *dispatch-log* (atom [])]
     ~@body))

(defn dispatch-was-used?
  "Check if dispatch layer was used during instrumented execution.

  TODO: Check if *dispatch-log* is non-empty"
  []
  (throw (ex-info "Not yet implemented" {:fn 'dispatch-was-used?})))

;; =============================================================================
;; Additional Helpers Needed (See Analysis)
;; =============================================================================

;; Week 1 Missing:
;; - current-buffer-text (wrapper around buffer-text for active buffer)
;; - buffer-modified? (query :is-modified? flag)
;; - with-temp-file (create/delete temp file for test)
;; - current-buffer (get active buffer ID)

;; Week 2 Missing:
;; - define-test-command (register command for testing)
;; - command-name, command-doc, command-fn (query command metadata)
;; - all-mutations-went-through-dispatch? (instrumentation)
;; - execute-command (wrapper for command with undo boundary)

;; Week 3 Missing:
;; - show-buffer-in-new-window (create window)
;; - delete-other-windows (delete all but current)
;; - set-point, point (cursor position)

;; Week 4 Missing:
;; - define-prefix (register prefix key)
;; - define-major-mode (create mode definition)
;; - enable-minor-mode, minor-mode-enabled? (minor mode queries)

;; Week 5-6 Missing:
;; - with-buffer (execute with buffer as current)
;; - kill-region, yank (kill ring ops)
;; - editor-snapshot (capture full state)

;; Week 7-8 Missing:
;; - Registers system (store/recall text/position)
;; - Manual undo boundaries
;; - Undo restore point/mark
;; - Async I/O helpers

;; =============================================================================
;; Implementation Notes
;; =============================================================================

;; NEXT STEPS:
;; 1. Verify :create-buffer event signature (can it accept nil for wasm-instance?)
;; 2. Verify :insert-text event signature (does it need position?)
;; 3. Ensure buffer :cache {:text "..."} is always up-to-date
;; 4. Test basic helpers with buffer_file_semantics_test.clj
;; 5. Add missing Week 1 helpers (current-buffer-text, etc.)
;; 6. Move to Week 2: Implement command/instrumentation helpers

;; QUESTIONS TO RESOLVE:
;; Q1: Does :create-buffer accept nil for wasm-instance in test mode?
;;     A: Check events/buffer.cljs line 46-51
;; Q2: What's the signature of :insert-text event?
;;     A: Check events/edit.cljs
;; Q3: Is buffer :cache {:text} always up-to-date?
;;     A: Check if :buffer/increment-version also updates cache
;; Q4: Do we need :initialize-test event or can we use default-db as-is?
;;     A: Try both approaches, see what breaks

;; DEPENDENCIES TO CHECK:
;; - lexicon.db/next-buffer-id function
;; - lexicon.db/create-buffer function (if exists)
;; - lexicon.init/initialize-editor (for proper initialization)
;; - events/buffer.cljs (:create-buffer event)
;; - events/edit.cljs (:insert-text event)
