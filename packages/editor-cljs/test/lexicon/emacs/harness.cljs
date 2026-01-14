(ns lexicon.emacs.harness
  "Semantic test harness for Emacs-derived compatibility tests.

  This harness provides editor-facing APIs that mirror Emacs test patterns
  while respecting Lexicon's architecture and the Emacs Compatibility Contract.

  Design principles:
  1. Use ONLY editor-facing APIs (commands, public functions)
  2. Make buffer lifecycle EXPLICIT (create, select, destroy)
  3. Test what packages will actually use
  4. No internal state peeking or shortcuts

  Based on: Phase 6.6 Step 3
  Contract: docs/EMACS_COMPATIBILITY_CONTRACT.md
  Source: /tmp/emacs-source/test/lisp/simple-tests.el (Emacs 29.4)"
  (:require [cljs.test :refer [deftest is testing]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]))

;; =============================================================================
;; Buffer Lifecycle (Explicit and Observable)
;; =============================================================================

(defn- get-wasm-constructor
  "Get WASM constructor from app-db. Internal helper only."
  []
  (get-in @rfdb/app-db [:system :wasm-constructor]))

(defn create-temp-buffer
  "Create a temporary buffer for testing.

  Returns: Buffer ID

  Lifecycle semantics (explicit):
  - Creates new buffer with unique ID
  - Does NOT switch to buffer automatically
  - Buffer persists until destroyed

  Emacs equivalent: (with-temp-buffer ...) creates buffer"
  []
  (let [WasmGapBuffer (get-wasm-constructor)
        _ (when-not WasmGapBuffer
            (throw (js/Error. "WASM not initialized - cannot create buffer")))
        wasm-instance (new WasmGapBuffer "")
        buffer-id (db/next-buffer-id (get-in @rfdb/app-db [:buffers]))]
    (rf/dispatch-sync [:create-buffer "temp-buffer" wasm-instance])
    buffer-id))

(defn destroy-buffer
  "Destroy a buffer, releasing resources.

  Args:
    buffer-id - Buffer to destroy

  Lifecycle semantics:
  - Releases WASM instance
  - Removes from buffer list
  - If buffer was current, clears current buffer

  Emacs equivalent: (with-temp-buffer ...) destroys buffer on exit"
  [buffer-id]
  (rf/dispatch-sync [:kill-buffer buffer-id]))

(defmacro with-temp-buffer
  "Execute body with a temporary buffer as current buffer.

  Lifecycle semantics (EXPLICIT):
  1. Create temporary buffer
  2. Switch to buffer (make current)
  3. Execute body
  4. Destroy buffer (even on error)

  Returns: Result of body

  Emacs equivalent: (with-temp-buffer ...)

  Example:
    (with-temp-buffer
      (insert \"hello\")
      (buffer-string))  ; => \"hello\"

  IMPORTANT: This is NOT just sugar. It encodes buffer lifecycle semantics
  that are part of the Emacs compatibility contract."
  [& body]
  `(let [buffer-id# (create-temp-buffer)]
     (try
       ;; Switch to buffer
       (rf/dispatch-sync [:set-current-buffer buffer-id#])
       ;; Execute body with buffer as current
       (do ~@body)
       (finally
         ;; Always destroy buffer
         (destroy-buffer buffer-id#)))))

;; =============================================================================
;; Buffer Content Operations (Editor-Facing APIs)
;; =============================================================================

(defn insert
  "Insert TEXT at point in current buffer.

  Args:
    text - String to insert

  Side effects:
    - Inserts text at current point
    - Moves point to end of inserted text

  Contract: 4.1 (Buffers), 4.2 (Point)

  Emacs equivalent: (insert text)"
  [text]
  (let [db @rfdb/app-db
        buffer-id (get-in db [:editor :current-buffer-id])
        point (get-in db [:ui :cursor-position] 0)]
    (when-not buffer-id
      (throw (js/Error. "No current buffer")))
    (rf/dispatch-sync [:buffer/insert buffer-id point text])
    ;; Move point to end of inserted text
    (rf/dispatch-sync [:buffer/goto-char buffer-id (+ point (count text))])))

(defn goto-char
  "Move point to POSITION in current buffer.

  Args:
    position - Target position (1-indexed like Emacs, or 0-indexed?)

  Contract: 4.2 (Point)

  Emacs equivalent: (goto-char pos)

  IMPORTANT: Need to verify Lexicon's position semantics (0-indexed vs 1-indexed)"
  [position]
  (let [db @rfdb/app-db
        buffer-id (get-in db [:editor :current-buffer-id])]
    (when-not buffer-id
      (throw (js/Error. "No current buffer")))
    (rf/dispatch-sync [:buffer/goto-char buffer-id position])))

(defn point
  "Return current point position in current buffer.

  Returns: Integer position

  Contract: 4.2 (Point)

  Emacs equivalent: (point)"
  []
  (get-in @rfdb/app-db [:ui :cursor-position] 0))

(defn point-min
  "Return minimum valid point position in current buffer.

  Returns: Integer (usually 1 in Emacs, possibly 0 in Lexicon)

  Contract: 4.2 (Point)

  Emacs equivalent: (point-min)

  IMPORTANT: Verify Lexicon's point-min semantics"
  []
  ;; Assuming 0-indexed for now - may need correction
  0)

(defn point-max
  "Return maximum valid point position in current buffer (end of buffer).

  Returns: Integer position

  Contract: 4.2 (Point), 4.1 (Buffers)

  Emacs equivalent: (point-max)"
  []
  (let [db @rfdb/app-db
        buffer-id (get-in db [:editor :current-buffer-id])
        buffer (get-in db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when-not wasm-instance
      (throw (js/Error. "No current buffer")))
    (.-length (.getText wasm-instance))))

(defn buffer-string
  "Return entire contents of current buffer as string.

  Returns: String

  Contract: 4.1 (Buffers)

  Emacs equivalent: (buffer-string)"
  []
  (let [db @rfdb/app-db
        buffer-id (get-in db [:editor :current-buffer-id])
        buffer (get-in db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when-not wasm-instance
      (throw (js/Error. "No current buffer")))
    (.getText wasm-instance)))

(defn buffer-substring
  "Return text between START and END in current buffer.

  Args:
    start - Start position
    end - End position

  Returns: String

  Contract: 4.1 (Buffers)

  Emacs equivalent: (buffer-substring start end)"
  [start end]
  (let [text (buffer-string)]
    (subs text start end)))

(defn forward-char
  "Move point forward by N characters (default 1).

  Args:
    n - Number of characters to move (default 1)

  Contract: 4.2 (Point)

  Emacs equivalent: (forward-char n)"
  ([]
   (forward-char 1))
  ([n]
   (goto-char (+ (point) n))))

(defn count-lines
  "Count number of lines between START and END positions.

  Args:
    start - Start position
    end - End position

  Returns: Integer count of newline characters

  Contract: 4.1 (Buffers)

  Emacs equivalent: (count-lines start end)

  Implementation: Count newline characters in substring"
  [start end]
  (let [text (buffer-substring start end)
        newlines (filter #(= % \newline) text)]
    (count newlines)))

;; =============================================================================
;; Command Invocation (Interactive Calls)
;; =============================================================================

(defn call-interactively
  "Call COMMAND as if invoked interactively.

  Args:
    command - Keyword command name (e.g., :newline)

  This simulates interactive command invocation, including:
  - Command lifecycle phases
  - Prefix argument handling
  - Dynamic scope setup

  Contract: 2.3 (Command Lifecycle), 4.4 (Commands)

  Emacs equivalent: (call-interactively 'command)

  IMPORTANT: This must invoke the full command machinery, not shortcuts"
  [command]
  ;; For now, dispatch as re-frame event
  ;; TODO: Verify this goes through full command lifecycle
  (rf/dispatch-sync [command]))

(defn set-prefix-arg
  "Set current-prefix-arg for next command (simulates C-u).

  Args:
    arg - Prefix argument value (list or number)

  Contract: 2.4 (Prefix Argument Semantics)

  Emacs test pattern: (let ((current-prefix-arg 5)) (call-interactively ...))"
  [arg]
  (rf/dispatch-sync [:set-prefix-arg arg]))

;; =============================================================================
;; Test Assertion Helpers
;; =============================================================================

(defn buffer-substrings
  "Return cons of buffer text before and after point.

  Returns: [before-point after-point]

  This is a common Emacs test pattern for asserting point position and content.

  Emacs equivalent:
    (cons (buffer-substring (point-min) (point))
          (buffer-substring (point) (point-max)))"
  []
  (let [p (point)]
    [(buffer-substring (point-min) p)
     (buffer-substring p (point-max))]))

;; =============================================================================
;; Commands to Test
;; =============================================================================
;; These will be invoked via call-interactively in tests
;; They should already exist in Lexicon, but we list them here for reference

;; :newline - Insert newline at point
;; :open-line - Insert newline without moving point
;; :delete-indentation - Join current line to previous, removing indentation

;; =============================================================================
;; Missing Command Stubs
;; =============================================================================
;; If commands don't exist yet, we'll discover that when tests fail

(comment
  "Commands expected to exist for our selected tests:

  1. newline (n)
     - Insert n newlines at point
     - Move point after inserted newlines
     - Handle prefix argument

  2. open-line (n)
     - Insert n newlines at point WITHOUT moving point
     - Point stays before inserted newlines

  3. delete-indentation (&optional join-following-p)
     - Join current line to previous line
     - Delete whitespace between them
     - If join-following-p, join to following line instead

  4. count-lines (start end)
     - Count lines between start and end

  These should map to existing Lexicon events or need to be implemented.")
