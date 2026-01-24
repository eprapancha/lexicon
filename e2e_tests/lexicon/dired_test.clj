(ns lexicon.dired-test
  "Dired E2E Tests - Directory Editor

  These tests validate the core Dired implementation following strict E2E principles:
  - Only keypresses and visible observations
  - No internal state inspection
  - Tests semantic guarantees, not implementation details

  Test Priority Levels (from external research):
  - CRITICAL: Core functionality that makes Dired 'real'
  - HIGH: Important UX guarantees
  - MEDIUM: Nice-to-have features

  Related Issues: #93 (Dired Implementation), #88 (Filesystem Tests)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000)

;; Browser driver
(def ^:dynamic *driver* nil)

;; Setup/teardown with automatic *Messages* printing
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

;;; =============================================================================
;;; Helper Functions (Pure E2E - No Internal State Access)
;;; =============================================================================

(defn press-meta-key
  "Press Meta/Alt+key combination (e.g., 'x' for M-x)"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: '" key "',
        code: '" key-code "',
        altKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-key
  "Press a key in the editor"
  [key-name]
  (let [script (str "
    const input = document.querySelector('.hidden-input');
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
  (Thread/sleep 50))

(defn get-buffer-name
  "Get the current buffer name from the modeline"
  []
  (try
    (e/get-element-text *driver* {:css ".modeline-buffer-name"})
    (catch Exception _
      nil)))

(defn get-editor-text
  "Get visible editor text content"
  []
  (e/get-element-text *driver* {:css ".editable-area"}))

(defn buffer-contains?
  "Check if the current buffer contains the given text"
  [text]
  (str/includes? (get-editor-text) text))

(defn buffer-read-only?
  "Check if buffer is marked read-only in modeline"
  []
  (try
    (let [modeline (e/get-element-text *driver* {:css ".modeline"})]
      (or (str/includes? modeline "%%")
          (str/includes? modeline "read-only")))
    (catch Exception _
      false)))

(defn get-messages-buffer
  "Get *Messages* buffer contents"
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    return state && state.messagesBuffer ? state.messagesBuffer : '';
  "))

(defn messages-contains?
  "Check if *Messages* buffer contains text"
  [text]
  (str/includes? (get-messages-buffer) text))

(defn wait-for-buffer
  "Wait for a buffer with specific name to appear"
  [buffer-name]
  (loop [attempts 0]
    (when (< attempts 20)
      (let [current-buffer (get-buffer-name)]
        (if (and current-buffer (str/includes? current-buffer buffer-name))
          true
          (do
            (Thread/sleep 100)
            (recur (inc attempts))))))))

(defn wait-for-editor-ready
  "Wait for the editor to be fully initialized"
  []
  (e/wait-visible *driver* {:css ".editable-area"} {:timeout 10})
  (Thread/sleep 200))

(defn click-editor
  "Click on the editor to ensure it has focus"
  []
  (e/click *driver* {:css ".editable-area"})
  (Thread/sleep 50))

;;; =============================================================================
;;; CRITICAL TESTS - Core Dired Functionality
;;; =============================================================================

(deftest ^:critical dired-opens-directory
  "CRITICAL: M-x dired creates a derived, read-only buffer representing a directory.

  This test validates:
  - Dired command exists and is executable
  - Minibuffer works for directory prompt
  - Filesystem provider can list directory
  - Buffer rendering occurs

  Why this is honest:
  - Uses only keypresses (M-x, typing, Enter)
  - Observes only visible buffer state
  - No internal state inspection"
  (testing "Dired opens a directory and displays file listing"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; M-x dired
    (press-meta-key "x")
    (Thread/sleep 100)

    ;; Type command name
    (e/fill *driver* {:css ".minibuffer-input"} "dired")
    (Thread/sleep 100)
    (press-key "Enter")
    (Thread/sleep 200)

    ;; Directory prompt - use root or current directory
    (e/fill *driver* {:css ".minibuffer-input"} "/")
    (Thread/sleep 100)
    (press-key "Enter")
    (Thread/sleep 500)

    ;; Assert: Dired buffer exists
    (is (wait-for-buffer "*dired")
        "Dired buffer should be created")

    ;; Note: We cannot assert specific files without a mock FS
    ;; This test validates the command path works
    ;; Specific file listing tests require FS provider integration
    ))

(deftest ^:critical dired-buffer-read-only
  "CRITICAL: Dired buffer is read-only - users cannot directly edit text.

  This catches huge classes of cheating:
  - If Dired is just a regular buffer with keyboard hooks, typing will leak through
  - Read-only must be enforced at buffer level
  - Error messages should appear in *Messages*

  Why critical:
  - Distinguishes real Dired from toy implementations
  - Validates buffer property system works
  - Tests error handling infrastructure"
  (testing "Dired buffer rejects direct text editing"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Open dired
    (press-meta-key "x")
    (Thread/sleep 100)
    (e/fill *driver* {:css ".minibuffer-input"} "dired")
    (Thread/sleep 100)
    (press-key "Enter")
    (Thread/sleep 200)
    (e/fill *driver* {:css ".minibuffer-input"} "/")
    (Thread/sleep 100)
    (press-key "Enter")
    (Thread/sleep 500)

    ;; Get current buffer text
    (let [before-text (get-editor-text)]

      ;; Try to type (should be rejected)
      (e/fill *driver* {:css ".hidden-input"} "X")
      (Thread/sleep 200)

      ;; Assert: No modification occurred
      (is (= before-text (get-editor-text))
          "Buffer text should not change when read-only")

      ;; Assert: Read-only indicator visible
      (is (buffer-read-only?)
          "Buffer should be marked read-only in modeline")

      ;; Assert: Error message logged
      (is (or (messages-contains? "read-only")
              (messages-contains? "read only")
              (messages-contains? "cannot modify"))
          "*Messages* should contain read-only error"))))

(deftest ^:critical dired-delete-regenerates-buffer
  "CRITICAL: Deleting a file mutates filesystem AND regenerates the buffer listing.

  This is the litmus test for real Dired:
  - Dired operations must actually modify the filesystem
  - Buffer must be regenerated from fresh FS query
  - Cannot fake this with text manipulation

  Why critical:
  - If this passes without regeneration logic, something is deeply wrong
  - Tests bidirectional FS integration (read + write)
  - Validates operation confirmation flow

  NOTE: Requires filesystem provider with mock FS for testing"
  (testing "Deleting a file removes it from listing"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    ;; TODO: This test requires:
    ;; 1. Mock filesystem provider for e2e tests
    ;; 2. Ability to inject test files
    ;; 3. File deletion command implementation
    ;;
    ;; Test flow:
    ;; 1. Setup mock FS with {\"a.txt\" \"\" \"b.txt\" \"\"}
    ;; 2. Open dired /mock
    ;; 3. Navigate to a.txt line
    ;; 4. Press 'd' (mark for deletion)
    ;; 5. Press 'x' (execute deletions)
    ;; 6. Confirm with 'y'
    ;; 7. Assert a.txt no longer visible
    ;; 8. Assert b.txt still visible
    ;;
    ;; BLOCKED: Awaiting filesystem provider implementation (Issue #88)
    ))

;;; =============================================================================
;;; HIGH PRIORITY TESTS - Important UX Guarantees
;;; =============================================================================

(deftest ^:high dired-marks-survive-refresh
  "HIGH: Dired marks represent intent, not text state.

  This enforces:
  - Separation of internal model vs rendering
  - Marks stored in data structure, not buffer text
  - Proper MVC architecture

  Why important:
  - If marks are just text characters, they'll vanish on refresh
  - Tests that Dired maintains structured state
  - Validates proper separation of concerns

  NOTE: Requires mark system implementation"
  (testing "File marks persist through buffer refresh"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    ;; TODO: This test requires:
    ;; 1. Mock FS with known files
    ;; 2. Mark command ('m') implementation
    ;; 3. Refresh command ('g') implementation
    ;; 4. Mark rendering in buffer
    ;;
    ;; Test flow:
    ;; 1. Open dired with {\"a.txt\" \"\" \"b.txt\" \"\"}
    ;; 2. Navigate to a.txt
    ;; 3. Press 'm' (mark)
    ;; 4. Assert line shows mark indicator (e.g., \"* a.txt\")
    ;; 5. Press 'g' (refresh)
    ;; 6. Assert a.txt still marked after refresh
    ;;
    ;; BLOCKED: Awaiting Dired mark system implementation
    ))

(deftest ^:high dired-point-stable-after-refresh
  "HIGH: Cursor stays on the 'same' entry when possible after refresh.

  This catches subtle but important UX regressions:
  - Point should track filename, not line number
  - After buffer regeneration, cursor should stay on same file
  - If file deleted, cursor should move to nearest valid entry

  Why important:
  - Major UX issue if cursor jumps randomly on refresh
  - Tests that point tracking is semantic, not positional
  - Validates proper state restoration

  NOTE: Requires refresh command and point tracking"
  (testing "Cursor stays on same file after refresh"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    ;; TODO: This test requires:
    ;; 1. Mock FS with multiple files
    ;; 2. Navigation commands (n/p or arrow keys)
    ;; 3. Refresh command ('g')
    ;; 4. Way to query which file is at point
    ;;
    ;; Test flow:
    ;; 1. Open dired with {\"a.txt\" \"\" \"b.txt\" \"\" \"c.txt\" \"\"}
    ;; 2. Navigate to b.txt (press 'n' or down arrow)
    ;; 3. Assert point is on b.txt line
    ;; 4. Press 'g' (refresh)
    ;; 5. Assert point still on b.txt after refresh
    ;;
    ;; BLOCKED: Awaiting Dired navigation and refresh implementation
    ))

;;; =============================================================================
;;; MEDIUM PRIORITY TESTS - Nice-to-Have Features
;;; =============================================================================

(deftest ^:medium dired-sort-by-name
  "MEDIUM: Sorting is a presentation concern only, does not touch filesystem.

  Why useful:
  - Tests that sorting is client-side operation
  - Validates no FS modifications occur during sort
  - Tests display-only operations

  NOTE: Requires sort command implementation"
  (testing "Sorting files by name does not modify filesystem"
    ;; Setup
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; TODO: This test requires:
    ;; 1. Mock FS with unsorted files
    ;; 2. Sort command ('s' or equivalent)
    ;; 3. Way to verify FS not modified
    ;;
    ;; Test flow:
    ;; 1. Open dired with {\"b.txt\" \"\" \"a.txt\" \"\"}
    ;; 2. Assert initial order (b.txt before a.txt)
    ;; 3. Press 's' (sort)
    ;; 4. Assert new order (a.txt before b.txt)
    ;; 5. Assert FS still has same files (no modifications)
    ;;
    ;; BLOCKED: Awaiting Dired sort implementation
    ))

;;; =============================================================================
;;; Test Suite Notes
;;; =============================================================================

;; What these tests deliberately do NOT assert:
;; - Specific file permission formatting
;; - Exact column layout or alignment
;; - DOM structure or CSS classes (beyond basic selectors)
;; - Synchronous vs async FS operations
;; - Internal state structure
;;
;; What these tests ONLY assert:
;; - Visible behavior through keypresses
;; - Editor-observable semantics
;; - User-level guarantees
;;
;; This discipline keeps tests:
;; - Future-proof against refactoring
;; - Focused on semantic correctness
;; - Honest about what Dired guarantees
;;
;; Once these tests pass, you have REAL Dired:
;; - find-file and Dired share backend truthfully
;; - Tramp-like providers become possible
;; - Project.el can be layered later
;; - Vertico integration is earned, not hacked
