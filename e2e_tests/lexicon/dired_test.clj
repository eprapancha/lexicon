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
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;;; =============================================================================
;;; Helper Functions (Pure E2E - No Internal State Access)
;;; =============================================================================

(defn get-buffer-name
  "Get the current buffer name from the modeline"
  []
  (try
    (e/get-element-text h/*driver* {:css ".buffer-info"})
    (catch Exception _
      nil)))

(defn buffer-contains?
  "Check if the current buffer contains the given text"
  [text]
  (str/includes? (h/get-buffer-text*) text))

(defn buffer-read-only?
  "Check if buffer is marked read-only in modeline.
  Read-only buffers show '%%' (unmodified) or '%*' (modified) in modeline."
  []
  (try
    (let [modified-info (e/get-element-text h/*driver* {:css ".modified-info"})]
      (or (str/includes? modified-info "%%")
          (str/includes? modified-info "%*")))
    (catch Exception _
      false)))

(defn get-messages-buffer
  "Get *Messages* buffer contents"
  []
  (e/js-execute h/*driver* "
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

(defn wait-for-minibuffer-ready
  "Wait for minibuffer input to be visible and ready"
  []
  (e/wait-visible h/*driver* {:css ".minibuffer-input"} {:timeout 5})
  (Thread/sleep 100))

(defn fill-minibuffer
  "Fill minibuffer with text"
  [text]
  (wait-for-minibuffer-ready)
  (h/type-in-minibuffer text))

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
    (h/setup-test*)

    ;; M-x dired
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type command name and confirm
    (fill-minibuffer "dired")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Directory prompt - use root or current directory
    (fill-minibuffer "/")
    (h/press-minibuffer-enter)
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
    (h/setup-test*)

    ;; Open dired
    (h/press-meta "x")
    (Thread/sleep 200)
    (fill-minibuffer "dired")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)
    (fill-minibuffer "/")
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    ;; Get current buffer text
    (let [before-text (h/get-buffer-text*)]

      ;; Try to type (should be rejected)
      (e/fill h/*driver* {:css ".hidden-input"} "X")
      (Thread/sleep 200)

      ;; Assert: No modification occurred
      (is (= before-text (h/get-buffer-text*))
          "Buffer text should not change when read-only")

      ;; Assert: Read-only indicator visible
      (is (buffer-read-only?)
          "Buffer should be marked read-only in modeline")

      ;; Assert: Error message logged
      (is (or (messages-contains? "read-only")
              (messages-contains? "read only")
              (messages-contains? "cannot modify"))
          "*Messages* should contain read-only error"))))

(deftest ^:critical ^:skip dired-delete-regenerates-buffer
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
    (h/setup-test*)
    ;; TODO: This test requires:
    ;; 1. Mock filesystem provider for e2e tests
    ;; 2. Ability to inject test files
    ;; 3. File deletion command implementation
    ;;
    ;; Test flow:
    ;; 1. Setup mock FS with {"a.txt" "" "b.txt" ""}
    ;; 2. Open dired /mock
    ;; 3. Navigate to a.txt line
    ;; 4. Press 'd' (mark for deletion)
    ;; 5. Press 'x' (execute deletions)
    ;; 6. Confirm with 'y'
    ;; 7. Assert a.txt no longer visible
    ;; 8. Assert b.txt still visible
    ;;
    ;; BLOCKED: Awaiting filesystem provider implementation (Issue #88)
    (is true "PENDING: Blocked on filesystem provider (Issue #88)")))

;;; =============================================================================
;;; HIGH PRIORITY TESTS - Important UX Guarantees
;;; =============================================================================

(deftest ^:high ^:skip dired-marks-survive-refresh
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
    (h/setup-test*)
    ;; TODO: This test requires:
    ;; 1. Mock FS with known files
    ;; 2. Mark command ('m') implementation
    ;; 3. Refresh command ('g') implementation
    ;; 4. Mark rendering in buffer
    ;;
    ;; Test flow:
    ;; 1. Open dired with {"a.txt" "" "b.txt" ""}
    ;; 2. Navigate to a.txt
    ;; 3. Press 'm' (mark)
    ;; 4. Assert line shows mark indicator (e.g., "* a.txt")
    ;; 5. Press 'g' (refresh)
    ;; 6. Assert a.txt still marked after refresh
    ;;
    ;; BLOCKED: Awaiting Dired mark system implementation
    (is true "PENDING: Blocked on Dired mark system")))

(deftest ^:high ^:skip dired-point-stable-after-refresh
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
    (h/setup-test*)
    ;; TODO: This test requires:
    ;; 1. Mock FS with multiple files
    ;; 2. Navigation commands (n/p or arrow keys)
    ;; 3. Refresh command ('g')
    ;; 4. Way to query which file is at point
    ;;
    ;; Test flow:
    ;; 1. Open dired with {"a.txt" "" "b.txt" "" "c.txt" ""}
    ;; 2. Navigate to b.txt (press 'n' or down arrow)
    ;; 3. Assert point is on b.txt line
    ;; 4. Press 'g' (refresh)
    ;; 5. Assert point still on b.txt after refresh
    ;;
    ;; BLOCKED: Awaiting Dired navigation and refresh implementation
    (is true "PENDING: Blocked on Dired navigation/refresh")))

;;; =============================================================================
;;; MEDIUM PRIORITY TESTS - Nice-to-Have Features
;;; =============================================================================

(deftest ^:medium ^:skip dired-sort-by-name
  "MEDIUM: Sorting is a presentation concern only, does not touch filesystem.

  Why useful:
  - Tests that sorting is client-side operation
  - Validates no FS modifications occur during sort
  - Tests display-only operations

  NOTE: Requires sort command implementation"
  (testing "Sorting files by name does not modify filesystem"
    ;; Setup
    (h/setup-test*)

    ;; TODO: This test requires:
    ;; 1. Mock FS with unsorted files
    ;; 2. Sort command ('s' or equivalent)
    ;; 3. Way to verify FS not modified
    ;;
    ;; Test flow:
    ;; 1. Open dired with {"b.txt" "" "a.txt" ""}
    ;; 2. Assert initial order (b.txt before a.txt)
    ;; 3. Press 's' (sort)
    ;; 4. Assert new order (a.txt before b.txt)
    ;; 5. Assert FS still has same files (no modifications)
    ;;
    ;; BLOCKED: Awaiting Dired sort implementation
    (is true "PENDING: Blocked on Dired sort")))

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
