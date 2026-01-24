(ns lexicon.semantic.inhibit-read-only-test
  "Semantic tests for inhibit-read-only - CORE primitive for mode self-management.

  Read-only buffers block user edits but modes must update their own content.
  Used by: Dired, Help, Info, Magit, all special buffers.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - without this, no mode can manage read-only buffers"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Read-Only and Inhibit-Read-Only - Core Primitive Tests
;;; =============================================================================

(deftest ^:critical read-only-blocks-user-edits
  "CRITICAL: Read-only buffers reject normal insert/delete operations.

  Semantic Guarantee:
  - (set-buffer-read-only t) makes buffer read-only
  - User commands like insert, delete throw error
  - Cursor movement still works
  - Read-only is for protection, not prevention

  Why this matters:
  - Help buffers shouldn't be modified by user
  - Dired listing is generated, not edited
  - Protect important buffers from accidents

  Implementation Note:
  - :read-only flag in buffer
  - Edit operations check flag before execution
  - NOT fully implemented yet"
  (testing "Insert fails in read-only buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Initial content")
      (helpers/set-buffer-read-only true)

      (is (thrown? js/Error (helpers/insert "More"))
          "Insert blocked in read-only buffer")))

  (testing "Delete fails in read-only buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Delete me")
      (helpers/set-buffer-read-only true)

      (is (thrown? js/Error (helpers/delete-region 0 6))
          "Delete blocked in read-only buffer")))

  (testing "Navigation works in read-only buffer"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-buffer-read-only true)

      ;; These should work fine
      (helpers/goto-char 5)
      (is (= 5 (helpers/point)))

      (helpers/forward-char 2)
      (is (= 7 (helpers/point))
          "Cursor movement works in read-only buffer"))))

(deftest ^:critical inhibit-read-only-allows-edits
  "CRITICAL: inhibit-read-only bypasses read-only protection.

  Semantic Guarantee:
  - (let [inhibit-read-only true] ...) allows edits in read-only buffer
  - Only affects dynamic scope
  - Used by modes to update their own buffers
  - Restore protection after scope ends

  Why this matters:
  - Dired must update its listing when refreshed
  - Help must insert cross-reference links
  - Every special buffer mode needs this

  Implementation Note:
  - Dynamic var checked by edit operations
  - Scoped with let binding
  - NOT implemented yet"
  (testing "Insert succeeds with inhibit-read-only"
    (with-test-buffer "*test*"
      (helpers/insert "Initial")
      (helpers/set-buffer-read-only true)

      ;; Should work with inhibit
      (let [inhibit-read-only true]
        (helpers/insert " Modified"))

      (is (= "Initial Modified" (helpers/buffer-text))
          "Edit succeeded despite read-only")))

  (testing "Delete succeeds with inhibit-read-only"
    (with-test-buffer "*test*"
      (helpers/insert "Delete this part")
      (helpers/set-buffer-read-only true)

      (let [inhibit-read-only true]
        (helpers/delete-region 7 16))

      (is (= "Delete " (helpers/buffer-text))
          "Delete succeeded with inhibit")))

  (testing "Protection restored after scope"
    (with-test-buffer "*test*"
      (helpers/insert "Test")
      (helpers/set-buffer-read-only true)

      ;; Edit in inhibited scope
      (let [inhibit-read-only true]
        (helpers/insert " OK"))

      ;; Protection restored
      (is (thrown? js/Error (helpers/insert " FAIL"))
          "Read-only protection restored after scope"))))

(deftest ^:high nested-inhibit-read-only
  "HIGH: inhibit-read-only works with nesting.

  Semantic Guarantee:
  - Nested let bindings work correctly
  - Inner scope inherits outer inhibit
  - Protection restores at outermost scope end

  Why this matters:
  - Modes call helper functions that also inhibit
  - Nested function calls common
  - No need to track depth manually

  Implementation Note:
  - Dynamic var binding handles nesting
  - NOT implemented yet"
  (testing "Nested inhibit scopes work"
    (with-test-buffer "*test*"
      (helpers/set-buffer-read-only true)

      (let [inhibit-read-only true]
        (helpers/insert "Outer ")

        (let [inhibit-read-only true]
          (helpers/insert "Inner "))

        (helpers/insert "Outer again"))

      (is (= "Outer Inner Outer again" (helpers/buffer-text))
          "Nested inhibit worked correctly"))))

(deftest ^:high inhibit-with-errors
  "HIGH: inhibit-read-only protection restored even on error.

  Semantic Guarantee:
  - Error in inhibited scope still restores protection
  - try/finally semantics
  - Buffer doesn't become permanently writable

  Why this matters:
  - Errors in mode code shouldn't break read-only
  - User buffer remains protected
  - Defensive programming requirement

  Implementation Note:
  - Let binding cleanup is automatic in Clojure
  - NOT implemented yet"
  (testing "Protection restored despite error"
    (with-test-buffer "*test*"
      (helpers/set-buffer-read-only true)

      (is (thrown? js/Error
            (let [inhibit-read-only true]
              (helpers/insert "Before error")
              (throw (js/Error. "Test error"))))
          "Error propagated")

      ;; Protection should be restored
      (is (thrown? js/Error (helpers/insert " Should fail"))
          "Read-only protection restored after error"))))

;;; =============================================================================
;;; Integration with Modes
;;; =============================================================================

(deftest ^:critical dired-refresh-pattern
  "CRITICAL: Dired uses inhibit-read-only to refresh listing.

  Semantic Guarantee:
  - Dired buffer is read-only to user
  - Dired-revert uses inhibit-read-only to update
  - User can't accidentally modify listing
  - Refresh always works

  Why this matters:
  - This is THE motivating use case
  - Every special buffer mode needs this pattern
  - Proves primitive is sufficient

  Implementation Note:
  - Dired sets buffer-read-only on creation
  - Refresh wraps updates in inhibit-read-only
  - NOT implemented yet - Dired not complete"
  (testing "Dired refresh updates read-only buffer"
    (with-test-buffer "*dired*"
      ;; Simulate Dired buffer setup
      (helpers/insert "Initial listing")
      (helpers/set-buffer-read-only true)

      ;; Simulate Dired refresh
      (let [inhibit-read-only true]
        (helpers/erase-buffer)
        (helpers/insert "Refreshed listing"))

      (is (= "Refreshed listing" (helpers/buffer-text)))
      (is (helpers/buffer-read-only?)
          "Buffer still read-only after refresh"))))

(deftest ^:high help-buffer-pattern
  "HIGH: Help buffers use inhibit-read-only for link insertion.

  Semantic Guarantee:
  - Help buffer displays documentation
  - Read-only prevents user edits
  - Help code adds cross-reference links with properties
  - Links are clickable with text properties

  Why this matters:
  - Help system needs to generate formatted content
  - Content includes interactive elements
  - User shouldn't modify help text

  Implementation Note:
  - Help buffer set read-only
  - Link insertion uses inhibit-read-only
  - NOT implemented yet"
  (testing "Help buffer link insertion"
    (with-test-buffer "*Help*"
      (helpers/set-buffer-read-only true)

      ;; Simulate help content generation
      (let [inhibit-read-only true]
        (helpers/insert "See also: ")
        (let [start (helpers/point)]
          (helpers/insert "save-excursion")
          (helpers/put-text-property start (helpers/point) 'face 'link)))

      (is (= "See also: save-excursion" (helpers/buffer-text)))
      (is (= 'link (helpers/get-text-property 10 'face))
          "Link property added to read-only buffer"))))

(deftest ^:medium info-buffer-pattern
  "MEDIUM: Info buffers navigate while read-only.

  Semantic Guarantee:
  - Info displays documentation nodes
  - Read-only during viewing
  - Node navigation uses inhibit-read-only to replace content
  - Cursor tracks position

  Why this matters:
  - Documentation browser needs content replacement
  - Navigation is frequent operation
  - Must be smooth and reliable

  Implementation Note:
  - Info uses inhibit-read-only for node switching
  - NOT implemented yet - Info not implemented"
  (testing "Info node switching"
    (with-test-buffer "*info*"
      (helpers/insert "Node 1 content")
      (helpers/set-buffer-read-only true)

      ;; Simulate node navigation
      (let [inhibit-read-only true]
        (helpers/erase-buffer)
        (helpers/insert "Node 2 content"))

      (is (= "Node 2 content" (helpers/buffer-text))
          "Node content replaced in read-only buffer"))))

;;; =============================================================================
;;; Read-Only Indicator and Query
;;; =============================================================================

(deftest ^:high buffer-read-only-query
  "HIGH: Can query read-only status of buffer.

  Semantic Guarantee:
  - (buffer-read-only?) returns t if buffer is read-only
  - Returns nil otherwise
  - Reflects current state

  Why this matters:
  - Commands need to check before edit
  - Mode line displays indicator
  - User awareness

  Implementation Note:
  - Query :read-only flag in buffer
  - NOT implemented yet"
  (testing "Query read-only status"
    (with-test-buffer "*test*"
      (is (not (helpers/buffer-read-only?))
          "New buffer not read-only")

      (helpers/set-buffer-read-only true)
      (is (helpers/buffer-read-only?)
          "Buffer now read-only")

      (helpers/set-buffer-read-only false)
      (is (not (helpers/buffer-read-only?))
          "Buffer writable again"))))

(deftest ^:medium read-only-mode-line-indicator
  "MEDIUM: Read-only status shown in mode line.

  Semantic Guarantee:
  - Mode line shows %% for writable, %* for modified
  - Shows %% for read-only
  - User knows buffer state

  Why this matters:
  - User awareness
  - Standard Emacs convention
  - Prevents confusion

  Implementation Note:
  - Mode line rendering checks :read-only
  - NOT implemented yet - mode line not complete"
  (testing "Mode line reflects read-only"
    (with-test-buffer "*test*"
      (let [mode-line (helpers/mode-line-string)]
        (is (not (re-find #"%%\s+\*test\*" mode-line))
            "Not read-only indicator"))

      (helpers/set-buffer-read-only true)
      (let [mode-line (helpers/mode-line-string)]
        (is (re-find #"%%\s+\*test\*" mode-line)
            "Shows read-only indicator")))))

;;; =============================================================================
;;; Special Cases and Edge Cases
;;; =============================================================================

(deftest ^:medium toggle-read-only
  "MEDIUM: toggle-read-only convenience command.

  Semantic Guarantee:
  - (toggle-read-only) flips read-only status
  - Convenient for temporary edits
  - User-facing command

  Why this matters:
  - User wants to edit special buffer temporarily
  - Quick toggle without remembering current state
  - Standard Emacs command

  Implementation Note:
  - Read current state, set opposite
  - NOT implemented yet"
  (testing "Toggle flips state"
    (with-test-buffer "*test*"
      (is (not (helpers/buffer-read-only?)))

      (helpers/toggle-read-only)
      (is (helpers/buffer-read-only?) "Toggled to read-only")

      (helpers/toggle-read-only)
      (is (not (helpers/buffer-read-only?)) "Toggled back to writable"))))

(deftest ^:medium read-only-with-text-properties
  "MEDIUM: Can modify text properties in read-only buffer with inhibit.

  Semantic Guarantee:
  - Text properties are metadata, not content
  - Inhibit-read-only allows property changes
  - Used by syntax highlighting, folding

  Why this matters:
  - Syntax highlighting needs to update faces
  - Org-mode folding sets invisible property
  - Properties change without 'editing'

  Implementation Note:
  - Property changes are edits, need inhibit
  - NOT implemented yet"
  (testing "Properties modifiable with inhibit"
    (with-test-buffer "*test*"
      (helpers/insert "Highlight me")
      (helpers/set-buffer-read-only true)

      ;; Should work with inhibit
      (let [inhibit-read-only true]
        (helpers/put-text-property 0 9 'face 'bold))

      (is (= 'bold (helpers/get-text-property 4 'face))
          "Property set in read-only buffer"))))

(deftest ^:low inhibit-read-only-interactive-warning
  "LOW: Commands warn when editing read-only buffer.

  Semantic Guarantee:
  - User command in read-only buffer prompts
  - \\\"Buffer is read-only\\\" error message
  - Helps user understand why edit failed

  Why this matters:
  - User awareness
  - Better error messages than silent failure
  - Standard Emacs behavior

  Implementation Note:
  - Edit operations throw with message
  - NOT implemented yet - error messages basic"
  (testing "Read-only error has helpful message"
    (with-test-buffer "*test*"
      (helpers/set-buffer-read-only true)

      (try
        (helpers/insert "Should fail")
        (is false "Should have thrown")
        (catch js/Error e
          (is (re-find #"read-only" (.-message e))
              "Error message mentions read-only"))))))

;;; =============================================================================
;;; Performance and Correctness
;;; =============================================================================

(deftest ^:medium inhibit-read-only-minimal-overhead
  "MEDIUM: inhibit-read-only has negligible performance impact.

  Semantic Guarantee:
  - Checking inhibit-read-only is fast
  - No noticeable slowdown
  - Dynamic var lookup is efficient

  Why this matters:
  - Every edit checks this flag
  - Modes use it heavily (Dired refresh)
  - Must not slow down editor

  Implementation Note:
  - Simple dynamic var check
  - ClojureScript binding is fast
  - NOT a concern, but worth documenting"
  (testing "Many edits with inhibit don't slow down"
    (with-test-buffer "*test*"
      (helpers/set-buffer-read-only true)

      (let [start-time (js/Date.now)
            inhibit-read-only true]
        ;; 1000 insertions
        (dotimes [i 1000]
          (helpers/insert "x"))

        (let [elapsed (- (js/Date.now) start-time)]
          (is (< elapsed 1000)
              "1000 inhibited edits took less than 1s"))))))
