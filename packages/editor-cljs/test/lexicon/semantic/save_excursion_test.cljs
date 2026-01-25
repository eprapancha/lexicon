(ns lexicon.semantic.save-excursion-test
  "Semantic tests for save-excursion - CORE macro for temporary navigation.

  Save-excursion saves point and mark, executes body, then restores them.
  Used by: virtually every command that 'looks around' without moving cursor.

  Related: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md
  Priority: CRITICAL - ergonomic requirement, used everywhere"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]
                   [lexicon.core.macros :refer [save-excursion save-current-buffer]]))

;; Emacs-style t for true
(def t true)

;;; =============================================================================
;;; Save-Excursion - Core Macro Tests
;;; =============================================================================

(deftest ^:critical save-excursion-restores-point
  "CRITICAL: save-excursion restores point after body execution.

  Semantic Guarantee:
  - Point before save-excursion: N
  - Body moves point to M
  - Point after save-excursion: N (restored)

  Why this matters:
  - Commands can look ahead/behind without confusing user
  - Dired can check file properties without moving cursor
  - Every search/query operation needs this

  Implementation Note:
  - Macro captures point before body
  - Executes body in try block
  - Restores point in finally block
  - NOT implemented yet"
  (testing "Point restored after moving in body"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 6)  ; Middle of buffer

      (save-excursion
        ;; Move point in body
        (helpers/goto-char 0)
        (is (= 0 (helpers/point)) "Point moved in body"))

      ;; Point should be restored
      (is (= 6 (helpers/point))
          "Point restored after save-excursion")))

  (testing "Point restored even with multiple moves"
    (with-test-buffer "*test*"
      (helpers/insert "ABCDEFGHIJ")
      (helpers/goto-char 5)

      (save-excursion
        (helpers/goto-char 0)
        (helpers/goto-char 10)
        (helpers/goto-char 3)
        (is (= 3 (helpers/point))))

      (is (= 5 (helpers/point))
          "Point restored despite multiple moves"))))

(deftest ^:critical save-excursion-restores-on-error
  "CRITICAL: save-excursion restores point even if body throws error.

  Semantic Guarantee:
  - Point saved before body
  - Body throws exception
  - Point still restored before exception propagates

  Why this matters:
  - Errors shouldn't leave cursor in random position
  - User expects cursor stability
  - Defensive programming requirement

  Implementation Note:
  - Use try/finally block
  - Restoration in finally, not after body
  - NOT implemented yet"
  (testing "Point restored even when body errors"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 6)

      (is (thrown? js/Error
            (save-excursion
              (helpers/goto-char 0)
              (throw (js/Error. "Test error"))))
          "Error propagates")

      ;; Despite error, point should be restored
      (is (= 6 (helpers/point))
          "Point restored even after error"))))

(deftest ^:high save-excursion-restores-mark
  "HIGH: save-excursion also restores mark (region).

  Semantic Guarantee:
  - Mark before save-excursion: N
  - Body sets mark to M
  - Mark after save-excursion: N (restored)

  Why this matters:
  - Operations that set mark temporarily
  - Region-based commands that look elsewhere
  - Preserve user's selection

  Implementation Note:
  - Save both point AND mark
  - Restore both in finally
  - NOT implemented yet"
  (testing "Mark restored after body"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 0)
      (helpers/set-mark 5)  ; Mark at position 5

      (save-excursion
        ;; Change mark in body
        (helpers/set-mark 10)
        (is (= 10 (helpers/mark))))

      ;; Mark should be restored
      (is (= 5 (helpers/mark))
          "Mark restored after save-excursion")))

  (testing "Active region status restored"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/set-mark 0)
      (helpers/goto-char 5)
      (helpers/activate-mark)  ; Explicitly activate

      (save-excursion
        (helpers/deactivate-mark)
        (is (not (helpers/region-active?))))

      (is (helpers/region-active?)
          "Region active state restored"))))

(deftest ^:high save-excursion-return-value
  "HIGH: save-excursion returns value of last form in body.

  Semantic Guarantee:
  - Body evaluates to value V
  - save-excursion evaluates to V
  - Like progn, returns last form

  Why this matters:
  - Can use save-excursion in functional style
  - Common pattern: (save-excursion (goto X) (looking-at REGEXP))
  - Convenience and composability

  Implementation Note:
  - Macro captures body result
  - Returns it after restoration
  - NOT implemented yet"
  (testing "Return value from body"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")

      (let [result (save-excursion
                     (helpers/goto-char 0)
                     (helpers/looking-at "Hello"))]
        (is (= true result)
            "save-excursion returns body value")))))

(deftest ^:medium save-excursion-nested
  "MEDIUM: save-excursion can be nested.

  Semantic Guarantee:
  - Outer save-excursion saves point P1
  - Inner save-excursion saves point P2
  - Inner restores to P2, outer restores to P1
  - Nesting works correctly

  Why this matters:
  - Complex operations with multiple levels
  - Recursive functions that need save-excursion
  - Composable primitives

  Implementation Note:
  - Each invocation has its own saved state
  - Stack-based restoration
  - NOT implemented yet"
  (testing "Nested save-excursion"
    (with-test-buffer "*test*"
      (helpers/insert "ABCDEFGHIJ")
      (helpers/goto-char 5)  ; Start at 'F'

      (save-excursion
        (helpers/goto-char 3)  ; Move to 'D'

        (save-excursion
          (helpers/goto-char 7)  ; Move to 'H'
          (is (= 7 (helpers/point))))

        ;; Inner restored, should be at 'D'
        (is (= 3 (helpers/point))))

      ;; Outer restored, should be at 'F'
      (is (= 5 (helpers/point))
          "Nested save-excursion restores correctly"))))

(deftest ^:medium save-excursion-with-buffer-switch
  "MEDIUM: save-excursion restores buffer if switched.

  Semantic Guarantee:
  - Current buffer before: B1
  - Body switches to buffer B2
  - Current buffer after: B1 (restored)

  Why this matters:
  - Commands that check other buffers
  - Dired operations that look at file buffers
  - User expects to stay in same buffer

  Implementation Note:
  - Save current buffer ID
  - Restore buffer in finally
  - Point is buffer-local, so restoring buffer restores point
  - NOT implemented yet"
  (testing "Buffer restored after switch"
    (with-test-buffer "*buf1*"
      (helpers/insert "Buffer 1")

      (save-excursion
        (with-test-buffer "*buf2*"
          (helpers/insert "Buffer 2")
          (is (= "*buf2*" (helpers/buffer-name)))))

      ;; Should be back in buf1
      (is (= "*buf1*" (helpers/buffer-name))
          "Buffer restored after save-excursion"))))

;;; =============================================================================
;;; Integration with Core Systems
;;; =============================================================================

(deftest ^:high save-excursion-in-read-only-buffer
  "HIGH: save-excursion works in read-only buffers.

  Semantic Guarantee:
  - Read-only buffer
  - save-excursion can move point (not an edit)
  - Point restored after

  Why this matters:
  - Help buffers, Dired buffers are read-only
  - Commands still need to navigate
  - Read-only only blocks edits, not navigation

  Implementation Note:
  - Point movement is not an edit
  - Should work regardless of read-only status
  - NOT implemented yet"
  (testing "Navigate in read-only buffer"
    (with-test-buffer "*help*"
      (helpers/insert "Help text here")
      (helpers/set-buffer-read-only true)
      (helpers/goto-char 5)

      (save-excursion
        (helpers/goto-char 0)
        (is (= 0 (helpers/point))))

      (is (= 5 (helpers/point))
          "Point restored in read-only buffer"))))

(deftest ^:medium save-excursion-with-narrowing
  "MEDIUM: save-excursion interacts correctly with narrowing.

  Semantic Guarantee:
  - Buffer narrowed to region [A, B]
  - save-excursion moves within narrowed region
  - Point restored within narrowed region
  - Narrowing status unchanged

  Why this matters:
  - Org-mode narrows to subtrees
  - Indirect buffers
  - Advanced editing modes

  Implementation Note:
  - save-excursion doesn't affect narrowing
  - Point is logical position in narrowed view
  - NOT implemented yet - narrowing not implemented"
  (testing "save-excursion with narrowing"
    (with-test-buffer "*test*"
      (helpers/insert "AAABBBCCCDDDEEE")
      ;; Narrow to "BBBCCCDD" (positions 3-11)
      (helpers/narrow-to-region 3 11)
      (helpers/goto-char 6)  ; Position 6 in full buffer, 3 in narrowed

      (save-excursion
        (helpers/goto-char 3)  ; Go to start of narrowed region
        (is (= 3 (helpers/point))))

      (is (= 6 (helpers/point))
          "Point restored in narrowed buffer"))))

;;; =============================================================================
;;; Usage Patterns from Emacs
;;; =============================================================================

(deftest ^:high save-excursion-look-ahead-pattern
  "HIGH: Common pattern - look ahead without moving cursor.

  Semantic Guarantee:
  - Function checks what's ahead
  - Returns result
  - Cursor stays at original position

  Why this matters:
  - Syntax analysis (what's the next token?)
  - Dired checking file properties
  - Every 'looking-at' operation

  Implementation Note:
  - This is THE primary use case
  - NOT implemented yet"
  (testing "Look ahead pattern"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 0)

      (let [next-word (save-excursion
                        (helpers/forward-word)
                        (helpers/word-at-point))]
        (is (= "Hello" next-word)
            "Extracted next word")
        (is (= 0 (helpers/point))
            "Cursor didn't move")))))

(deftest ^:medium save-excursion-scan-buffer-pattern
  "MEDIUM: Common pattern - scan entire buffer for matches.

  Semantic Guarantee:
  - Scan from beginning to end
  - Collect all matches
  - Return to original position

  Why this matters:
  - Count occurrences
  - Build index
  - Dired collecting marked files

  Implementation Note:
  - Dired uses this 30+ times
  - NOT implemented yet"
  (testing "Scan buffer pattern"
    (with-test-buffer "*test*"
      (helpers/insert "foo bar foo baz foo")
      (helpers/goto-char 10)  ; Middle of buffer

      (let [count (save-excursion
                    (helpers/goto-char 0)
                    (loop [n 0]
                      (if (helpers/search-forward "foo" nil t)
                        (recur (inc n))
                        n)))]
        (is (= 3 count) "Found 3 occurrences")
        (is (= 10 (helpers/point))
            "Cursor back at original position")))))
