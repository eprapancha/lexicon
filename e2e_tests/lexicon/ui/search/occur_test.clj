(ns lexicon.ui.search.occur-test
  "E2E tests for occur-mode - list lines matching regexp.

  Tests occur functionality:
  - M-x occur prompts for regexp
  - Creates *Occur* buffer with matching lines
  - Line numbers are displayed

  Uses keyboard simulation for all operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Occur - Find Matching Lines
;; =============================================================================

(deftest test-occur-basic
  (testing "M-x occur creates buffer with matching lines"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create multi-line content with pattern
    (h/type-text "line one")
    (h/press-key "Enter")
    (h/type-text "line two")
    (h/press-key "Enter")
    (h/type-text "line three")
    (h/press-key "Enter")
    (h/type-text "other four")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type "occur" into minibuffer and execute
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Now we should have the regexp prompt - enter the pattern
    (h/type-in-minibuffer "line")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Check that *Occur* buffer was created with matches
    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "matches for")
          (str "Occur buffer should show match count, got: " buffer-text))
      (is (str/includes? buffer-text "line one")
          "Occur buffer should contain matching line 1")
      (is (str/includes? buffer-text "line two")
          "Occur buffer should contain matching line 2")
      (is (str/includes? buffer-text "line three")
          "Occur buffer should contain matching line 3")
      (is (not (str/includes? buffer-text "other four"))
          "Occur buffer should NOT contain non-matching line"))))

(deftest test-occur-no-matches
  (testing "M-x occur with no matches shows message"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "hello world")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Search for non-existent pattern
    (h/type-in-minibuffer "ZZZZZ")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Should show "No matches" message - check messages buffer
    (let [messages (h/get-messages-buffer h/*driver*)]
      (is (str/includes? messages "No matches")
          (str "Should show 'No matches' message, got: " messages)))))

(deftest test-occur-line-numbers
  (testing "Occur buffer displays line numbers"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create content where matches are on specific lines
    (h/type-text "first")
    (h/press-key "Enter")
    (h/type-text "match here")
    (h/press-key "Enter")
    (h/type-text "third")
    (h/press-key "Enter")
    (h/type-text "match again")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Search for "match"
    (h/type-in-minibuffer "match")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Check line numbers are present
    (let [buffer-text (h/get-buffer-text*)]
      ;; Line 2: "match here"
      ;; Line 4: "match again"
      (is (str/includes? buffer-text "2:")
          (str "Should show line number 2, got: " buffer-text))
      (is (str/includes? buffer-text "4:")
          (str "Should show line number 4, got: " buffer-text)))))

(deftest test-occur-regexp
  (testing "Occur supports regexp patterns"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "foo123")
    (h/press-key "Enter")
    (h/type-text "bar456")
    (h/press-key "Enter")
    (h/type-text "foo789")
    (h/press-key "Enter")
    (h/type-text "baz")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Search with regexp for lines starting with "foo"
    (h/type-in-minibuffer "^foo")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "foo123")
          (str "Should find foo123, got: " buffer-text))
      (is (str/includes? buffer-text "foo789")
          "Should find foo789")
      (is (not (str/includes? buffer-text "bar456"))
          "Should NOT find bar456"))))

;; =============================================================================
;; Occur Mode Navigation - Issue #197
;; =============================================================================

(deftest test-occur-next-prev-navigation
  (testing "n/p keys navigate between matches in *Occur* buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create content with multiple matches
    (h/type-text "apple one")
    (h/press-key "Enter")
    (h/type-text "banana")
    (h/press-key "Enter")
    (h/type-text "apple two")
    (h/press-key "Enter")
    (h/type-text "cherry")
    (h/press-key "Enter")
    (h/type-text "apple three")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Search for "apple"
    (h/type-in-minibuffer "apple")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Now in *Occur* buffer - verify we have 3 matches
    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "3 matches")
          (str "Should have 3 matches, got: " buffer-text)))

    ;; Press 'n' to move to next match - should NOT insert 'n'
    (h/press-key "n")
    (Thread/sleep 100)
    (let [buffer-text (h/get-buffer-text*)]
      (is (not (str/includes? buffer-text "\nn\n"))
          "Pressing 'n' should navigate, not insert character"))

    ;; Press 'p' to move to previous match - should NOT insert 'p'
    (h/press-key "p")
    (Thread/sleep 100)
    (let [buffer-text (h/get-buffer-text*)]
      (is (not (str/includes? buffer-text "\np\n"))
          "Pressing 'p' should navigate, not insert character"))))

(deftest test-occur-navigation-skips-headers
  (testing "n/p skip header lines and only navigate between match lines"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create content - 3 matches on lines 1, 3, 5
    (h/type-text "match one")
    (h/press-key "Enter")
    (h/type-text "other")
    (h/press-key "Enter")
    (h/type-text "match two")
    (h/press-key "Enter")
    (h/type-text "other")
    (h/press-key "Enter")
    (h/type-text "match three")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "match")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Verify occur buffer has 3 matches
    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "3 matches")
          (str "Should have 3 matches, got: " buffer-text)))

    ;; Press n/p multiple times - buffer should NOT be modified
    ;; This verifies n/p are navigation commands, not inserting characters
    (let [before-text (h/get-buffer-text*)]
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")
      (Thread/sleep 50)
      (h/press-key "n")  ;; Should be at last match or past it
      (Thread/sleep 50)
      (h/press-key "p")
      (Thread/sleep 50)
      (h/press-key "p")
      (Thread/sleep 100)
      (let [after-text (h/get-buffer-text*)]
        (is (= before-text after-text)
            "Buffer should not change after n/p navigation")))))

(deftest test-occur-enter-jumps-to-source
  (testing "RET in *Occur* buffer jumps to match in source buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create content
    (h/type-text "first line")
    (h/press-key "Enter")
    (h/type-text "target line here")
    (h/press-key "Enter")
    (h/type-text "third line")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Search for "target"
    (h/type-in-minibuffer "target")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Verify we're in *Occur* buffer with a match
    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "target line here")
          (str "Occur buffer should show match, got: " buffer-text)))

    ;; Verify we have 2 windows before pressing Enter
    (let [window-count-before (h/get-window-count)]
      (is (= 2 window-count-before)
          (str "Should have 2 windows before Enter, got: " window-count-before)))

    ;; Press Enter to jump to source
    (h/press-key "Enter")
    (Thread/sleep 200)

    ;; CRITICAL: Should still have 2 windows - *Occur* should NOT be replaced
    (let [window-count-after (h/get-window-count)]
      (is (= 2 window-count-after)
          (str "Should still have 2 windows after Enter (Occur must not be replaced), got: " window-count-after)))

    ;; Should now be in the original buffer (not *Occur*)
    (let [buffer-text (h/get-buffer-text*)]
      (is (str/includes? buffer-text "first line")
          (str "Should have jumped to source buffer, got: " buffer-text))
      (is (not (str/includes? buffer-text "matches for"))
          "Should NOT be in *Occur* buffer anymore"))

    ;; CRITICAL: *Occur* buffer should still exist
    (let [occur-exists? (h/buffer-exists? "*Occur*")]
      (is occur-exists?
          "*Occur* buffer should still exist after jumping to source"))))

(deftest test-occur-buffer-read-only
  (testing "*Occur* buffer should be read-only"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "test content")
    (Thread/sleep 50)

    ;; Run M-x occur
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (h/type-in-minibuffer "test")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Try to type in *Occur* buffer
    (let [before-text (h/get-buffer-text*)]
      (h/type-text "INSERTED")
      (Thread/sleep 100)
      (let [after-text (h/get-buffer-text*)]
        (is (= before-text after-text)
            (str "Buffer should be read-only. Before: " before-text " After: " after-text))))))

;; =============================================================================
;; Occur Highlighting Tests - Issue #197
;; These tests verify that match highlight DATA is correct in buffer state.
;; This tests the data layer, not the rendering layer.
;; =============================================================================

(deftest test-occur-highlight-data-exists
  (testing "Occur stores highlight data in buffer state"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "hello world")
    (h/press-key "Enter")
    (h/type-text "hello again")
    (Thread/sleep 50)

    ;; Run M-x occur for "hello"
    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "hello")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    ;; Check that highlight data exists
    (let [highlights (h/get-occur-highlights)]
      (is (some? highlights)
          "Occur buffer should have highlight data")
      (is (seq highlights)
          (str "Highlights should not be empty, got: " highlights)))))

(deftest test-occur-highlight-ranges-for-pattern-at-start
  (testing "Occur highlight ranges are correct for pattern at start of line"
    (h/setup-test*)
    (h/clear-buffer)
    ;; "hello one" - "hello" is at position 0-5 in original line
    ;; In occur buffer: "     1: hello one"
    ;; prefix is 8 chars ("     1: "), so highlight should be [8, 13]
    (h/type-text "hello one")
    (Thread/sleep 50)

    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "hello")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    (let [highlights (h/get-occur-highlights)]
      (is (= 1 (count highlights))
          (str "Should have 1 highlight entry, got: " (count highlights)))
      (when (seq highlights)
        (let [first-highlight (first highlights)
              ranges (:ranges first-highlight)]
          (is (= 1 (count ranges))
              (str "Should have 1 range, got: " ranges))
          (when (seq ranges)
            (let [[start end] (first ranges)]
              ;; "hello" is 5 chars, prefix is 8 chars
              ;; So highlight should be at columns 8-13
              (is (= 8 start)
                  (str "Highlight should start at column 8 (after prefix), got: " start))
              (is (= 13 end)
                  (str "Highlight should end at column 13 (8 + 5), got: " end)))))))))

(deftest test-occur-highlight-ranges-for-pattern-at-end
  (testing "Occur highlight ranges are correct for pattern at end of line"
    (h/setup-test*)
    (h/clear-buffer)
    ;; "one hello" - "hello" is at position 4-9 in original line
    ;; In occur buffer: "     1: one hello"
    ;; prefix is 8 chars, so highlight should be [8+4, 8+9] = [12, 17]
    (h/type-text "one hello")
    (Thread/sleep 50)

    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "hello")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    (let [highlights (h/get-occur-highlights)]
      (is (= 1 (count highlights))
          (str "Should have 1 highlight entry, got: " (count highlights)))
      (when (seq highlights)
        (let [first-highlight (first highlights)
              ranges (:ranges first-highlight)]
          (is (= 1 (count ranges))
              (str "Should have 1 range, got: " ranges))
          (when (seq ranges)
            (let [[start end] (first ranges)]
              ;; "hello" starts at position 4 in "one hello"
              ;; prefix is 8 chars, so highlight should be at columns 12-17
              (is (= 12 start)
                  (str "Highlight should start at column 12 (8 + 4), got: " start))
              (is (= 17 end)
                  (str "Highlight should end at column 17 (8 + 9), got: " end)))))))))

(deftest test-occur-highlight-multiple-matches-per-line
  (testing "Occur highlight data includes ALL matches on same line"
    (h/setup-test*)
    (h/clear-buffer)
    ;; "cat dog cat" - two "cat" matches at positions 0-3 and 8-11
    (h/type-text "cat dog cat")
    (Thread/sleep 50)

    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "cat")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    (let [highlights (h/get-occur-highlights)]
      (is (= 1 (count highlights))
          (str "Should have 1 highlight entry (one line), got: " (count highlights)))
      (when (seq highlights)
        (let [first-highlight (first highlights)
              ranges (:ranges first-highlight)]
          (is (= 2 (count ranges))
              (str "Should have 2 ranges (two 'cat' matches), got: " ranges))
          (when (= 2 (count ranges))
            (let [[start1 end1] (first ranges)
                  [start2 end2] (second ranges)]
              ;; First "cat" at position 0: [8, 11]
              (is (= 8 start1) (str "First match should start at 8, got: " start1))
              (is (= 11 end1) (str "First match should end at 11, got: " end1))
              ;; Second "cat" at position 8: [16, 19]
              (is (= 16 start2) (str "Second match should start at 16, got: " start2))
              (is (= 19 end2) (str "Second match should end at 19, got: " end2)))))))))

(deftest test-occur-highlight-line-numbers-correct
  (testing "Occur highlight data has correct line numbers"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Create 3 lines, matches on lines 1 and 3 only
    ;; Use a pattern that doesn't appear in middle line
    (h/type-text "apple one")
    (h/press-key "Enter")
    (h/type-text "banana")
    (h/press-key "Enter")
    (h/type-text "apple two")
    (Thread/sleep 50)

    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "apple")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    (let [highlights (h/get-occur-highlights)]
      (is (= 2 (count highlights))
          (str "Should have 2 highlight entries (two matching lines), got: " (count highlights)))
      (when (= 2 (count highlights))
        ;; Occur buffer format (with keybinding hints header):
        ;; Line 0: "2 matches for..."
        ;; Line 1: "[n/p: navigate, RET: goto match, q: quit]"
        ;; Line 2: (blank from \n in header)
        ;; Line 3: "     1: apple one"  <- first match
        ;; Line 4: "     3: apple two"  <- second match (skipping line 2 "banana")
        (let [line1 (:line (first highlights))
              line2 (:line (second highlights))]
          (is (= 3 line1)
              (str "First highlight should be on occur-line 3, got: " line1))
          (is (= 4 line2)
              (str "Second highlight should be on occur-line 4, got: " line2)))))))

;; =============================================================================
;; Occur Rendering Tests - Issue #197
;; These tests verify that the occur-match CSS class is applied correctly in DOM.
;; =============================================================================

(deftest test-occur-match-class-in-dom
  (testing "DOM elements have occur-match class with correct text"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Simple case: "hello world" with search for "hello"
    (h/type-text "hello world")
    (Thread/sleep 50)

    (h/press-meta "x")
    (Thread/sleep 200)
    (h/type-in-minibuffer "occur")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)
    (h/type-in-minibuffer "hello")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 500)

    ;; Verify DOM has occur-match elements
    (let [match-elements (h/get-occur-match-elements)]
      (is (seq match-elements)
          (str "Should have DOM elements with occur-match class, got: " match-elements))
      (when (seq match-elements)
        (let [texts (mapv :text match-elements)]
          (is (some #(= % "hello") texts)
              (str "One occur-match element should contain 'hello', got: " texts)))))))
