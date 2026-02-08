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

    ;; Press Enter to jump to source
    (h/press-key "Enter")
    (Thread/sleep 200)

    ;; Should now be in the original buffer (not *Occur*)
    ;; The buffer should contain the original content
    (let [buffer-text (h/get-buffer-text*)]
      ;; If we're in *Occur*, Enter would have inserted a newline
      ;; If we jumped to source, we should see the original 3-line content
      (is (str/includes? buffer-text "first line")
          (str "Should have jumped to source buffer, got: " buffer-text))
      (is (not (str/includes? buffer-text "matches for"))
          "Should NOT be in *Occur* buffer anymore"))))

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
