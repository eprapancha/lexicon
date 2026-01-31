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
