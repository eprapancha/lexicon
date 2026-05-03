(ns lexicon.lisp.grep-test
  "Lisp API tests for WASM grep search functionality.

  Tests the grep-search function which uses ripgrep's search engine
  compiled to WASM for fast regex matching on buffer content.

  JUSTIFICATION: grep-search is a Lisp function that calls WASM
  ripgrep and returns structured match data. It has no keyboard
  equivalent and must be tested via Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Search
;; =============================================================================

(deftest test-grep-search-basic
  (testing "grep-search finds matching lines in buffer content"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello world\\nfoo bar\\nhello again\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"hello\" (buffer-string))")]
      (is (= 2 (count result)) "Should find 2 matches")
      (is (= 1 (:line_number (first result))) "First match on line 1")
      (is (= 3 (:line_number (second result))) "Second match on line 3"))))

(deftest test-grep-search-line-text
  (testing "grep-search returns matched line text"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"alpha\\nbeta\\ngamma\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"beta\" (buffer-string))")]
      (is (= 1 (count result)))
      (is (= "beta" (:line_text (first result)))))))

(deftest test-grep-search-match-positions
  (testing "grep-search returns match start and end positions within line"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"foo hello bar\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"hello\" (buffer-string))")]
      (is (= 1 (count result)))
      (is (= 4 (:match_start (first result))) "Match starts at column 4")
      (is (= 9 (:match_end (first result))) "Match ends at column 9"))))

;; =============================================================================
;; Regex Search
;; =============================================================================

(deftest test-grep-search-regex
  (testing "grep-search supports regex patterns"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"foo123\\nbar456\\nfoo789\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"foo[0-9]+\" (buffer-string))")]
      (is (= 2 (count result)) "Should find 2 regex matches")
      (is (= "foo123" (:line_text (first result))))
      (is (= "foo789" (:line_text (second result)))))))

;; =============================================================================
;; Options
;; =============================================================================

(deftest test-grep-search-case-insensitive
  (testing "grep-search with case-insensitive option"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\\nhello world\\nHELLO WORLD\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"hello\" (buffer-string) :case-insensitive true)")]
      (is (= 3 (count result)) "Case-insensitive should match all 3 lines"))))

(deftest test-grep-search-max-count
  (testing "grep-search respects max-count limit"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"a\\na\\na\\na\\na\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"a\" (buffer-string) :max-count 2)")]
      (is (= 2 (count result)) "Should return at most 2 matches"))))

(deftest test-grep-search-fixed-strings
  (testing "grep-search with fixed-strings treats pattern as literal"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"foo.bar\\nfooXbar\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"foo.bar\" (buffer-string) :fixed-strings true)")]
      (is (= 1 (count result)) "Fixed-strings should only match literal foo.bar")
      (is (= "foo.bar" (:line_text (first result)))))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-grep-search-no-matches
  (testing "grep-search returns empty list when no matches"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello world\\n\")")
    (let [result (lisp/eval-lisp! "(grep-search \"xyz\" (buffer-string))")]
      (is (= 0 (count result)) "Should return empty list"))))

(deftest test-grep-search-empty-buffer
  (testing "grep-search on empty content returns empty list"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(grep-search \"foo\" (buffer-string))")]
      (is (= 0 (count result)) "Should return empty list for empty buffer"))))
