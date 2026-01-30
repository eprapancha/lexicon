(ns lexicon.lisp.editing-test
  "Lisp API tests for editing macros and navigation.

  Tests editing-related Lisp macros and functions:
  - save-excursion: Save/restore point and mark around body
  - Navigation functions: point, goto-char, forward-line
  - Search functions: search-forward

  JUSTIFICATION: save-excursion is a Lisp macro, not a keyboard command.
  Testing its behavior (save point, execute body, restore point) requires
  Lisp evaluation. Navigation functions are also tested at API level to
  verify correct exposure to Lisp."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Save-Excursion
;; =============================================================================
;;
;; save-excursion is a Lisp macro that saves point/mark, executes body,
;; then restores point/mark. These tests verify the Lisp API behavior.

(deftest test-save-excursion-restores-point
  (testing "Point restored after movement in save-excursion body"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(goto-char 5)")
    (let [pt-before (lisp/eval-lisp! "(point)")]
      ;; Move point inside save-excursion
      (lisp/eval-lisp! "(save-excursion (goto-char 0))")
      (is (= pt-before (lisp/eval-lisp! "(point)"))
          "Point should be restored after save-excursion"))))

(deftest test-save-excursion-restores-point-multiple-moves
  (testing "Point restored even after multiple moves"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (lisp/eval-lisp! "(goto-char 5)")
    (let [pt-before (lisp/eval-lisp! "(point)")]
      ;; Multiple moves inside save-excursion
      (lisp/eval-lisp! "(save-excursion
                         (goto-char 0)
                         (goto-char 10)
                         (goto-char 3))")
      (is (= pt-before (lisp/eval-lisp! "(point)"))
          "Point restored after multiple moves"))))

(deftest test-save-excursion-return-value
  (testing "save-excursion returns body's value"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (let [result (lisp/eval-lisp! "(save-excursion (goto-char 0) (+ 1 2))")]
      (is (= 3 result) "Should return last form's value"))))

(deftest test-save-excursion-restores-on-error
  (testing "Point restored even when body errors"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(goto-char 5)")
    (let [pt-before (lisp/eval-lisp! "(point)")]
      ;; Try to cause an error inside save-excursion
      (lisp/eval-lisp "(save-excursion (goto-char 0) (error \"test\"))")
      ;; Point should still be restored
      (is (= pt-before (lisp/eval-lisp! "(point)"))
          "Point restored even after error in body"))))

(deftest test-save-excursion-nested
  (testing "Nested save-excursion works correctly"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"ABCDEFGHIJ\")")
    (lisp/eval-lisp! "(goto-char 5)")
    (let [outer-pt (lisp/eval-lisp! "(point)")]
      (lisp/eval-lisp! "(save-excursion
                         (goto-char 2)
                         (save-excursion
                           (goto-char 8)))")
      (is (= outer-pt (lisp/eval-lisp! "(point)"))
          "Outer point restored after nested save-excursion"))))

(deftest test-save-excursion-look-ahead-pattern
  (testing "Look-ahead pattern - check content without moving cursor"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (let [pt-before (lisp/eval-lisp! "(point)")
          ;; Look ahead to find "World" without moving point
          found? (lisp/eval-lisp! "(save-excursion
                                    (search-forward \"World\"))")]
      (is (= pt-before (lisp/eval-lisp! "(point)"))
          "Point unchanged after look-ahead")
      (is (some? found?) "Should find 'World'"))))

;; =============================================================================
;; Navigation Functions
;; =============================================================================

(deftest test-point
  (testing "point returns current position"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello\")")
    (is (number? (lisp/eval-lisp! "(point)")))))

(deftest test-goto-char
  (testing "goto-char moves to specified position"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"hello\")")
    (lisp/eval-lisp! "(goto-char 2)")
    (is (= 2 (lisp/eval-lisp! "(point)")))))

(deftest test-forward-line
  (testing "forward-line moves to next line"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"line1\\nline2\\nline3\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (lisp/eval-lisp! "(forward-line 1)")
    ;; Should be at start of line2
    (is (>= (lisp/eval-lisp! "(point)") 6))))

;; =============================================================================
;; Search Functions
;; =============================================================================

(deftest test-search-forward
  (testing "search-forward finds text and returns position after match"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (let [result (lisp/eval-lisp! "(search-forward \"World\")")]
      ;; "World" starts at 6, so position after match is 11
      (is (= 11 result) "Should return position after match")
      (is (= 11 (lisp/eval-lisp! "(point)")) "Point should be at end of match"))))

(deftest test-search-forward-not-found
  (testing "search-forward returns nil when text not found"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (let [result (lisp/eval-lisp! "(search-forward \"xyz\")")]
      (is (nil? result) "Should return nil when not found"))))

(deftest test-search-forward-case-insensitive
  (testing "search-forward is case-insensitive for lowercase patterns"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello WORLD hello\")")
    (lisp/eval-lisp! "(goto-char 0)")
    ;; lowercase "world" should match "WORLD"
    (let [result (lisp/eval-lisp! "(search-forward \"world\")")]
      (is (= 11 result) "Should find case-insensitively"))))

(deftest test-search-forward-with-bound
  (testing "search-forward respects bound parameter"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello World\")")
    (lisp/eval-lisp! "(goto-char 0)")
    ;; Search with bound at position 10 - should not find "World" at 6-11
    (let [result (lisp/eval-lisp! "(search-forward \"World\" 10)")]
      (is (nil? result) "Should not find match beyond bound"))))

(deftest test-search-forward-multiple-occurrences
  (testing "search-forward finds from current point"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"cat dog cat dog\")")
    (lisp/eval-lisp! "(goto-char 0)")
    ;; First search finds first "cat"
    (let [first-result (lisp/eval-lisp! "(search-forward \"cat\")")]
      (is (= 3 first-result) "First search should find 'cat' at 0-3")
      ;; Second search from current position finds second "cat"
      (let [second-result (lisp/eval-lisp! "(search-forward \"cat\")")]
        (is (= 11 second-result) "Second search should find 'cat' at 8-11")))))

;; =============================================================================
;; search-backward
;; =============================================================================

(deftest test-search-backward
  (testing "search-backward finds text and returns position at start of match"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello\")")
    (lisp/eval-lisp! "(goto-char 17)") ;; End of buffer
    (let [result (lisp/eval-lisp! "(search-backward \"World\")")]
      ;; "World" starts at 6
      (is (= 6 result) "Should return position at start of match")
      (is (= 6 (lisp/eval-lisp! "(point)")) "Point should be at start of match"))))

(deftest test-search-backward-not-found
  (testing "search-backward returns nil when text not found"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World\")")
    (lisp/eval-lisp! "(goto-char 11)") ;; End of buffer
    (let [result (lisp/eval-lisp! "(search-backward \"xyz\")")]
      (is (nil? result) "Should return nil when not found"))))

(deftest test-search-backward-case-insensitive
  (testing "search-backward is case-insensitive for lowercase patterns"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello WORLD hello\")")
    (lisp/eval-lisp! "(goto-char 17)")
    ;; lowercase "world" should match "WORLD"
    (let [result (lisp/eval-lisp! "(search-backward \"world\")")]
      (is (= 6 result) "Should find case-insensitively"))))

(deftest test-search-backward-with-bound
  (testing "search-backward respects bound parameter"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello World\")")
    (lisp/eval-lisp! "(goto-char 23)")
    ;; Search backward with bound at position 15 - should only find second "World"
    (let [result (lisp/eval-lisp! "(search-backward \"World\" 15)")]
      (is (= 18 result) "Should find match after bound"))))

(deftest test-search-backward-multiple-occurrences
  (testing "search-backward finds from current point backwards"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"cat dog cat dog\")")
    (lisp/eval-lisp! "(goto-char 14)") ;; End of buffer
    ;; First backward search finds last "cat"
    (let [first-result (lisp/eval-lisp! "(search-backward \"cat\")")]
      (is (= 8 first-result) "First backward search should find 'cat' at 8")
      ;; Second backward search finds first "cat"
      (let [second-result (lisp/eval-lisp! "(search-backward \"cat\")")]
        (is (= 0 second-result) "Second backward search should find 'cat' at 0")))))
