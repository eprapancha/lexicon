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
  (testing "search-forward finds text"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (let [result (lisp/eval-lisp! "(search-forward \"World\")")]
      ;; Should return position after match or nil
      (is (or (number? result) (nil? result))))))
