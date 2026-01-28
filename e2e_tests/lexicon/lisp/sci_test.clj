(ns lexicon.lisp.sci-test
  "Core integration tests for SCI (Small Clojure Interpreter).

  This file tests SCI as a product feature - verifying that the Lisp
  evaluation system works correctly. It focuses on:
  - Basic evaluation (eval-string)
  - Error handling (syntax errors, runtime errors)
  - API exposure verification (functions are callable)

  Feature-specific Lisp API tests are in separate namespaces:
  - lexicon.lisp.buffer-test: Buffer operations (buffer-modified-p, etc.)
  - lexicon.lisp.editing-test: Editing macros (save-excursion, navigation)
  - lexicon.lisp.command-test: Command system (defcommand, prefix-arg)
  - lexicon.lisp.security-test: Security constraints (js/eval blocking)

  Related: Issue #106 (SCI Integration), Issue #94 (TDD)
  Priority: CRITICAL - foundation for Emacs Lisp compatibility"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Evaluation
;; =============================================================================

(deftest test-eval-string-basic
  (testing "eval-string returns numeric result"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(+ 1 2)")]
      (is (= 3 result))))

  (testing "eval-string with multiple forms returns last"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(setq x 5) (+ x 3)")]
      (is (= 8 result))))

  (testing "eval-string with string result"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp! "(str \"hello\" \" \" \"world\")")]
      (is (= "hello world" result)))))

;; =============================================================================
;; Error Handling
;; =============================================================================

(deftest test-eval-string-error-handling
  (testing "syntax error returns error"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(+ 1")]  ; Missing closing paren
      (is (not (:success result))
          "Should return error indicator")))

  (testing "runtime error returns error"
    (lisp/setup-test)
    (let [result (lisp/eval-lisp "(error \"test error\")")]
      (is (not (:success result))
          "Should handle gracefully"))))

;; =============================================================================
;; API Exposure Verification
;; =============================================================================
;; These tests verify that core Lisp functions are exposed and callable.
;; Detailed behavior tests are in the lexicon.lisp.* namespaces.

(deftest test-core-buffer-api-exposed
  (testing "point function is exposed"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (let [result (lisp/eval-lisp! "(point)")]
      (is (number? result) "point should return a number")))

  (testing "goto-char function is exposed"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (is (= 0 (lisp/eval-lisp! "(point)")) "goto-char should work"))

  (testing "insert function is exposed"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"test\")")
    (is (= "test" (lisp/eval-lisp! "(buffer-string)")) "insert should work"))

  (testing "buffer-string function is exposed"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello\")")
    (let [result (lisp/eval-lisp! "(buffer-string)")]
      (is (= "Hello" result) "buffer-string should return content"))))

(deftest test-core-search-api-exposed
  (testing "search-forward is exposed"
    (lisp/setup-test)
    (lisp/eval-lisp! "(insert \"Hello World Hello\")")
    (lisp/eval-lisp! "(goto-char 0)")
    (let [result (lisp/eval-lisp! "(search-forward \"World\")")]
      ;; Should return position after match or nil
      (is (or (number? result) (nil? result))
          "search-forward should return position or nil"))))
