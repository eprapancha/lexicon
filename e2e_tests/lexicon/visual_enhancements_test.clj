(ns lexicon.visual-enhancements-test
  "E2E tests for visual enhancements - user-visible typing behavior.

  Note: Visual modes (show-paren-mode, hl-line-mode, etc.) are Lisp APIs.
  E2E tests focus on user-visible typing of code.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Typing with Parentheses
;; =============================================================================

(deftest test-user-types-parenthesized-code
  (testing "User can type parenthesized expressions"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types parenthesized expression
    (h/type-text "(foo (bar baz))")
    (Thread/sleep 100)

    (is (= "(foo (bar baz))"
           (h/get-buffer-text*))
        "User can type balanced parens")))

(deftest test-user-types-with-whitespace
  (testing "User can type text with whitespace"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with tabs and trailing spaces
    (h/type-text "hello")
    (h/press-key "Tab")
    (h/type-text "world  ")
    (Thread/sleep 100)

    (is (= "hello\tworld  "
           (h/get-buffer-text*))
        "User can type text with whitespace")))

;; =============================================================================
;; Show Paren Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-show-paren-mode
  (testing "show-paren-mode highlights matching parens"
    ;; show-paren-mode is a Lisp function
    (is false "show-paren-mode tested via unit tests")))

;; =============================================================================
;; Hl-Line Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-hl-line-mode
  (testing "hl-line-mode highlights current line"
    ;; hl-line-mode is a Lisp function
    (is false "hl-line-mode tested via unit tests")))

;; =============================================================================
;; Whitespace Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-whitespace-mode
  (testing "whitespace-mode shows spaces/tabs"
    ;; whitespace-mode is a Lisp function
    (is false "whitespace-mode tested via unit tests")))

;; =============================================================================
;; Line Numbers - Placeholders for Unit Tests
;; =============================================================================

(deftest test-line-numbers
  (testing "display-line-numbers-mode works"
    ;; display-line-numbers-mode is a Lisp function
    (is false "display-line-numbers-mode tested via unit tests")))
