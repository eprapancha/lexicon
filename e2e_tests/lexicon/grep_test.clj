(ns lexicon.grep-test
  "E2E tests for grep and regexp highlighting - user-visible behavior.

  Note: grep, hi-lock, occur are Lisp APIs. E2E tests focus on
  user-visible typing of searchable content.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Content Typing
;; =============================================================================

(deftest test-user-types-searchable-content
  (testing "User can type content with repeated patterns"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with repeated pattern
    (h/type-text "foo bar foo baz")
    (Thread/sleep 100)

    (is (= "foo bar foo baz"
           (h/get-buffer-text*))
        "User can type content with patterns")))

(deftest test-user-types-multiline-content
  (testing "User can type multi-line content for occur"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types multi-line content
    (h/type-text "line1 foo")
    (h/press-key "Enter")
    (h/type-text "line2")
    (h/press-key "Enter")
    (h/type-text "line3 foo")
    (Thread/sleep 100)

    (is (= "line1 foo\nline2\nline3 foo"
           (h/get-buffer-text*))
        "User can type multiline content")))

;; =============================================================================
;; Grep Command - Placeholders for Unit Tests
;; =============================================================================

(deftest test-grep-command
  (testing "grep creates grep buffer"
    ;; grep is a Lisp function
    (is false "grep tested via unit tests")))

(deftest test-grep-find-project
  (testing "grep-find searches recursively"
    ;; grep-find is a Lisp function
    (is false "grep-find tested via unit tests")))

;; =============================================================================
;; Hi-Lock Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-hi-lock-mode
  (testing "hi-lock-mode can be enabled"
    ;; hi-lock-mode is a Lisp function
    (is false "hi-lock-mode tested via unit tests")))

(deftest test-highlight-regexp
  (testing "highlight-regexp highlights matches"
    ;; highlight-regexp is a Lisp function
    (is false "highlight-regexp tested via unit tests")))

;; =============================================================================
;; Occur - Placeholders for Unit Tests
;; =============================================================================

(deftest test-occur
  (testing "occur creates occur buffer"
    ;; occur is a Lisp function
    (is false "occur tested via unit tests")))
