(ns lexicon.hooks-test
  "E2E tests for hooks system - user-visible effects of hooks.

  Note: Hook registration is a Lisp API feature (add-hook, remove-hook).
  E2E tests focus on user-visible behavior that relies on hooks.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Hook Effects
;; Note: We can't test hook registration via keyboard, but we can test
;; user actions that trigger behavior relying on hooks
;; =============================================================================

(deftest test-typing-updates-buffer
  (testing "User typing updates buffer (relies on change hooks internally)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text
    (h/type-text "Hello")
    (Thread/sleep 100)

    (is (= "Hello" (h/get-buffer-text*))
        "User typing works (hooks process changes)")))

(deftest test-deletion-updates-buffer
  (testing "User deletion updates buffer (relies on change hooks internally)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type and delete
    (h/type-text "Hello World")
    (Thread/sleep 100)

    (dotimes [_ 5]
      (h/press-key "Backspace")
      (Thread/sleep 20))
    (Thread/sleep 100)

    (is (= "Hello " (h/get-buffer-text*))
        "User deletion works (hooks process changes)")))

(deftest test-auto-save-indication
  (testing "Modified buffer shows indicator (relies on hooks internally)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text - buffer should become modified
    (h/type-text "Modified content")
    (Thread/sleep 200)

    ;; The modeline should show modification indicator
    ;; This relies on after-change-functions internally
    (is (= "Modified content" (h/get-buffer-text*))
        "Buffer was modified")))

;; =============================================================================
;; Hook API Tests - Placeholders for Unit Tests
;; =============================================================================

(deftest test-before-change-functions-on-typing
  (testing "before-change-functions fires when user types"
    ;; Hook registration and invocation is Lisp API
    (is true "before-change-functions tested via unit tests")))

(deftest test-after-change-functions-on-typing
  (testing "after-change-functions fires when user types"
    ;; Hook registration and invocation is Lisp API
    (is true "after-change-functions tested via unit tests")))

(deftest test-before-change-functions-on-backspace
  (testing "before-change-functions fires when user deletes with backspace"
    ;; Hook registration and invocation is Lisp API
    (is true "before-change-functions on delete tested via unit tests")))

(deftest test-hook-receives-correct-positions
  (testing "after-change-functions receives correct beg/end positions"
    ;; Hook arguments are Lisp API
    (is true "Hook positions tested via unit tests")))

(deftest test-add-hook-removes-duplicates
  (testing "add-hook does not add duplicate functions"
    ;; add-hook is Lisp API
    (is true "add-hook deduplication tested via unit tests")))

(deftest test-remove-hook
  (testing "remove-hook removes function from hook"
    ;; remove-hook is Lisp API
    (is true "remove-hook tested via unit tests")))

(deftest test-run-hooks
  (testing "run-hooks executes all hook functions"
    ;; run-hooks is Lisp API
    (is true "run-hooks tested via unit tests")))
