(ns lexicon.lisp.hooks-test
  "Lisp API tests for hooks system.

  Tests hook-related Lisp functions:
  - add-hook: Add function to a hook
  - remove-hook: Remove function from a hook
  - run-hooks: Execute all functions on a hook
  - after-change-functions: Called when buffer content changes

  JUSTIFICATION: Hooks are Lisp entities. Testing hook registration,
  ordering, execution, and integration with keyboard input requires Lisp evaluation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Basic Hook Registration (verified via behavior)
;; Note: Hooks are stored in a registry, not Lisp variables.
;; We verify add-hook works by checking run-hooks behavior.
;; =============================================================================

(deftest test-add-hook-basic
  (testing "add-hook adds function that runs"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq hook-ran false)")
    ;; Add a function
    (lisp/eval-lisp! "(add-hook 'test-hook-basic (fn [] (setq hook-ran true)))")
    ;; Run the hook
    (lisp/eval-lisp! "(run-hooks 'test-hook-basic)")
    ;; Verify function ran
    (is (true? (lisp/eval-lisp! "hook-ran")) "Hook function should have run")))

(deftest test-add-hook-multiple
  (testing "add-hook adds multiple functions that all run"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq counter 0)")
    ;; Add two functions
    (lisp/eval-lisp! "(add-hook 'test-hook-multi (fn [] (setq counter (+ counter 1))))")
    (lisp/eval-lisp! "(add-hook 'test-hook-multi (fn [] (setq counter (+ counter 10))))")
    ;; Run the hook
    (lisp/eval-lisp! "(run-hooks 'test-hook-multi)")
    ;; Both should have run
    (is (= 11 (lisp/eval-lisp! "counter")) "Both hook functions should have run")))

(deftest test-add-hook-order
  (testing "add-hook appends functions (first added runs first)"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq test-result [])")
    ;; Add functions that record their order
    (lisp/eval-lisp! "(add-hook 'test-hook-order (fn [] (setq test-result (conj test-result \"first\"))))")
    (lisp/eval-lisp! "(add-hook 'test-hook-order (fn [] (setq test-result (conj test-result \"second\"))))")
    ;; Run hooks
    (lisp/eval-lisp! "(run-hooks 'test-hook-order)")
    ;; First added runs first (append order)
    (let [result (lisp/eval-lisp! "test-result")]
      (is (= ["first" "second"] result) "Functions should run in order added"))))

;; =============================================================================
;; Run Hooks
;; =============================================================================

(deftest test-run-hooks-executes-all
  (testing "run-hooks executes all functions"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq test-hook nil)")
    (lisp/eval-lisp! "(setq counter 0)")
    (lisp/eval-lisp! "(add-hook 'test-hook (fn [] (setq counter (+ counter 1))))")
    (lisp/eval-lisp! "(add-hook 'test-hook (fn [] (setq counter (+ counter 10))))")
    (lisp/eval-lisp! "(run-hooks 'test-hook)")
    (let [result (lisp/eval-lisp! "counter")]
      (is (= 11 result) "Both functions should execute"))))

(deftest test-run-hooks-empty-hook
  (testing "run-hooks on empty hook is no-op"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq test-hook nil)")
    ;; Should not error
    (let [result (lisp/eval-lisp "(run-hooks 'test-hook)")]
      (is (:success result) "Running empty hook should succeed"))))

(deftest test-run-hooks-undefined-hook
  (testing "run-hooks on undefined hook is no-op"
    (lisp/setup-test)
    ;; Should not error
    (let [result (lisp/eval-lisp "(run-hooks 'undefined-hook-xyz)")]
      (is (:success result) "Running undefined hook should succeed"))))

;; =============================================================================
;; Remove Hook
;; =============================================================================

(deftest test-remove-hook-basic
  (testing "remove-hook removes function from hook"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq counter 0)")
    ;; Define a named function we can remove
    (lisp/eval-lisp! "(defn my-hook-fn [] (setq counter (+ counter 1)))")
    (lisp/eval-lisp! "(add-hook 'test-hook-remove my-hook-fn)")
    ;; Verify it runs
    (lisp/eval-lisp! "(run-hooks 'test-hook-remove)")
    (is (= 1 (lisp/eval-lisp! "counter")) "Hook should have run once")
    ;; Remove and run again
    (lisp/eval-lisp! "(remove-hook 'test-hook-remove my-hook-fn)")
    (lisp/eval-lisp! "(run-hooks 'test-hook-remove)")
    (is (= 1 (lisp/eval-lisp! "counter")) "Counter should still be 1 (hook removed)")))

;; =============================================================================
;; Hook with Buffer Changes
;; =============================================================================

(deftest test-hook-with-insert
  (testing "Hook can insert text into buffer"
    (lisp/setup-test)
    (lisp/eval-lisp! "(setq test-hook nil)")
    (lisp/eval-lisp! "(add-hook 'test-hook (fn [] (insert \"hooked\")))")
    (lisp/eval-lisp! "(run-hooks 'test-hook)")
    (let [content (lisp/eval-lisp! "(buffer-string)")]
      (is (= "hooked" content) "Hook should have inserted text"))))

;; =============================================================================
;; Keyboard Input Triggers Change Hooks
;; These tests verify that user keyboard actions trigger before/after-change hooks
;; =============================================================================

(deftest test-after-change-functions-on-typing
  (testing "after-change-functions fires when user types"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Register a hook that records it was called
    (lisp/eval-lisp! "(setq hook-called false)")
    (lisp/eval-lisp! "(add-hook 'after-change-functions (fn [beg end len] (setq hook-called true)))")

    ;; User types text
    (h/type-text "Hello")
    (Thread/sleep 200)

    ;; Verify hook was called
    (is (true? (lisp/eval-lisp! "hook-called"))
        "after-change-functions should fire on keyboard input")))

(deftest test-after-change-functions-on-backspace
  (testing "after-change-functions fires when user deletes"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text first
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; Register hook
    (lisp/eval-lisp! "(setq delete-hook-called false)")
    (lisp/eval-lisp! "(add-hook 'after-change-functions (fn [beg end len] (setq delete-hook-called true)))")

    ;; User deletes with backspace
    (h/press-key "Backspace")
    (Thread/sleep 200)

    ;; Verify hook was called
    (is (true? (lisp/eval-lisp! "delete-hook-called"))
        "after-change-functions should fire on backspace")))

(deftest test-hook-receives-positions
  (testing "after-change-functions receives position arguments"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Register hook that records positions
    (lisp/eval-lisp! "(setq hook-args nil)")
    (lisp/eval-lisp! "(add-hook 'after-change-functions (fn [beg end len] (setq hook-args [beg end len])))")

    ;; User types at beginning
    (h/type-text "Hi")
    (Thread/sleep 200)

    ;; Verify hook received arguments
    (let [args (lisp/eval-lisp! "hook-args")]
      (is (vector? args) "Hook should receive arguments")
      (is (= 3 (count args)) "Hook should receive 3 arguments (beg end len)"))))

