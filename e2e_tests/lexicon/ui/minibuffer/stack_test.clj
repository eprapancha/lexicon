(ns lexicon.ui.minibuffer.stack-test
  "Phase 6.5 Week 3-4: Minibuffer Stack Tests - E2E with Etaoin

  Tests minibuffer stack functionality:
  - Issue #72: M-x query-replace-regexp minibuffer activation
  - Recursive minibuffer when enabled
  - Recursive minibuffer blocked when disabled
  - Stack depth tracking
  - Frame replacement (:replace? true)
  - Depth indicator in mode-line

  Based on Issue #77 acceptance criteria."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; Helper functions (test-specific)
(defn get-minibuffer-depth []
  "Get current minibuffer depth from window.editorState"
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state || !state.minibufferStack) return 0;
    return state.minibufferStack.length;
  "))

(defn press-escape []
  "Press Escape key - tries minibuffer first, then hidden-input"
  (let [script "
    // Try minibuffer input first
    let input = document.querySelector('.minibuffer-input');
    if (!input) {
      // Fall back to hidden input
      input = document.querySelector('.hidden-input');
    }
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Escape',
        code: 'Escape',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 100))

(defn get-mode-line-text []
  "Get mode-line text"
  (try
    (e/get-element-text h/*driver* {:css ".mode-line"})
    (catch Exception e
      "")))

;; =============================================================================
;; Test Suite: Issue #72 - M-x query-replace-regexp
;; =============================================================================

(deftest test-issue-72-query-replace-regexp
  (testing "Issue #72: M-x query-replace-regexp activates minibuffer without timeout"
    (h/setup-test*)

    ;; Type some text to replace
    (h/type-text "hello world hello")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Execute M-x
    (h/press-meta "x")
    (Thread/sleep 100)

    ;; Verify M-x minibuffer appeared
    (is (= 1 (get-minibuffer-depth)) "M-x should activate minibuffer (depth 1)")
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Type query-replace-regexp into minibuffer
    (h/type-in-minibuffer "query-replace-regexp")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)

    ;; Wait for command's minibuffer to appear (should replace M-x's frame)
    (Thread/sleep 200)

    ;; Verify minibuffer is still active and prompt changed
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 (replaced frame)")
    (is (h/minibuffer-visible?) "Minibuffer should still be visible")

    (let [prompt (h/get-minibuffer-text)]
      (is (str/includes? (str/lower-case (or prompt "")) "query replace regexp")
          (str "Prompt should mention query-replace-regexp, but got: " prompt)))))

;; =============================================================================
;; Test Suite: M-x → Command Transition (Frame Replacement)
;; =============================================================================

(deftest test-mx-replace-string-transition
  (testing "M-x replace-string transitions by replacing frame (depth stays 1)"
    (h/setup-test*)

    ;; Type text
    (h/type-text "foo bar foo")
    (Thread/sleep 100)
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; M-x
    (h/press-meta "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "M-x should set depth to 1")

    ;; Type replace-string into minibuffer
    (h/type-in-minibuffer "replace-string")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Depth should still be 1 (frame replaced)
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 after frame replacement")

    (let [prompt (h/get-minibuffer-text)]
      (is (str/includes? (str/lower-case (or prompt "")) "replace string")
          (str "Prompt should change to replace-string, but got: " prompt)))))

;; =============================================================================
;; Test Suite: Stack Depth Lifecycle (Workflow Test)
;; Subsumes: test-minibuffer-depth-inactive, test-minibuffer-depth-active,
;;           test-minibuffer-cancel-restores-depth,
;;           test-mode-line-no-depth-indicator-when-inactive,
;;           test-mode-line-no-depth-indicator-at-depth-1
;; =============================================================================

(deftest test-minibuffer-depth-lifecycle
  (testing "Minibuffer depth tracking through activation cycle"
    (h/setup-test*)

    ;; === Part 1: Depth is 0 when inactive (subsumes test-minibuffer-depth-inactive) ===
    (is (= 0 (get-minibuffer-depth)) "Depth should be 0 when inactive")

    ;; === Part 2: No depth indicator when inactive (subsumes test-mode-line-no-depth-indicator-when-inactive) ===
    (let [mode-line (get-mode-line-text)]
      (is (not (str/includes? mode-line "[1]")) "Should not show [1] when depth is 0")
      (is (not (str/includes? mode-line "[2]")) "Should not show [2] when depth is 0"))

    ;; === Part 3: Depth is 1 when M-x is active (subsumes test-minibuffer-depth-active) ===
    (h/press-meta "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "Depth should be 1 when M-x is active")

    ;; === Part 4: No depth indicator at depth 1 (subsumes test-mode-line-no-depth-indicator-at-depth-1) ===
    (let [mode-line (get-mode-line-text)]
      (is (not (str/includes? mode-line "[1]")) "Should not show [1] at depth 1")
      (is (not (str/includes? mode-line "[2]")) "Should not show [2] at depth 1"))

    ;; === Part 5: Cancel restores depth to 0 (subsumes test-minibuffer-cancel-restores-depth) ===
    (press-escape)
    (Thread/sleep 100)
    (is (= 0 (get-minibuffer-depth)) "Depth should return to 0 after cancel")))

;; =============================================================================
;; Test Suite: Recursive Minibuffer (when enabled)
;; =============================================================================

(deftest test-recursive-minibuffer-blocked-by-default
  (testing "Recursive minibuffer is blocked when enable-recursive-minibuffers is false (default)"
    (h/setup-test*)

    ;; Activate M-x
    (h/press-meta "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "Depth should be 1")

    ;; Try to activate another M-x (should be blocked)
    (h/press-meta "x")
    (Thread/sleep 100)

    ;; Depth should still be 1 (not nested)
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 (recursive blocked)")))

;; NOTE: Mode-line depth indicator tests removed - subsumed by test-minibuffer-depth-lifecycle

;; Note: Testing depth > 1 requires enable-recursive-minibuffers to be true,
;; which would need to be set via configuration or eval-expression.
;; This is left as a future enhancement once we have a way to set variables in E2E tests.

(comment
  (deftest test-mode-line-depth-indicator-at-depth-2
    (testing "Mode-line shows [2] when minibuffer depth is 2"
      ;; Future test - requires setting enable-recursive-minibuffers to true
      ;; and nesting minibuffers
      )))
