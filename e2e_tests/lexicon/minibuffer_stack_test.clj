(ns lexicon.minibuffer-stack-test
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
            [etaoin.api :as e]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000) ;; 10 seconds

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown
(defn start-driver []
  (e/firefox {:headless true}))

(defn stop-driver [driver]
  (when driver
    (e/quit driver)))

(defn with-driver [f]
  (let [driver (start-driver)]
    (try
      (binding [*driver* driver]
        (f))
      (finally
        (stop-driver driver)))))

(use-fixtures :once with-driver)

;; Helper functions
(defn wait-for-editor-ready []
  "Wait for editor to be ready by checking for .editor-wrapper"
  (e/wait-visible *driver* {:css ".editor-wrapper"} {:timeout (/ test-timeout 1000)}))

(defn click-editor []
  "Click the editor to focus it"
  (e/click *driver* {:css ".editor-wrapper"}))

(defn get-editor-text []
  "Get text content from the editable area"
  (e/get-element-text *driver* {:css ".editable-area"}))

(defn type-text
  "Type text with delay between characters"
  [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

(defn press-alt-key
  "Press Alt+key combination (M-x)"
  [key]
  (let [script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-ctrl-key
  "Press Ctrl+key combination"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-key
  "Press a non-modifier key"
  [key]
  (e/fill *driver* {:css ".hidden-input"} key)
  (Thread/sleep 50))

(defn press-enter []
  "Press Enter key in minibuffer"
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        keyCode: 13,
        which: 13,
        bubbles: true,
        cancelable: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 200))

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
    (e/js-execute *driver* script))
  (Thread/sleep 100))

(defn get-minibuffer-depth []
  "Get current minibuffer depth from window.editorState"
  (e/js-execute *driver* "
    const state = window.editorState;
    if (!state || !state.minibufferStack) return 0;
    return state.minibufferStack.length;
  "))

(defn get-minibuffer-prompt []
  "Get current minibuffer prompt"
  (try
    (e/get-element-text *driver* {:css ".minibuffer-prompt"})
    (catch Exception e
      nil)))

(defn minibuffer-visible? []
  "Check if minibuffer is visible"
  (e/exists? *driver* {:css ".minibuffer-input"}))

(defn wait-for-minibuffer []
  "Wait for minibuffer to be visible"
  (e/wait-visible *driver* {:css ".minibuffer-input"} {:timeout 2}))

(defn get-mode-line-text []
  "Get mode-line text"
  (try
    (e/get-element-text *driver* {:css ".mode-line"})
    (catch Exception e
      "")))

;; =============================================================================
;; Test Suite: Issue #72 - M-x query-replace-regexp
;; =============================================================================

(deftest test-issue-72-query-replace-regexp
  (testing "Issue #72: M-x query-replace-regexp activates minibuffer without timeout"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type some text to replace
    (type-text "hello world hello")
    (Thread/sleep 100)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 50)

    ;; Execute M-x
    (press-alt-key "x")
    (Thread/sleep 100)

    ;; Verify M-x minibuffer appeared
    (is (= 1 (get-minibuffer-depth)) "M-x should activate minibuffer (depth 1)")
    (is (minibuffer-visible?) "Minibuffer should be visible")

    ;; Type query-replace-regexp
    (type-text "query-replace-regexp")
    (Thread/sleep 100)
    (press-enter)

    ;; Wait for command's minibuffer to appear (should replace M-x's frame)
    (Thread/sleep 200)

    ;; Verify minibuffer is still active and prompt changed
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 (replaced frame)")
    (is (minibuffer-visible?) "Minibuffer should still be visible")

    (let [prompt (get-minibuffer-prompt)]
      (is (str/includes? (str/lower-case (or prompt "")) "query replace regexp")
          (str "Prompt should mention query-replace-regexp, but got: " prompt)))))

;; =============================================================================
;; Test Suite: M-x → Command Transition (Frame Replacement)
;; =============================================================================

(deftest test-mx-replace-string-transition
  (testing "M-x replace-string transitions by replacing frame (depth stays 1)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "foo bar foo")
    (Thread/sleep 100)
    (press-ctrl-key "a")
    (Thread/sleep 50)

    ;; M-x
    (press-alt-key "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "M-x should set depth to 1")

    ;; Type replace-string
    (type-text "replace-string")
    (press-enter)
    (Thread/sleep 200)

    ;; Depth should still be 1 (frame replaced)
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 after frame replacement")

    (let [prompt (get-minibuffer-prompt)]
      (is (str/includes? (str/lower-case (or prompt "")) "replace string")
          (str "Prompt should change to replace-string, but got: " prompt)))))

;; =============================================================================
;; Test Suite: Stack Depth Tracking
;; =============================================================================

(deftest test-minibuffer-depth-inactive
  (testing "Minibuffer depth is 0 when inactive"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Initially depth should be 0
    (is (= 0 (get-minibuffer-depth)) "Depth should be 0 when inactive")))

(deftest test-minibuffer-depth-active
  (testing "Minibuffer depth is 1 when M-x is active"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Activate M-x
    (press-alt-key "x")
    (Thread/sleep 100)

    (is (= 1 (get-minibuffer-depth)) "Depth should be 1 when M-x is active")))

(deftest test-minibuffer-cancel-restores-depth
  (testing "Canceling minibuffer restores depth to 0"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Activate M-x
    (press-alt-key "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "Depth should be 1")

    ;; Cancel with Escape
    (press-escape)
    (Thread/sleep 100)

    (is (= 0 (get-minibuffer-depth)) "Depth should return to 0 after cancel")))

;; =============================================================================
;; Test Suite: Recursive Minibuffer (when enabled)
;; =============================================================================

(deftest test-recursive-minibuffer-blocked-by-default
  (testing "Recursive minibuffer is blocked when enable-recursive-minibuffers is false (default)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Activate M-x
    (press-alt-key "x")
    (Thread/sleep 100)
    (is (= 1 (get-minibuffer-depth)) "Depth should be 1")

    ;; Try to activate another M-x (should be blocked)
    (press-alt-key "x")
    (Thread/sleep 100)

    ;; Depth should still be 1 (not nested)
    (is (= 1 (get-minibuffer-depth)) "Depth should remain 1 (recursive blocked)")))

;; =============================================================================
;; Test Suite: Mode-line Depth Indicator
;; =============================================================================

(deftest test-mode-line-no-depth-indicator-when-inactive
  (testing "Mode-line has no depth indicator when minibuffer is inactive"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    (let [mode-line (get-mode-line-text)]
      (is (not (str/includes? mode-line "[2]")) "Should not show [2] when depth is 0")
      (is (not (str/includes? mode-line "[1]")) "Should not show [1] when depth is 0"))))

(deftest test-mode-line-no-depth-indicator-at-depth-1
  (testing "Mode-line has no depth indicator when depth is 1 (only shows when > 1)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Activate M-x (depth 1)
    (press-alt-key "x")
    (Thread/sleep 100)

    (let [mode-line (get-mode-line-text)]
      (is (not (str/includes? mode-line "[1]")) "Should not show [1] at depth 1")
      (is (not (str/includes? mode-line "[2]")) "Should not show [2] at depth 1"))))

;; Note: Testing depth > 1 requires enable-recursive-minibuffers to be true,
;; which would need to be set via configuration or eval-expression.
;; This is left as a future enhancement once we have a way to set variables in E2E tests.

(comment
  (deftest test-mode-line-depth-indicator-at-depth-2
    (testing "Mode-line shows [2] when minibuffer depth is 2"
      ;; Future test - requires setting enable-recursive-minibuffers to true
      ;; and nesting minibuffers
      )))
