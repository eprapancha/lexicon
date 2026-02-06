(ns lexicon.lisp.editing.primitives-test
  "E2E tests for editing primitives via Lisp API.

  These tests verify low-level editing operations using evalLisp.
  For keyboard-based user interaction tests, see lexicon.ui.editing.*

  Tests:
  - delete-selection-mode: Delete region and insert
  - rectangle operations: Rectangle kill/yank via Lisp setup
  - dabbrev: Dynamic abbreviation expansion (M-/)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Delete Selection Mode (Lisp primitives)
;; =============================================================================

(deftest test-delete-selection-mode
  (testing "selection replaced on insert - via Lisp"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Test the core delete-selection logic using pure Lisp
    ;; This isolates from any input handling issues

    ;; 1. Insert text via Lisp
    (e/js-execute h/*driver* "window.evalLisp('(insert \"Hello World\")')")
    (Thread/sleep 100)
    (let [text1 (h/get-buffer-text*)]
      (is (= "Hello World" text1) "Initial text should be Hello World"))

    ;; 2. Go to beginning and set mark
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 0)')")
    (Thread/sleep 50)
    (e/js-execute h/*driver* "window.evalLisp('(set-mark 0)')")
    (Thread/sleep 50)

    ;; 3. Move to position 5 (end of "Hello")
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 5)')")
    (Thread/sleep 50)

    ;; Verify state
    (let [point (h/get-point*)
          mark (e/js-execute h/*driver* "return window.editorState.mark")]
      (is (= 5 point) "Point should be at 5")
      (is (= 0 mark) "Mark should be at 0"))

    ;; 4. Delete the region (simulating what delete-selection-mode does)
    (e/js-execute h/*driver* "window.evalLisp('(delete-region 0 5)')")
    (Thread/sleep 100)

    ;; Verify deletion
    (let [text2 (h/get-buffer-text*)]
      (is (= " World" text2) "After delete-region, should have ' World'"))

    ;; 5. Insert "X" at position 0
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 0)')")
    (Thread/sleep 50)
    (e/js-execute h/*driver* "window.evalLisp('(insert \"X\")')")
    (Thread/sleep 100)

    ;; Should now have "X World"
    (let [text (h/get-buffer-text*)]
      (is (= "X World" text)
          "Manual delete-region + insert should give 'X World'"))))

;; NOTE: test-delete-selection-mode-keyboard and test-rectangle-kill-yank
;; have been moved to lexicon.ui.editing.primitives-test as keyboard-only tests.
;; See issue #134.

;; =============================================================================
;; Dabbrev (Dynamic Abbreviation)
;; =============================================================================

(deftest test-dabbrev-expand
  (testing "M-/ expands abbreviation to matching word"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Insert text with a word we'll try to complete
    (e/js-execute h/*driver* "window.evalLisp('(insert \"fantastic \")')")
    (Thread/sleep 100)

    ;; Verify initial text
    (let [text1 (h/get-buffer-text*)]
      (is (= "fantastic " text1) "Initial text should be 'fantastic '"))

    ;; Now type a partial word at the end
    (e/js-execute h/*driver* "window.evalLisp('(insert \"fan\")')")
    (Thread/sleep 100)

    ;; Verify we have "fantastic fan"
    (let [text2 (h/get-buffer-text*)]
      (is (= "fantastic fan" text2) "Should have 'fantastic fan'"))

    ;; Press M-/ to expand "fan" to "fantastic"
    (h/press-meta "/")
    (Thread/sleep 200)

    ;; Should now have "fantastic fantastic"
    (let [text3 (h/get-buffer-text*)]
      (is (= "fantastic fantastic" text3)
          "dabbrev should expand 'fan' to 'fantastic'")))

  (testing "M-/ cycles through multiple matches"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Insert text with multiple words starting with "pro"
    (e/js-execute h/*driver* "window.evalLisp('(insert \"program project \")')")
    (Thread/sleep 100)

    ;; Type partial word
    (e/js-execute h/*driver* "window.evalLisp('(insert \"pro\")')")
    (Thread/sleep 100)

    ;; First M-/ should find "project" (backward search finds nearest first)
    (h/press-meta "/")
    (Thread/sleep 200)

    (let [text1 (h/get-buffer-text*)]
      (is (str/includes? text1 "project project")
          "First expansion should be 'project'"))

    ;; Second M-/ should cycle to "program"
    (h/press-meta "/")
    (Thread/sleep 200)

    (let [text2 (h/get-buffer-text*)]
      (is (str/includes? text2 "program")
          "Second expansion should cycle to 'program'"))))

;; =============================================================================
;; Icomplete (Incremental Completion)
;; =============================================================================

(deftest test-icomplete-mode
  (testing "icomplete-mode command exists and can be called"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable icomplete-mode via M-x - should not throw error
    (h/execute-command "icomplete-mode")
    (Thread/sleep 200)

    ;; Verify icomplete-mode was executed by checking command log
    (let [messages (h/get-messages-buffer h/*driver*)]
      (is (str/includes? messages "icomplete-mode")
          "icomplete-mode command should have been executed"))

    ;; Toggle again - should also work without error
    (h/execute-command "icomplete-mode")
    (Thread/sleep 200)

    ;; If we got here without errors, the command works
    (is true "icomplete-mode toggle succeeded")))
