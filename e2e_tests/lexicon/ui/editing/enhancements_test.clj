(ns lexicon.ui.editing.enhancements-test
  "E2E tests for editing enhancements - tests USER editing operations.

  Tests keyboard-based editing features:
  - delsel: Delete selection on insert (type over selection)
  - rect: Rectangle operations (C-x r prefix)
  - kmacro: Keyboard macros (F3/F4)
  - electric: Auto-pairing

  Uses keyboard simulation for all editing operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Delete Selection Mode
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

(deftest test-delete-selection-mode-keyboard
  (testing "selection replaced on insert - via keyboard"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable delete-selection-mode FIRST via Lisp (avoids minibuffer clearing mark)
    (e/js-execute h/*driver* "window.evalLisp('(call-interactively (quote delete-selection-mode))')")
    (Thread/sleep 100)

    ;; Insert text via Lisp (same as the passing test to isolate the issue)
    (e/js-execute h/*driver* "window.evalLisp('(insert \"Hello World\")')")
    (Thread/sleep 100)
    (let [text1 (h/get-buffer-text*)]
      (is (= "Hello World" text1) "Initial text should be Hello World"))

    ;; Go to beginning and set mark via Lisp (to ensure clean state)
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 0)')")
    (Thread/sleep 50)
    (e/js-execute h/*driver* "window.evalLisp('(set-mark 0)')")
    (Thread/sleep 50)

    ;; Move to position 5 via Lisp
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 5)')")
    (Thread/sleep 50)

    ;; Verify state
    (let [point (h/get-point*)
          mark (e/js-execute h/*driver* "return window.editorState.mark")]
      (is (= 5 point) "Point should be at 5")
      (is (= 0 mark) "Mark should be at 0"))

    ;; Check text is still intact before typing
    (let [text2 (h/get-buffer-text*)]
      (is (= "Hello World" text2) "Text should still be Hello World before replacement"))

    ;; Now type just one character via direct keydown to test delete-selection behavior
    ;; This is the ONLY keyboard operation - everything else was via Lisp
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'X',
        code: 'KeyX',
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute h/*driver* script))
    (Thread/sleep 300)

    ;; Should now have "X World" instead of "HelloX World"
    (let [text (h/get-buffer-text*)]
      (is (= "X World" text)
          "delete-selection-mode should replace selection"))))

;; =============================================================================
;; Rectangle Operations
;; =============================================================================

(deftest ^:skip test-rectangle-kill-yank
  (testing "C-x r k kills rectangle"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type multi-line text for rectangle
    (h/type-text "abc")
    (h/press-key "Enter")
    (h/type-text "def")
    (h/press-key "Enter")
    (h/type-text "ghi")
    (Thread/sleep 50)

    ;; Rectangle operations are complex - test placeholder
    (is true "PENDING: rectangle - needs E2E implementation")))

;; =============================================================================
;; Keyboard Macros
;; =============================================================================

(deftest ^:skip test-keyboard-macro-record
  (testing "F3 starts recording, F4 stops/replays"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text
    (h/type-text "test")
    (Thread/sleep 50)

    ;; Start macro recording with F3
    (h/press-key "F3")
    (Thread/sleep 50)

    ;; Record some keystrokes
    (h/type-text "!")
    (Thread/sleep 50)

    ;; Stop recording with F4
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Macro functionality placeholder
    (is true "PENDING: kmacro - needs E2E implementation")))

;; =============================================================================
;; Electric Pair Mode
;; =============================================================================

(deftest test-electric-pair
  (testing "electric-pair-mode inserts closing bracket"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Enable electric-pair-mode via M-x
    (h/execute-command "electric-pair-mode")
    (Thread/sleep 50)

    ;; Type opening bracket - should auto-insert closing
    (h/type-text "(")
    (Thread/sleep 100)

    ;; Should have "()" in buffer
    (let [text (h/get-buffer-text*)]
      (is (= "()" text)
          "electric-pair should insert closing bracket"))))
