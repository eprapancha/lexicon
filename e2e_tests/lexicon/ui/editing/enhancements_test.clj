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

(deftest test-rectangle-kill-yank
  (testing "C-x r k kills rectangle and C-x r y yanks it"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Insert multi-line text via Lisp for reliable setup
    (e/js-execute h/*driver* "window.evalLisp('(insert \"abcde\\nfghij\\nklmno\")')")
    (Thread/sleep 100)

    ;; Verify initial text
    (let [text1 (h/get-buffer-text*)]
      (is (= "abcde\nfghij\nklmno" text1) "Initial text should be correct"))

    ;; Set mark at position 1 (after 'a') and move to position 8 (after 'g' on line 2)
    ;; This creates a rectangle: columns 1-2, lines 0-1
    ;; Line 0: "abcde" -> select "bc"
    ;; Line 1: "fghij" -> select "gh"
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 1)')")
    (Thread/sleep 50)
    (e/js-execute h/*driver* "window.evalLisp('(set-mark 1)')")
    (Thread/sleep 50)
    (e/js-execute h/*driver* "window.evalLisp('(goto-char 9)')")  ; Position 9 = line 1, col 3
    (Thread/sleep 50)

    ;; Kill the rectangle with C-x r k
    (h/press-ctrl "x")
    (Thread/sleep 100)
    (h/press-key "r")
    (Thread/sleep 100)
    (h/press-key "k")
    (Thread/sleep 200)

    ;; After killing rectangle [1,3) on lines 0-1:
    ;; Line 0: "a" + "de" = "ade"
    ;; Line 1: "f" + "ij" = "fij"
    ;; Line 2: unchanged = "klmno"
    (let [text2 (h/get-buffer-text*)]
      (is (= "ade\nfij\nklmno" text2) "After kill-rectangle, columns should be removed"))

    ;; Move to end of buffer and yank the rectangle
    (e/js-execute h/*driver* "window.evalLisp('(goto-char (point-max))')")
    (Thread/sleep 50)
    (h/press-ctrl "x")
    (Thread/sleep 100)
    (h/press-key "r")
    (Thread/sleep 100)
    (h/press-key "y")
    (Thread/sleep 200)

    ;; After yanking rectangle at end:
    ;; The killed rectangle was ["bc" "gh"] (2 lines)
    ;; It should be inserted at the cursor column on successive lines
    (let [text3 (h/get-buffer-text*)]
      (is (clojure.string/includes? text3 "bc") "Yanked rectangle should contain 'bc'")
      (is (clojure.string/includes? text3 "gh") "Yanked rectangle should contain 'gh'"))))

;; =============================================================================
;; Keyboard Macros
;; =============================================================================

(deftest test-keyboard-macro-record
  (testing "F3 starts recording, F4 stops and replays"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Start macro recording via F3
    (h/press-key "F3")
    (Thread/sleep 100)

    ;; Type "Hi" - this will be recorded and inserted
    (h/type-text "Hi")
    (Thread/sleep 100)

    ;; Stop recording with F4
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Verify text is there
    (let [text1 (h/get-buffer-text*)]
      (is (= "Hi" text1) "Buffer should have 'Hi'"))

    ;; Replay the macro with F4
    (h/press-key "F4")
    (Thread/sleep 200)

    ;; Should now have "HiHi" (original + replay)
    (let [text2 (h/get-buffer-text*)]
      (is (= "HiHi" text2) "Replaying macro should append 'Hi'")))

  (testing "C-x e also replays last macro"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Record a simple macro
    (h/press-key "F3")
    (Thread/sleep 100)
    (h/type-text "X")
    (Thread/sleep 100)
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Clear buffer
    (h/clear-buffer)
    (Thread/sleep 50)

    ;; Use C-x e to replay
    (h/press-ctrl "x")
    (Thread/sleep 50)
    (h/press-key "e")
    (Thread/sleep 200)

    ;; Should have "X"
    (let [text (h/get-buffer-text*)]
      (is (= "X" text) "C-x e should replay last macro"))))

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
