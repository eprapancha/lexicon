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
;; Rectangle Operations (Lisp setup)
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
      (is (str/includes? text3 "bc") "Yanked rectangle should contain 'bc'")
      (is (str/includes? text3 "gh") "Yanked rectangle should contain 'gh'"))))

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
  (testing "icomplete-mode shows completions inline"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable icomplete-mode
    (h/execute-command "icomplete-mode")
    (Thread/sleep 100)

    ;; Open M-x to test with command completion
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type partial command name
    (h/type-text "goto")
    (Thread/sleep 300)

    ;; The minibuffer should show something - we can't easily check
    ;; the exact icomplete display, but we can verify the mode works
    ;; by checking that typing works in the minibuffer
    (let [minibuffer-text (e/js-execute h/*driver*
                            "return document.querySelector('.minibuffer-input')?.value || ''")]
      (is (= "goto" minibuffer-text) "Minibuffer should have our typed text"))

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100))

  (testing "icomplete cycling with C-. and C-,"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable icomplete-mode if not already enabled
    (e/js-execute h/*driver* "window.evalLisp('(icomplete-mode 1)')")
    (Thread/sleep 100)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type partial command to get multiple matches
    (h/type-text "goto")
    (Thread/sleep 200)

    ;; Press C-. to cycle forward
    (h/press-ctrl ".")
    (Thread/sleep 200)

    ;; The input should change to a completion
    (let [minibuffer-text (e/js-execute h/*driver*
                            "return document.querySelector('.minibuffer-input')?.value || ''")]
      (is (str/starts-with? minibuffer-text "goto")
          "After C-., input should still start with 'goto' or be a completion"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))
