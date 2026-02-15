(ns lexicon.ui.editing.prefix-argument-test
  "Phase 6.5 Week 1-2: Prefix Argument (C-u) Tests - E2E with Etaoin

  Tests prefix argument accumulation and command modification:
  - C-u alone → (4)
  - C-u C-u → (16)
  - C-u 5 → 5
  - C-u - → '-
  - C-u - 5 → -5
  - Interactive spec 'p' conversion
  - Interactive spec 'P' raw form

  Based on Issue #76 acceptance criteria."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; Helper functions (test-specific)
(defn press-minus
  "Press the minus key using keyboard event simulation"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    if (input) {
      input.focus();
      const keydownEvent = new KeyboardEvent('keydown', {
        key: '-',
        code: 'Minus',
        bubbles: true,
        cancelable: true
      });
      input.dispatchEvent(keydownEvent);
      // Update input value and dispatch input event
      const nativeInputValueSetter = Object.getOwnPropertyDescriptor(
        window.HTMLInputElement.prototype, 'value'
      ).set || Object.getOwnPropertyDescriptor(
        window.HTMLTextAreaElement.prototype, 'value'
      ).set;
      if (nativeInputValueSetter) {
        nativeInputValueSetter.call(input, input.value + '-');
      } else {
        input.value = input.value + '-';
      }
      input.dispatchEvent(new Event('input', {bubbles: true}));
      const keyupEvent = new KeyboardEvent('keyup', {
        key: '-',
        code: 'Minus',
        bubbles: true,
        cancelable: true
      });
      input.dispatchEvent(keyupEvent);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn get-prefix-arg
  "Get current prefix-arg from window.editorState"
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state) return null;
    return state.prefixArg || null;
  "))

;; =============================================================================
;; Test Suite: Prefix Argument Accumulation
;; =============================================================================

(deftest test-cu-alone
  (testing "C-u alone sets prefix-arg to (4)"
    (h/setup-test*)

    ;; Press C-u
    (h/press-ctrl "u")
    (Thread/sleep 50)

    ;; Check prefix-arg is (4) - represented as a list/array in JS
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 4 (first prefix-arg)) "prefix-arg should be (4)"))))

(deftest test-cu-cu
  (testing "C-u C-u sets prefix-arg to (16)"
    (h/setup-test*)

    ;; Press C-u C-u
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-ctrl "u")
    (Thread/sleep 50)

    ;; Check prefix-arg is (16)
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 16 (first prefix-arg)) "prefix-arg should be (16)"))))

(deftest test-cu-digit
  (testing "C-u 5 sets prefix-arg to 5"
    (h/setup-test*)

    ;; Press C-u 5
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)

    ;; Check prefix-arg is 5 (number)
    (let [prefix-arg (get-prefix-arg)]
      (is (number? prefix-arg) "prefix-arg should be a number")
      (is (= 5 prefix-arg) "prefix-arg should be 5"))))

(deftest test-cu-multi-digit
  (testing "C-u 5 2 sets prefix-arg to 52"
    (h/setup-test*)

    ;; Press C-u 5 2
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)

    ;; Check prefix-arg is 52
    (let [prefix-arg (get-prefix-arg)]
      (is (= 52 prefix-arg) "prefix-arg should be 52"))))

(deftest test-cu-minus
  (testing "C-u - sets prefix-arg to '- (symbol)"
    (h/setup-test*)

    ;; Press C-u -
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (press-minus)

    ;; Check prefix-arg is '- (symbol, represented as string in ClojureScript)
    (let [prefix-arg (get-prefix-arg)]
      (is (or (= prefix-arg "-") (= prefix-arg '-))
          "prefix-arg should be '- symbol"))))

(deftest test-cu-minus-digit
  (testing "C-u - 5 sets prefix-arg to -5"
    (h/setup-test*)

    ;; Press C-u - 5
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (press-minus)
    (h/press-key "5")
    (Thread/sleep 50)

    ;; Check prefix-arg is -5
    (let [prefix-arg (get-prefix-arg)]
      (is (= -5 prefix-arg) "prefix-arg should be -5"))))

;; =============================================================================
;; Test Suite: Prefix Argument with Commands
;; =============================================================================

(deftest test-cu-forward-char
  (testing "C-u 4 C-f moves forward 4 characters"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 4 C-f should move forward 4 chars
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "4")
    (Thread/sleep 50)
    (h/press-ctrl "f")
    (Thread/sleep 100)

    ;; Cursor should be after "Hell" (position 4)
    ;; Insert a marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "HellXo World")
          "Cursor should be at position 4 after C-u 4 C-f"))))

(deftest test-cu-backward-char
  (testing "C-u 5 C-b moves backward 5 characters"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Cursor is at end (position 11)
    ;; C-u 5 C-b should move backward 5 chars to position 6
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)
    (h/press-ctrl "b")
    (Thread/sleep 100)

    ;; Cursor should be at position 6 (before "World")
    ;; Insert a marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "Hello XWorld")
          "Cursor should be at position 6 after C-u 5 C-b"))))

(deftest test-cu-next-line
  (testing "C-u 3 Down moves down 3 lines"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple lines - cursor ends at end of Line 5
    (h/type-text "Line 1")
    (h/press-key "Enter")
    (h/type-text "Line 2")
    (h/press-key "Enter")
    (h/type-text "Line 3")
    (h/press-key "Enter")
    (h/type-text "Line 4")
    (h/press-key "Enter")
    (h/type-text "Line 5")
    (Thread/sleep 100)

    ;; Move to beginning of Line 1 using Up arrow repeatedly
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 3 Down should move down 3 lines (to Line 4)
    ;; Note: Using Down arrow instead of C-n because browsers intercept C-n
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "3")
    (Thread/sleep 50)
    (h/press-key "ArrowDown")
    (Thread/sleep 100)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "XLine 4")
          "Cursor should be at beginning of Line 4 after C-u 3 C-n"))))

(deftest test-cu-previous-line
  (testing "C-u 2 Up moves up 2 lines"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple lines
    (h/type-text "Line 1")
    (h/press-key "Enter")
    (h/type-text "Line 2")
    (h/press-key "Enter")
    (h/type-text "Line 3")
    (h/press-key "Enter")
    (h/type-text "Line 4")
    (Thread/sleep 100)

    ;; Cursor is at end of Line 4
    ;; Move to beginning of line
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 2 Up should move up 2 lines (to Line 2)
    ;; Note: Using Up arrow instead of C-p for consistency with next-line test
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 100)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "XLine 2")
          "Cursor should be at beginning of Line 2 after C-u 2 C-p"))))

(deftest test-cu-forward-word
  (testing "C-u 2 M-f moves forward 2 words"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple words
    (h/type-text "one two three four")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 2 M-f should move forward 2 words (to end of "two")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)
    (h/press-meta "f")
    (Thread/sleep 100)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "one twoX three four")
          "Cursor should be after 'two' with C-u 2 M-f"))))

(deftest test-cu-backward-word
  (testing "C-u 2 M-b moves backward 2 words"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple words
    (h/type-text "one two three four")
    (Thread/sleep 100)

    ;; Cursor is at end
    ;; C-u 2 M-b should move backward 2 words (to beginning of "three")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)
    (h/press-meta "b")
    (Thread/sleep 100)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "one two Xthree four")
          "Cursor should be before 'three' with C-u 2 M-b"))))

(deftest test-cu-newline
  (testing "C-u 3 RET inserts 3 newlines"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello")
    (Thread/sleep 100)

    ;; C-u 3 Enter should insert 3 newlines
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "3")
    (Thread/sleep 50)
    (h/press-key "Enter")
    (Thread/sleep 100)

    (h/type-text "World")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (= text "Hello\n\n\nWorld")
          "C-u 3 RET should insert 3 newlines"))))

(deftest test-cu-kill-word
  (testing "C-u 2 M-d kills 2 words forward"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple words
    (h/type-text "one two three four")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 2 M-d should kill 2 words forward ("one two")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)
    (h/press-meta "d")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (= text " three four")
          "C-u 2 M-d should kill 2 words, leaving ' three four'"))))

(deftest test-cu-backward-kill-word
  (testing "C-u 2 M-DEL kills 2 words backward"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple words
    (h/type-text "one two three four")
    (Thread/sleep 100)

    ;; Cursor is at end
    ;; C-u 2 M-DEL should kill 2 words backward ("three four")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)
    (h/press-meta "Backspace")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (= text "one two ")
          "C-u 2 M-DEL should kill 2 words, leaving 'one two '"))))

(deftest test-cu-delete-backward-char
  (testing "C-u 3 Backspace deletes 3 characters backward"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; C-u 3 Backspace should delete 3 chars backward ("rld")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "3")
    (Thread/sleep 50)
    (h/press-key "Backspace")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (= text "Hello Wo")
          "C-u 3 Backspace should delete 3 chars, leaving 'Hello Wo'"))))

(deftest test-cu-delete-forward-char
  (testing "C-u 3 Delete deletes 3 characters forward"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 3 Delete should delete 3 chars forward ("Hel")
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "3")
    (Thread/sleep 50)
    (h/press-key "Delete")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (= text "lo World")
          "C-u 3 Delete should delete 3 chars, leaving 'lo World'"))))

(deftest test-prefix-arg-cleared-after-command
  (testing "Prefix arg is cleared after command execution"
    (h/setup-test*)

    ;; Set prefix arg
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (let [prefix-arg-before (get-prefix-arg)]
      (is prefix-arg-before "prefix-arg should be set"))

    ;; Execute command
    (h/press-ctrl "f")
    (Thread/sleep 100)

    ;; Check prefix-arg is cleared
    (let [prefix-arg-after (get-prefix-arg)]
      (is (nil? prefix-arg-after) "prefix-arg should be cleared after command"))))

;; =============================================================================
;; Test Suite: Mode Line Display (Future - requires mode-line implementation)
;; =============================================================================

(comment
  (deftest test-mode-line-display
    (testing "C-u shows 'C-u' in mode line"
      ;; Future test - requires mode-line display of prefix-arg-description
      ))

  (deftest test-mode-line-cu-cu
    (testing "C-u C-u shows 'C-u C-u' in mode line"
      ;; Future test
      ))

  (deftest test-mode-line-cu-5
    (testing "C-u 5 shows 'C-u 5' in mode line"
      ;; Future test
      )))
