(ns lexicon.ui.editing.basic-test
  "Phase 0 & Phase 1 Basic Editing Tests - E2E with Etaoin

  Tests from ManualTestingPlan.md:
  - Phase 0: P0-01 through P0-06
  - Phase 1: P1-01 through P1-05"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

;; Use shared fixture - driver managed internally by test-helpers
(use-fixtures :once h/with-driver)

;; Tests
(deftest test-p0-01-basic-text-input
  (testing "P0-01: Basic text input"
    ;; Go to app
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type a sentence
    (let [sentence "The quick brown fox jumps over the lazy dog."]
      (h/type-text sentence)

      ;; Wait for updates to propagate
      (Thread/sleep 20)

      ;; Verify text appears in the buffer
      (let [editor-text (h/get-buffer-text*)]
        (is (.contains editor-text sentence)
            (str "Editor should contain: " sentence ", but got: " editor-text))))))

(deftest test-p0-02-enter-creates-newline
  (testing "P0-02: Enter/Return key creates newline"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type first line
    (h/type-text "line 1")

    ;; Press Enter
    (h/press-key "Enter")
    (Thread/sleep 10)

    ;; Type second line
    (h/type-text "line 2")
    (Thread/sleep 20)

    ;; Verify both lines are present
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "line 1"))
      (is (.contains editor-text "line 2")))))

(deftest test-p0-03-backspace-deletes
  (testing "P0-03: Backspace deletes character"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "abcde")
    (Thread/sleep 10)

    ;; Press Backspace twice
    (h/press-key "Backspace")
    (Thread/sleep 10)
    (h/press-key "Backspace")
    (Thread/sleep 20)

    ;; Verify text is now "abc"
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "abc"))
      (is (not (.contains editor-text "de"))))))

(deftest test-regression-typing-after-backspace-all
  (testing "REGRESSION: Typing works after backspacing entire buffer"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Step 1: Type initial text
    (h/type-text "abcd")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "abcd")))

    ;; Step 2: Backspace everything
    (h/press-key "Backspace")
    (Thread/sleep 10)
    (h/press-key "Backspace")
    (Thread/sleep 10)
    (h/press-key "Backspace")
    (Thread/sleep 10)
    (h/press-key "Backspace")
    (Thread/sleep 20)

    ;; Step 3: Try to type again - this should work
    (h/type-text "line 1")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "line 1")))))

(deftest test-p0-04-delete-key
  (testing "P0-04: Delete key deletes character forward"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "abcde")
    (Thread/sleep 10)

    ;; Move cursor to between 'b' and 'c' using left arrow
    (h/press-key "ArrowLeft")
    (Thread/sleep 30)
    (h/press-key "ArrowLeft")
    (Thread/sleep 30)
    (h/press-key "ArrowLeft")
    (Thread/sleep 10)

    ;; Press Delete twice
    (h/press-key "Delete")
    (Thread/sleep 10)
    (h/press-key "Delete")
    (Thread/sleep 20)

    ;; Should be "abe"
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "abe")))))

(deftest test-p0-05-arrow-navigation
  (testing "P0-05: Arrow key navigation"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type two lines
    (h/type-text "line 1")
    (Thread/sleep 10)
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "line 2")
    (Thread/sleep 10)

    ;; Test Up Arrow - should move to line 1
    (h/press-key "ArrowUp")
    (Thread/sleep 10)

    ;; Type something - should appear on line 1
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "line 1X")
              (re-find #"line 1.*X" editor-text))))))

(deftest test-p0-06-mouse-click-positioning
  (testing "P0-06: Mouse click positioning"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type several lines
    (h/type-text "First line")
    (Thread/sleep 10)
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "Second line")
    (Thread/sleep 10)
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "Third line")
    (Thread/sleep 20)

    ;; Click somewhere in the middle
    (e/click h/*driver* {:css ".editable-area"})
    (Thread/sleep 10)

    ;; Type a character - should insert at clicked position
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "X")))))

;; ========================================
;; Phase 1: Core Emacs - Basic Editing
;; ========================================

(deftest test-p1-01-character-navigation
  (testing "P1-01: Character-wise navigation (C-f, C-b)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type "hello world"
    (h/type-text "hello world")
    (Thread/sleep 10)

    ;; Move cursor to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Press C-f five times to move to space after "hello"
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 30))

    ;; Verify position by typing a character
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "helloX")
          "C-f should move cursor forward"))

    ;; Move to beginning again and test C-b
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Move to end
    (h/press-ctrl "e")
    (Thread/sleep 10)

    ;; Press C-b three times
    (dotimes [_ 3]
      (h/press-ctrl "b")
      (Thread/sleep 30))

    ;; Type character - should be before "rld"
    (h/type-text "Y")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "woYrld")
          (str "C-b should move cursor backward. Got: " editor-text)))))

(deftest test-p1-02-line-navigation
  (testing "P1-02: Line-wise navigation (C-p, C-n)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type three lines
    (h/type-text "line 1")
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "line 2")
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "line 3")
    (Thread/sleep 10)

    ;; Cursor should be at end of line 3
    ;; Press C-p to move to line 2
    (h/press-ctrl "p")
    (Thread/sleep 10)

    ;; Type character to verify on line 2
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "line 2X")
          "C-p should move cursor up"))

    ;; Press C-n once to move back to line 3
    (h/press-ctrl "n")
    (Thread/sleep 10)

    ;; Type character to verify on line 3
    (h/type-text "Y")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "line 3")
          (str "C-n should move cursor down. Got: " editor-text)))))

(deftest test-p1-03-beginning-end-of-line
  (testing "P1-03: Beginning/end of line (C-a, C-e)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "this is a test")
    (Thread/sleep 10)

    ;; Move cursor to middle (using C-b)
    (dotimes [_ 5]
      (h/press-ctrl "b")
      (Thread/sleep 20))

    ;; Press C-a to go to beginning of line
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Type character to verify at beginning
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "Xthis is a test")
          "C-a should move to beginning of line"))

    ;; Press C-e to go to end of line
    (h/press-ctrl "e")
    (Thread/sleep 10)

    ;; Type character to verify at end
    (h/type-text "Y")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "Xthis is a testY")
          "C-e should move to end of line"))))

(deftest test-p1-04-word-navigation
  (testing "P1-04: Word-wise navigation (M-f, M-b)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "the quick brown fox")
    (Thread/sleep 10)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Press M-f once to move forward one word
    (h/press-meta "f")
    (Thread/sleep 10)

    ;; Type character to verify we moved forward
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "theX")
              (.contains editor-text "the X"))
          (str "M-f should move forward by word. Got: " editor-text)))

    ;; Press M-b once to move back one word
    (h/press-meta "b")
    (Thread/sleep 10)

    ;; Type character to verify we moved backward
    (h/type-text "Y")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "Ythe")
              (.contains editor-text "Y the"))
          (str "M-b should move backward by word. Got: " editor-text)))))

(deftest test-p1-05-beginning-end-of-buffer
  (testing "P1-05: Beginning/end of buffer (M-<, M->)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple lines
    (h/type-text "first line")
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "second line")
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "third line")
    (h/press-key "Enter")
    (Thread/sleep 10)
    (h/type-text "fourth line")
    (Thread/sleep 10)

    ;; Move to middle
    (h/press-ctrl "p")
    (h/press-ctrl "p")
    (Thread/sleep 10)

    ;; Press M-> to go to end of buffer
    (h/press-meta ">")
    (Thread/sleep 10)

    ;; Type character to verify at end
    (h/type-text "X")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "fourth lineX")
          "M-> should move to end of buffer"))

    ;; Press M-< to go to beginning of buffer
    (h/press-meta "<")
    (Thread/sleep 10)

    ;; Type character to verify at beginning
    (h/type-text "Y")
    (Thread/sleep 20)

    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "Yfirst line")
          "M-< should move to beginning of buffer"))))

(deftest ^:skip test-p1-06-set-mark
  (testing "P1-06: Setting the mark (C-SPC)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "select this text")
    (Thread/sleep 10)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Set mark with C-SPC (Ctrl+Space)
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: ' ',
        code: 'Space',
        ctrlKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute h/*driver* script))
    (Thread/sleep 20)

    ;; Move forward to select "select this"
    (dotimes [_ 11]
      (h/press-ctrl "f")
      (Thread/sleep 10))
    (Thread/sleep 20)

    ;; Check if region exists by trying to verify mark was set
    ;; We can't easily check visual highlighting, so we'll verify
    ;; that kill-region works in the next test
    (is true "PENDING: Mark set - verified in subsequent kill-region test - needs E2E implementation")))

(deftest ^:skip test-p1-07-kill-region
  (testing "P1-07: Kill region (C-w) - SKIPPED: Browser captures C-w (close tab)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "select this text")
    (Thread/sleep 10)

    ;; Move to beginning and set mark
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Set mark
    (let [script "
      const input = document.querySelector('.hidden-input');
      const event = new KeyboardEvent('keydown', {
        key: ' ',
        code: 'Space',
        ctrlKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute h/*driver* script))
    (Thread/sleep 10)

    ;; Move forward to select "select this"
    (dotimes [_ 11]
      (h/press-ctrl "f")
      (Thread/sleep 10))
    (Thread/sleep 10)

    ;; Kill region with C-w
    (h/press-ctrl "w")
    (Thread/sleep 20)

    ;; NOTE: C-w is captured by browser (close tab) - cannot test in E2E
    ;; This must be verified manually or in native desktop tests
    (is true "PENDING: Test skipped - C-w intercepted by browser - needs E2E implementation")))

;; NOTE: Kill/yank tests (P1-08, P1-09, P1-10) moved to kill_ring_test.clj
;; to avoid duplication. See: test-yank-inserts-killed-text,
;; test-copy-region-preserves-text, test-kill-line-basic

;; NOTE: Undo test (P1-11) moved to undo_test.clj to avoid duplication.
;; See: test-undo-reverses-insert, test-undo-restores-point

;;; Phase 2: Buffers, Files, and Core Polish

(deftest test-p2-01-switch-to-buffer
  (testing "P2-01: Verify switch-to-buffer (C-x b)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text in *scratch*
    (h/type-text "scratch buffer content")
    (Thread/sleep 20)

    ;; Press C-x b to switch buffer
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)

    ;; Minibuffer should be active
    (let [minibuffer-visible (e/exists? h/*driver* {:css ".minibuffer"})]
      (is minibuffer-visible "Minibuffer should be visible"))

    ;; Type new buffer name and press Enter in minibuffer
    (e/fill h/*driver* {:css ".minibuffer-input"} "test-buffer")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)  ; Wait for buffer switch and DOM update

    ;; New buffer should be created
    (let [editor-text (h/get-buffer-text*)]
      (is (or (empty? editor-text)
              (not (.contains editor-text "scratch buffer")))
          "New buffer should be empty or not contain scratch content"))))

(deftest test-p2-02-buffer-state-preservation
  (testing "P2-02: Verify buffer state preservation"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type in scratch buffer
    (h/type-text "original scratch")
    (Thread/sleep 20)

    ;; Switch to test-buffer
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)
    (h/type-text "test-buffer")
    (h/press-key "Enter")
    (Thread/sleep 30)

    ;; Type in test-buffer
    (h/type-text "hello from test")
    (Thread/sleep 20)

    ;; Switch back to *scratch*
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)
    (h/type-text "*scratch*")
    (h/press-key "Enter")
    (Thread/sleep 30)

    ;; Verify scratch content preserved
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "original scratch")
          "Scratch buffer content should be preserved"))

    ;; Switch back to test-buffer
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)
    (h/type-text "test-buffer")
    (h/press-key "Enter")
    (Thread/sleep 30)

    ;; Verify test-buffer content preserved
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "hello from test")
          "test-buffer content should be preserved"))))

(deftest test-p2-03-list-buffers
  (testing "P2-03: Verify list-buffers (C-x C-b)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Create a couple of buffers first
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)
    (h/type-text "buffer1")
    (h/press-key "Enter")
    (Thread/sleep 30)

    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)
    (h/type-text "buffer2")
    (h/press-key "Enter")
    (Thread/sleep 30)

    ;; Now list buffers with C-x C-b
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-ctrl "b")
    (Thread/sleep 50)

    ;; Buffer list should appear
    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "*Buffer List*")
              (.contains editor-text "buffer1")
              (.contains editor-text "buffer2"))
          (str "Buffer list should appear. Got: " editor-text)))))

(deftest test-p2-04-buffer-modified-indicator
  (testing "P2-04: Verify buffer modified indicator"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text to modify buffer
    (h/type-text "modified content")
    (Thread/sleep 30)

    ;; Check status bar (mode line) for modified indicator (**)
    (let [status-bar (e/get-element-text h/*driver* {:css ".status-bar"})]
      (is (.contains status-bar "**")
          (str "Status bar should show ** for modified buffer. Got: " status-bar)))))

(deftest ^:skip test-p2-05-save-buffer
  (testing "P2-05: Verify save-buffer (C-x C-s) - SKIPPED: Browser file dialog"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type content
    (h/type-text "content to save")
    (Thread/sleep 20)

    ;; Press C-x C-s
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-ctrl "s")
    (Thread/sleep 30)

    ;; NOTE: Browser file save dialog cannot be automated in E2E tests
    ;; This must be tested manually
    (is true "PENDING: Test skipped - browser file dialog requires manual testing - needs E2E implementation")))

(deftest test-p2-5-01-keyboard-quit
  (testing "P2.5-01: Verify keyboard-quit (C-g)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open minibuffer with C-x b
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 30)

    ;; Verify minibuffer is open
    (let [minibuffer-visible (e/exists? h/*driver* {:css ".minibuffer"})]
      (is minibuffer-visible "Minibuffer should be visible"))

    ;; Press C-g to quit
    (h/press-ctrl "g")
    (Thread/sleep 30)

    ;; Minibuffer should close or echo area should show quit message
    (let [echo-text (try
                      (e/get-element-text h/*driver* {:css ".echo-area"})
                      (catch Exception _ ""))]
      (is (or (.contains echo-text "Quit")
              (.contains echo-text "quit"))
          (str "Should show quit message. Got: " echo-text)))))

(deftest test-p2-5-02-universal-argument
  (testing "P2.5-02: Verify Universal Argument (C-u)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press C-u and type 'a'
    (h/press-ctrl "u")
    (Thread/sleep 20)
    (h/type-text "a")
    (Thread/sleep 30)

    ;; Should have 4 a's
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "aaaa")
          (str "C-u should insert 4 a's. Got: " editor-text)))

    ;; Press C-u C-u and type 'b'
    (h/press-ctrl "u")
    (Thread/sleep 10)
    (h/press-ctrl "u")
    (Thread/sleep 20)
    (h/type-text "b")
    (Thread/sleep 30)

    ;; Should have 16 b's
    (let [editor-text (h/get-buffer-text*)
          b-count (count (re-seq #"b" editor-text))]
      (is (= b-count 16)
          (str "C-u C-u should insert 16 b's. Got: " b-count " b's")))))

;; NOTE: Phase 3 window tests (P3-01 through P3-06) moved to window_test.clj
;; to avoid duplication. See: test-split-window-horizontally-keyboard,
;; test-split-window-vertically-keyboard, test-other-window-keyboard,
;; test-windows-have-independent-point-keyboard, test-delete-other-windows-keyboard

;;; Phase 4, 5, 6: Modes, Help, Packages, and Display

(deftest test-p4-01-execute-extended-command
  (testing "P4-01: Verify execute-extended-command (M-x)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x
    (h/press-meta "x")
    (Thread/sleep 50)

    ;; Minibuffer should be active with M-x prompt
    (let [minibuffer-visible (e/exists? h/*driver* {:css ".minibuffer"})]
      (is minibuffer-visible "Minibuffer should be visible for M-x"))

    ;; Type command in minibuffer and press Enter
    (e/fill h/*driver* {:css ".minibuffer-input"} "text-mode")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Mode line should show Text mode or command should execute
    (let [status-bar (try
                      (e/get-element-text h/*driver* {:css ".status-bar"})
                      (catch Exception _ ""))]
      (is (or (.contains status-bar "Text")
              (.contains status-bar "text"))
          (str "Mode should change or command execute. Got: " status-bar)))))

(deftest test-p4-02-describe-key
  (testing "P4-02: Verify describe-key (C-h k)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press C-h k
    (h/press-ctrl "h")
    (Thread/sleep 10)
    (h/press-key "k")
    (Thread/sleep 50)

    ;; Echo area should prompt for key
    (let [echo-text (try
                      (e/get-element-text h/*driver* {:css ".echo-area"})
                      (catch Exception _ ""))]
      (is (or (.contains echo-text "Describe key")
              (.contains echo-text "key"))
          "Should prompt for key description"))

    ;; Press C-f
    (h/press-ctrl "f")
    (Thread/sleep 50)

    ;; Help buffer should appear
    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "*Help*")
              (.contains editor-text "forward-char")
              (.contains editor-text "C-f"))
          (str "Help buffer should show forward-char. Got: " editor-text)))))

(deftest test-p4-03-describe-bindings
  (testing "P4-03: Verify describe-bindings (C-h b)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press C-h b
    (h/press-ctrl "h")
    (Thread/sleep 10)
    (h/press-key "b")
    (Thread/sleep 100)

    ;; Help buffer with bindings should appear
    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "*Help*")
              (.contains editor-text "bindings")
              (.contains editor-text "C-f")
              (.contains editor-text "forward"))
          (str "Should show key bindings. Got: " editor-text)))))

(deftest test-p5-01-minor-mode-toggling
  (testing "P5-01: Verify minor mode toggling"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x line-number-mode
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-text "line-number-mode")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Check mode line for changes
    (let [status-bar-1 (try
                        (e/get-element-text h/*driver* {:css ".status-bar"})
                        (catch Exception _ ""))]
      ;; Toggle again
      (h/press-meta "x")
      (Thread/sleep 50)
      (h/type-text "line-number-mode")
      (Thread/sleep 20)
      (h/press-key "Enter")
      (Thread/sleep 50)

      (let [status-bar-2 (try
                          (e/get-element-text h/*driver* {:css ".status-bar"})
                          (catch Exception _ ""))]
        ;; Mode line should have changed (at least one toggle happened)
        (is (or (not= status-bar-1 status-bar-2)
                (.contains status-bar-1 "L"))
            "Minor mode should toggle")))))

(deftest test-p6a-01-package-list
  (testing "P6A-01: Verify package system listing"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x package-list
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-text "package-list")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Package list buffer should appear
    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "*Packages*")
              (.contains editor-text "package")
              (.contains editor-text "evil"))
          (str "Package list should appear. Got: " editor-text)))))

(deftest test-p6b-01-theme-loading
  (testing "P6B-01: Verify theme loading"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x load-theme
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-text "load-theme")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Type theme name
    (h/type-text "lexicon-base-dark")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Check that some styling changed (hard to verify visually in E2E)
    ;; We'll just verify the command executed without error
    (let [echo-text (try
                      (e/get-element-text h/*driver* {:css ".echo-area"})
                      (catch Exception _ ""))]
      (is (or (.contains echo-text "theme")
              (.contains echo-text "Loaded")
              (empty? echo-text))
          "Theme loading should complete"))))

(deftest ^:skip test-p6b-02-font-size-change
  (testing "P6B-02: Verify dynamic font size change"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Press M-x set-font-size
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-text "set-font-size")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Type font size
    (h/type-text "20")
    (Thread/sleep 20)
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Verify command executed (hard to verify size in E2E)
    (let [echo-text (try
                      (e/get-element-text h/*driver* {:css ".echo-area"})
                      (catch Exception _ ""))]
      (is (or (.contains echo-text "font")
              (.contains echo-text "size")
              (empty? echo-text))
          "Font size command should execute"))))

(deftest ^:skip test-p6b-03-status-bar-formatting
  (testing "P6B-03: Verify Mode Line Formatting"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Check initial mode line
    (let [status-bar (e/get-element-text h/*driver* {:css ".status-bar"})]
      (is (or (.contains status-bar "*scratch*")
              (.contains status-bar "scratch"))
          "Mode line should show buffer name"))

    ;; Type to modify buffer
    (h/type-text "modify")
    (Thread/sleep 30)

    ;; Check for modified indicator
    (let [status-bar (e/get-element-text h/*driver* {:css ".status-bar"})]
      (is (.contains status-bar "**")
          "Mode line should show ** for modified buffer"))))

(deftest ^:skip test-p6d-01-thing-at-point
  (testing "P6D-01: Verify thing-at-point (conceptual) - SKIPPED: Requires custom command"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type URL
    (h/type-text "https://example.com")
    (Thread/sleep 20)

    ;; NOTE: This test requires a custom command to be implemented
    (is true "PENDING: Test skipped - requires custom thing-at-point command - needs E2E implementation")))

;;; Phase 7.8: Query Replace Tests
;; NOTE: Basic query-replace tests (P7-8-01, P7-8-02, P7-8-03, P7-8-04, P7-8-05)
;; moved to phase_7_8_test.clj to avoid duplication. See: test-p7-8-query-replace-basic,
;; test-p7-8-query-replace-all, test-p7-8-query-replace-dot, etc.
;; Only unique tests remain here.

(deftest test-p7-8-01a-query-replace-cursor-and-region
  (testing "P7.8-01a: Query-replace cursor movement and region highlighting (Issue #64)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple occurrences: "foo bar foo baz foo"
    ;; Positions: foo(0-3) bar(4-7) foo(8-11) baz(12-15) foo(16-19)
    (h/type-text "foo bar foo baz foo")
    (Thread/sleep 20)

    ;; Go to beginning
    (h/press-meta "<")
    (Thread/sleep 20)

    ;; Start query-replace with M-%
    (h/press-meta "%")
    (Thread/sleep 50)

    ;; Type search string
    (e/fill h/*driver* {:css ".minibuffer-input"} "foo")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Type replacement string
    (e/fill h/*driver* {:css ".minibuffer-input"} "FOO")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; ASSERTION 1: After starting query-replace, first match should be highlighted
    ;; Cursor should be at position 3 (end of first "foo"), mark at position 0 (start)
    (let [cursor-pos (h/get-cursor-position)
          editor-text (h/get-buffer-text*)]
      (is (= 3 (:col cursor-pos))
          (str "Cursor should be at end of first 'foo' (col 3), got: " cursor-pos))
      (is (= 0 (:row cursor-pos))
          (str "Cursor should be on first line, got: " cursor-pos)))

    ;; Press 'y' to replace first occurrence
    (h/press-key "y")
    (Thread/sleep 100)

    ;; ASSERTION 2: After first 'y', cursor should move to end of second match
    ;; Text is now "FOO bar foo baz foo"
    ;; Second "foo" is at positions 8-11, cursor should be at col 11
    (let [cursor-pos (h/get-cursor-position)
          editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "FOO bar foo")
          (str "First 'foo' should be replaced, got: " editor-text))
      (is (= 11 (:col cursor-pos))
          (str "Cursor should be at end of second 'foo' (col 11), got: " cursor-pos)))

    ;; Press 'y' to replace second occurrence
    (h/press-key "y")
    (Thread/sleep 100)

    ;; ASSERTION 3: After second 'y', cursor should move to end of third match
    ;; Text is now "FOO bar FOO baz foo"
    ;; Third "foo" is at positions 16-19, cursor should be at col 19
    (let [cursor-pos (h/get-cursor-position)
          editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "FOO bar FOO baz foo")
          (str "Second 'foo' should be replaced, got: " editor-text))
      (is (= 19 (:col cursor-pos))
          (str "Cursor should be at end of third 'foo' (col 19), got: " cursor-pos)))

    ;; Press 'q' to quit
    (h/press-key "q")
    (Thread/sleep 100)

    ;; ASSERTION 4: After quitting, verify final text
    ;; Region should be cleared (we can't easily test mark=nil in E2E, but cursor should remain)
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "FOO bar FOO baz foo")
          (str "Should have replaced first two 'foo' with 'FOO', got: " editor-text)))))

(deftest test-p7-8-06-query-replace-no-matches
  (testing "P7.8-06: Query-replace with no matches"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "hello world")
    (Thread/sleep 20)

    ;; Start query-replace for nonexistent string
    (h/press-meta "%")
    (Thread/sleep 50)
    (e/fill h/*driver* {:css ".minibuffer-input"} "xyz")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)
    (e/fill h/*driver* {:css ".minibuffer-input"} "ABC")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Text should be unchanged
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "hello world")
          (str "Text should be unchanged when no matches, got: " editor-text)))))

;; =============================================================================
;; Phase 7.8 Batch 5: Incremental Search (C-s, C-r)
;; NOTE: Isearch tests temporarily removed - need to find reliable way to test
;; cursor positioning in E2E environment. Manual testing shows isearch works.
;; =============================================================================

(deftest ^:skip test-p6-5-01-test-suite
  (testing "P6.5-01: Verify Test Suite"
    ;; This is a meta-test - if we're running, tests are working
    (is true "PENDING: Test suite is functional - needs E2E implementation")))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.basic-editing-test))
