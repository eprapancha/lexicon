(ns lexicon.ui.editing.primitives-test
  "E2E tests for editing primitives - tests USER EDITING produces correct state.

  Tests that when users type/delete, the internal state is correct:
  - Point moves correctly
  - Buffer contents are correct
  - Position predicates work
  - Character access works

  Uses keyboard simulation for all editing actions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Text Insertion via User Typing
;; =============================================================================

(deftest test-typing-adds-text
  (testing "Typing adds text at point"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 100)
    (is (= "Hello" (h/get-buffer-text*)))
    (is (= 5 (h/get-point*)) "Point at end of typing"))

  (testing "Typing at middle inserts text"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "HelloWorld")
    (Thread/sleep 50)
    ;; Move to position 5
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)
    ;; Type space
    (h/type-text " ")
    (Thread/sleep 100)
    (is (= "Hello World" (h/get-buffer-text*)))))

;; =============================================================================
;; Text Deletion via User Actions
;; =============================================================================

(deftest test-backspace-deletes-backwards
  (testing "Backspace removes character before point"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    ;; Delete last 5 chars with backspace
    (dotimes [_ 5]
      (h/press-key "Backspace")
      (Thread/sleep 30))
    (Thread/sleep 100)
    (is (= "Hello " (h/get-buffer-text*)))))

(deftest test-delete-key-removes-forward
  (testing "Delete key removes character after point"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)
    ;; Delete forward
    (dotimes [_ 6]
      (h/press-key "Delete")
      (Thread/sleep 30))
    (Thread/sleep 100)
    (is (= "World" (h/get-buffer-text*)))))

(deftest test-delete-selection
  (testing "Typing or backspace with selection deletes selection"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 30)
    ;; Select "Hello " with shift+arrows
    (dotimes [_ 6]
      (h/press-shift "right")
      (Thread/sleep 20))
    (Thread/sleep 50)
    ;; Delete selection
    (h/press-key "Backspace")
    (Thread/sleep 100)
    (is (= "World" (h/get-buffer-text*)))))

;; =============================================================================
;; Point Movement via Keyboard
;; =============================================================================

(deftest test-cursor-movement-updates-point
  (testing "Ctrl+F moves point forward"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    (h/press-ctrl "a")  ; Go to beginning
    (Thread/sleep 30)
    (h/press-ctrl "f")  ; Forward one char
    (Thread/sleep 50)
    (is (= 1 (h/get-point*))))

  (testing "Ctrl+B moves point backward"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    ;; Point is at 5
    (h/press-ctrl "b")  ; Back one char
    (Thread/sleep 50)
    (is (= 4 (h/get-point*))))

  (testing "Ctrl+A moves to beginning"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (is (= 0 (h/get-point*))))

  (testing "Ctrl+E moves to end"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/press-ctrl "e")
    (Thread/sleep 50)
    (is (= 5 (h/get-point*)))))

;; =============================================================================
;; Position Predicates after User Actions
;; =============================================================================

(deftest test-bobp-after-cursor-movement
  (testing "bobp true at beginning after Ctrl+A"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (is (h/bobp*) "Should be at beginning"))

  (testing "bobp false after typing"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (is (not (h/bobp*)) "Should not be at beginning after typing")))

(deftest test-eobp-after-cursor-movement
  (testing "eobp true at end after typing"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (is (h/eobp*) "Should be at end after typing"))

  (testing "eobp false after moving back"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (h/press-ctrl "b")
    (Thread/sleep 50)
    (is (not (h/eobp*)) "Should not be at end after Ctrl+B")))

(deftest test-bolp-eolp-after-user-actions
  (testing "bolp at line beginning"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (Thread/sleep 50)
    (h/press-ctrl "a")  ; Move to beginning of line
    (Thread/sleep 50)
    (is (h/bolp*) "Should be at line beginning"))

  (testing "eolp at line end"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (Thread/sleep 50)
    ;; Go to first line end
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/press-ctrl "p")  ; Up to first line
    (Thread/sleep 30)
    (h/press-ctrl "e")  ; End of line
    (Thread/sleep 50)
    (is (h/eolp*) "Should be at line end")))

;; =============================================================================
;; Character Access after User Actions
;; =============================================================================

(deftest test-char-after-reflects-user-typing
  (testing "char-after returns typed character"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (is (= "H" (h/char-after*))))

  (testing "char-after at end returns nil"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (is (nil? (h/char-after*)))))

(deftest test-char-before-reflects-user-typing
  (testing "char-before returns previous character"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (is (= "o" (h/char-before*))))

  (testing "char-before at beginning returns nil"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (is (nil? (h/char-before*)))))

;; =============================================================================
;; Line Positions after User Actions
;; =============================================================================

(deftest test-line-positions-after-typing-multiline
  (testing "line-beginning-position works after typing"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (h/press-key "Enter")
    (h/type-text "Foo")
    (Thread/sleep 50)
    ;; Move to middle of "World"
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/press-ctrl "p")  ; Up one line
    (Thread/sleep 30)
    (h/press-ctrl "f")
    (h/press-ctrl "f")
    (Thread/sleep 50)
    (is (= 6 (h/line-beginning-position*))))

  (testing "line-end-position works after typing"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (h/press-key "Enter")
    (h/type-text "Foo")
    (Thread/sleep 50)
    ;; Move to middle of "World"
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (h/press-ctrl "p")  ; Up one line
    (Thread/sleep 30)
    (h/press-ctrl "f")
    (h/press-ctrl "f")
    (Thread/sleep 50)
    (is (= 11 (h/line-end-position*)))))

;; =============================================================================
;; Region after Selection
;; =============================================================================

(deftest test-region-after-user-selection
  (testing "region-beginning/end reflect user selection"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 50)
    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 30)
    ;; Select "Hello" with shift+arrows
    (dotimes [_ 5]
      (h/press-shift "right")
      (Thread/sleep 20))
    (Thread/sleep 100)
    (is (= 0 (h/region-beginning*)))
    (is (= 5 (h/region-end*)))))

;; =============================================================================
;; Edge Cases
;; =============================================================================

(deftest test-empty-buffer-predicates
  (testing "bobp and eobp both true in empty buffer"
    (h/setup-test*)
    (h/clear-buffer)
    (is (h/bobp*) "Should be at beginning")
    (is (h/eobp*) "Should be at end")))

(deftest test-typing-newlines
  (testing "Enter key inserts newline"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello")
    (h/press-key "Enter")
    (h/type-text "World")
    (Thread/sleep 100)
    (is (= "Hello\nWorld" (h/get-buffer-text*)))))

(deftest test-buffer-substring-after-typing
  (testing "buffer-substring extracts typed text"
    (h/setup-test*)
    (h/clear-buffer)
    (h/type-text "Hello World")
    (Thread/sleep 100)
    (is (= "World" (h/buffer-substring* 6 11)))))

;; =============================================================================
;; Delete Selection Mode
;; =============================================================================

(deftest test-delete-selection-mode
  (testing "delete-selection-mode replaces selection when typing"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable delete-selection-mode
    (h/execute-command "delete-selection-mode")
    (Thread/sleep 100)

    ;; Type initial text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 30)

    ;; Select "Hello" with shift+arrows (5 chars)
    (dotimes [_ 5]
      (h/press-shift "right")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Type "X" - should replace the selection
    (h/type-text "X")
    (Thread/sleep 100)

    ;; Should now have "X World" (not "XHello World")
    (is (= "X World" (h/get-buffer-text*))
        "delete-selection-mode should replace selection with typed text")))

(deftest test-delete-selection-mode-with-mark
  (testing "delete-selection-mode replaces C-SPC selection when typing"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable delete-selection-mode
    (h/execute-command "delete-selection-mode")
    (Thread/sleep 100)

    ;; Type initial text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 30)

    ;; Set mark with C-SPC
    (h/press-ctrl "SPC")
    (Thread/sleep 50)

    ;; Move forward 5 chars to select "Hello"
    (dotimes [_ 5]
      (h/press-ctrl "f")
      (Thread/sleep 20))
    (Thread/sleep 50)

    ;; Type "X" - should replace the selection
    (h/type-text "X")
    (Thread/sleep 100)

    ;; Should now have "X World" (not "XHello World")
    (is (= "X World" (h/get-buffer-text*))
        "delete-selection-mode should replace C-SPC selection with typed text")))

;; =============================================================================
;; Rectangle Operations
;; =============================================================================

(deftest test-rectangle-kill-yank
  (testing "C-x r k kills rectangle and C-x r y yanks it"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multi-line text
    (h/type-text "abcde")
    (h/press-key "Enter")
    (h/type-text "fghij")
    (h/press-key "Enter")
    (h/type-text "klmno")
    (Thread/sleep 100)

    ;; Verify initial text
    (is (= "abcde\nfghij\nklmno" (h/get-buffer-text*))
        "Initial text should be correct")

    ;; Go to absolute beginning of buffer
    (h/press-meta "<")
    (Thread/sleep 50)

    ;; Move forward 1 char (to column 1, after 'a')
    (h/press-ctrl "f")
    (Thread/sleep 30)

    ;; Set mark with C-Space
    (h/press-ctrl " ")
    (Thread/sleep 50)

    ;; Move down to line 1, then forward to column 3
    ;; For rectangle: columns 1-3 on lines 0-1
    ;; Use execute-command for next-line since C-n may have focus issues
    (h/execute-command "next-line")
    (Thread/sleep 100)
    (h/press-ctrl "f")
    (Thread/sleep 30)
    (h/press-ctrl "f")
    (Thread/sleep 50)

    ;; Kill the rectangle with C-x r k
    (h/press-ctrl "x")
    (Thread/sleep 100)
    (h/press-key "r")
    (Thread/sleep 100)
    (h/press-key "k")
    (Thread/sleep 200)

    ;; After killing rectangle columns 1-3 on lines 0-1:
    ;; Line 0: "a" + "de" = "ade"
    ;; Line 1: "f" + "ij" = "fij"
    ;; Line 2: unchanged = "klmno"
    (let [text (h/get-buffer-text*)]
      (is (= "ade\nfij\nklmno" text)
          "After kill-rectangle, columns should be removed"))

    ;; Move to end of buffer
    (h/press-meta ">")
    (Thread/sleep 50)

    ;; Yank the rectangle with C-x r y
    (h/press-ctrl "x")
    (Thread/sleep 100)
    (h/press-key "r")
    (Thread/sleep 100)
    (h/press-key "y")
    (Thread/sleep 200)

    ;; After yanking rectangle at end:
    ;; The killed rectangle was ["bc" "gh"] (2 lines)
    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "bc") "Yanked rectangle should contain 'bc'")
      (is (str/includes? text "gh") "Yanked rectangle should contain 'gh'"))))

;; =============================================================================
;; Goal Column Tests
;; =============================================================================

(deftest test-goal-column-preserved
  (testing "Goal column is preserved when moving through short lines"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type three lines with different lengths:
    ;; Line 1: "Hello World!" (12 chars)
    ;; Line 2: "Hi" (2 chars - short line)
    ;; Line 3: "Hello World!" (12 chars)
    (h/type-text "Hello World!")
    (h/press-key "Enter")
    (h/type-text "Hi")
    (h/press-key "Enter")
    (h/type-text "Hello World!")
    (Thread/sleep 100)

    ;; Move to beginning of buffer
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 50)

    ;; Move to column 8 (after "Hello Wo")
    (h/press-ctrl "a")
    (Thread/sleep 50)
    (dotimes [_ 8]
      (h/press-ctrl "f")
      (Thread/sleep 30))

    ;; Now at column 8 on line 1
    ;; Move down to line 2 (short line "Hi") - cursor should go to end (col 2)
    (h/press-key "ArrowDown")
    (Thread/sleep 50)

    ;; Move down to line 3 - cursor should return to column 8
    (h/press-key "ArrowDown")
    (Thread/sleep 50)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      ;; If goal column works, X should be inserted at column 8: "Hello WoXrld!"
      (is (str/includes? text "Hello WoXrld!")
          "Goal column should be preserved - cursor should return to column 8"))))

;; =============================================================================
;; Word Movement Tests
;; =============================================================================

(deftest test-forward-word-punctuation
  (testing "M-f stops at punctuation, not after it"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with punctuation
    (h/type-text "hello, world")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; M-f should stop after "hello" (before comma)
    (h/press-meta "f")
    (Thread/sleep 100)

    ;; Insert marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      ;; X should be after "hello", before comma: "helloX, world"
      (is (str/includes? text "helloX, world")
          "forward-word should stop at word boundary, not include punctuation"))))

;; =============================================================================
;; Selection / Region Tests
;; =============================================================================

(deftest test-shift-selection-cleared-by-keyboard-quit
  (testing "C-g clears selection - subsequent arrow movement doesn't extend it"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type multiple lines
    (h/type-text "AAAA")
    (h/press-key "Enter")
    (h/type-text "BBBB")
    (h/press-key "Enter")
    (h/type-text "CCCC")
    (Thread/sleep 100)

    ;; Move to beginning of Line 1
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-key "ArrowUp")
    (Thread/sleep 50)
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Press Shift+Down to select Line 1 (selection: "AAAA\n")
    (h/press-shift "ArrowDown")
    (Thread/sleep 100)

    ;; Press C-g to clear selection
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Now press Down arrow WITHOUT shift
    ;; If C-g worked: cursor moves to Line 3, NO selection
    ;; If C-g failed: selection extends to include Line 2 too
    (h/press-key "ArrowDown")
    (Thread/sleep 100)

    ;; Now delete with Backspace
    ;; If NO selection: deletes 1 char (last char of Line 2 since we're at start of Line 3)
    ;; If selection persists: deletes entire selection (AAAA + BBBB + part of CCCC)
    (h/press-key "Backspace")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      ;; If C-g cleared selection properly:
      ;; - We were at start of Line 3 after ArrowDown
      ;; - Backspace deleted the newline before Line 3, joining BBBB and CCCC
      ;; - Result: "AAAA\nBBBBCCCC" (Line 1 intact)
      ;;
      ;; If selection persisted:
      ;; - Backspace would delete the selection (AAAA\nBBBB\n or more)
      ;; - Line 1 (AAAA) would be gone
      (is (str/includes? text "AAAA")
          "AAAA should exist - C-g should have cleared selection, Backspace only deleted 1 char"))))
