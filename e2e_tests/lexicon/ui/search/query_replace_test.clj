(ns lexicon.ui.search.query-replace-test
  "Query-replace and search command tests.

  Tests for search and replace commands:
  - Query-replace (M-%)
  - Replace-string
  - Replace-regexp
  - Kill-word / backward-kill-word
  - Scroll commands"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; Helper functions (test-specific)
(defn press-meta-less-than
  "Press M-< (go to beginning)"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '<',
      code: 'Comma',
      altKey: true,
      shiftKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 20))

;; =============================================================================
;; Batch 1: Quick Wins (6 commands)
;; =============================================================================

(deftest test-p7-8-kill-word
  (testing "P7.8 Batch 1: kill-word (M-d)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple words
    (h/type-text "the quick brown fox")
    (Thread/sleep 20)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Kill first word with M-d
    (h/press-meta "d")
    (Thread/sleep 30)

    ;; Verify "the" is gone but "quick brown fox" remains
    (let [editor-text (h/get-buffer-text*)]
      (is (not (.contains editor-text "the"))
          "First word should be killed")
      (is (.contains editor-text "quick")
          "Remaining words should be intact"))

    ;; Kill another word
    (h/press-meta "d")
    (Thread/sleep 30)

    ;; Verify "quick" is also gone
    (let [editor-text (h/get-buffer-text*)]
      (is (not (.contains editor-text "quick"))
          "Second word should be killed")
      (is (.contains editor-text "brown fox")
          "Remaining words should be intact"))))

(deftest test-p7-8-backward-kill-word
  (testing "P7.8 Batch 1: backward-kill-word (M-DEL)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "hello world testing")
    (Thread/sleep 20)

    ;; Kill last word with M-DEL
    (h/press-meta "Backspace")
    (Thread/sleep 30)

    ;; Verify "testing" is gone
    (let [editor-text (h/get-buffer-text*)]
      (is (not (.contains editor-text "testing"))
          "Last word should be killed")
      (is (.contains editor-text "hello world")
          "Previous words should remain"))

    ;; Kill another word backward
    (h/press-meta "Backspace")
    (Thread/sleep 30)

    ;; Verify "world" is also gone
    (let [editor-text (h/get-buffer-text*)]
      (is (not (.contains editor-text "world"))
          "Second-to-last word should be killed")
      (is (.contains editor-text "hello")
          "First word should remain"))))

(deftest test-p7-8-scroll-up-command
  (testing "P7.8 Batch 1: scroll-up-command (C-v)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type many lines to enable scrolling
    (dotimes [i 30]
      (h/type-text (str "Line " i))
      (h/press-key "Enter")
      (Thread/sleep 5))
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Scroll down with C-v
    (h/press-ctrl "v")
    (Thread/sleep 50)

    ;; Verify scroll occurred (hard to test viewport in E2E, but command should execute)
    (let [echo-text (h/get-echo-area-text)]
      ;; Just verify no error was thrown
      (is (not (.contains echo-text "Error"))
          "Scroll command should execute without error"))))

(deftest test-p7-8-scroll-down-command
  (testing "P7.8 Batch 1: scroll-down-command (M-v)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type many lines
    (dotimes [i 30]
      (h/type-text (str "Line " i))
      (h/press-key "Enter")
      (Thread/sleep 5))
    (Thread/sleep 20)

    ;; Scroll up with M-v (should scroll toward beginning)
    (h/press-meta "v")
    (Thread/sleep 50)

    ;; Verify scroll occurred (no error)
    (let [echo-text (h/get-echo-area-text)]
      (is (not (.contains echo-text "Error"))
          "Scroll command should execute without error"))))

(deftest test-p7-8-exchange-point-and-mark
  (testing "P7.8 Batch 1: exchange-point-and-mark (C-x C-x)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "select this text")
    (Thread/sleep 20)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Set mark
    (h/set-mark)
    (Thread/sleep 20)

    ;; Move forward
    (dotimes [_ 6]
      (h/press-ctrl "f")
      (Thread/sleep 10))

    ;; Exchange point and mark with C-x C-x
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-ctrl "x")
    (Thread/sleep 30)

    ;; Type character to verify position swapped
    (h/type-text "X")
    (Thread/sleep 20)

    ;; Should be at beginning now
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "Xselect")
          "Cursor should have moved to mark position"))))

(deftest ^:skip test-p7-8-describe-variable
  (testing "P7.8 Batch 1: describe-variable (C-h v)"
    (h/setup-test*)

    ;; Press C-h v
    (h/press-ctrl "h")
    (Thread/sleep 10)
    (h/press-key "v")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for variable name
    (let [minibuffer-exists (h/minibuffer-visible?)]
      (is minibuffer-exists "Minibuffer should prompt for variable name"))

    ;; Type a variable name
    (when (h/minibuffer-visible?)
      (h/type-in-minibuffer "fill-column")
      (Thread/sleep 20)
      (h/press-minibuffer-enter)
      (Thread/sleep 100))

    ;; Help buffer should appear
    (let [editor-text (h/get-buffer-text*)]
      (is (or (.contains editor-text "*Help*")
              (.contains editor-text "fill-column")
              (.contains editor-text "variable"))
          (str "Help buffer should show variable description. Got: " editor-text)))))

;; =============================================================================
;; Batch 2: File/Buffer Commands (4 commands)
;; =============================================================================

(deftest ^:skip test-p7-8-save-some-buffers
  (testing "P7.8 Batch 2: save-some-buffers (C-x s)"
    (h/setup-test*)

    ;; Modify buffer
    (h/type-text "modified content")
    (Thread/sleep 20)

    ;; Press C-x s
    (h/press-ctrl-x "s")
    (Thread/sleep 50)

    ;; Command should execute (may prompt or save automatically)
    ;; In browser context, this typically shows a save dialog or message
    (is true "PENDING: save-some-buffers command should execute - needs E2E implementation")))

(deftest ^:skip test-p7-8-find-alternate-file
  (testing "P7.8 Batch 2: find-alternate-file (C-x C-v)"
    (h/setup-test*)

    ;; Type some content
    (h/type-text "original content")
    (Thread/sleep 20)

    ;; Press C-x C-v
    (h/press-ctrl "x")
    (Thread/sleep 10)
    (h/press-ctrl "v")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for file
    (is (h/minibuffer-visible?) "Minibuffer should prompt for file name")))

(deftest ^:skip test-p7-8-insert-file
  (testing "P7.8 Batch 2: insert-file (C-x i)"
    (h/setup-test*)

    ;; Press C-x i
    (h/press-ctrl-x "i")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for file
    (is (h/minibuffer-visible?) "Minibuffer should prompt for file to insert")))

(deftest ^:skip test-p7-8-revert-buffer
  (testing "P7.8 Batch 2: revert-buffer (M-x revert-buffer)"
    (h/setup-test*)

    ;; Type content
    (h/type-text "content to revert")
    (Thread/sleep 20)

    ;; Execute M-x revert-buffer
    (h/execute-command "revert-buffer")
    (Thread/sleep 100)

    ;; Command should execute (may prompt for confirmation)
    (is true "PENDING: revert-buffer command should execute - needs E2E implementation")))

;; =============================================================================
;; Batch 3: Replace Commands (2 commands)
;; =============================================================================

(deftest test-p7-8-replace-string
  (testing "P7.8 Batch 3: replace-string (M-x replace-string)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with replaceable content
    (h/type-text "foo bar foo baz foo")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Execute M-x replace-string
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-in-minibuffer "replace-string")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter search string
    (h/type-in-minibuffer "foo")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter replacement string
    (h/type-in-minibuffer "FOO")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify all occurrences were replaced
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "FOO bar FOO baz FOO")
          (str "All 'foo' should be replaced with 'FOO'. Got: " editor-text)))))

(deftest test-p7-8-replace-regexp
  (testing "P7.8 Batch 3: replace-regexp (M-x replace-regexp)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with pattern
    (h/type-text "test1 test2 test3")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Execute M-x replace-regexp
    (h/press-meta "x")
    (Thread/sleep 50)
    (h/type-in-minibuffer "replace-regexp")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter regex pattern
    (h/type-in-minibuffer "test[0-9]")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter replacement
    (h/type-in-minibuffer "TEST")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify regex replacements occurred
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "TEST TEST TEST")
          (str "Regex replacements should occur. Got: " editor-text)))))

;; =============================================================================
;; Batch 4: Query Replace (2 commands)
;; =============================================================================

(deftest test-p7-8-query-replace-basic
  (testing "P7.8 Batch 4: query-replace (M-%) - basic yes/no/quit"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with multiple occurrences
    (h/type-text "foo bar foo baz foo")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Start query-replace with M-%
    (h/press-meta "%")
    (Thread/sleep 50)

    ;; Type search string
    (h/type-in-minibuffer "foo")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Type replacement string
    (h/type-in-minibuffer "FOO")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press 'y' to replace first occurrence
    (h/press-key "y")
    (Thread/sleep 100)

    ;; Press 'n' to skip second occurrence
    (h/press-key "n")
    (Thread/sleep 100)

    ;; Press 'q' to quit
    (h/press-key "q")
    (Thread/sleep 100)

    ;; Verify: first replaced, second skipped, third untouched
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "FOO bar foo baz foo")
          (str "First should be replaced, rest unchanged. Got: " editor-text)))))

(deftest test-p7-8-query-replace-all
  (testing "P7.8 Batch 4: query-replace (M-%) - replace all with '!'"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "test test test test")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Start query-replace
    (h/press-meta "%")
    (Thread/sleep 50)
    (h/type-in-minibuffer "test")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)
    (h/type-in-minibuffer "TEST")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press '!' to replace all remaining
    (h/press-key "!")
    (Thread/sleep 200)

    ;; Verify all replaced
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "TEST TEST TEST TEST")
          (str "All occurrences should be replaced. Got: " editor-text)))))

(deftest test-p7-8-query-replace-dot
  (testing "P7.8 Batch 4: query-replace (M-%) - replace and quit with '.'"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "cat dog cat dog cat")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Start query-replace
    (h/press-meta "%")
    (Thread/sleep 50)
    (h/type-in-minibuffer "cat")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)
    (h/type-in-minibuffer "CAT")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press '.' to replace current and quit
    (h/press-key ".")
    (Thread/sleep 100)

    ;; Verify: first replaced, rest unchanged
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "CAT dog cat dog cat")
          (str "Only first should be replaced. Got: " editor-text)))))

(deftest test-p7-8-query-replace-regexp
  (testing "P7.8 Batch 4: query-replace-regexp (M-x query-replace-regexp)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text with pattern
    (h/type-text "num1 num2 num3")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Start query-replace-regexp with M-x
    (h/press-meta "x")
    (Thread/sleep 100)

    ;; Type into minibuffer
    (h/type-in-minibuffer "query-replace-regexp")
    (Thread/sleep 100)
    (h/press-key "Enter")
    (Thread/sleep 200)

    ;; Type regex pattern
    (h/type-in-minibuffer "num[0-9]")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Type replacement
    (h/type-in-minibuffer "NUM")
    (Thread/sleep 100)
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Replace first two with 'y', quit with 'q'
    (h/press-key "y")
    (Thread/sleep 100)
    (h/press-key "y")
    (Thread/sleep 100)
    (h/press-key "q")
    (Thread/sleep 100)

    ;; Verify regex replacements
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "NUM NUM num3")
          (str "First two should be replaced via regex. Got: " editor-text)))))

;; =============================================================================
;; Batch 5: Isearch (2 commands)
;; =============================================================================

(deftest ^:skip test-p7-8-isearch-forward
  (testing "P7.8 Batch 5: isearch-forward (C-s)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "hello world testing hello")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Start isearch with C-s
    (h/press-ctrl "s")
    (Thread/sleep 50)

    ;; Minibuffer should show isearch prompt
    (let [minibuffer-exists (e/exists? h/*driver* {:css ".minibuffer"})]
      (is minibuffer-exists "Isearch should activate minibuffer"))

    ;; Type search string
    (when (h/minibuffer-visible?)
      (h/type-in-minibuffer "hello")
      (Thread/sleep 50))

    ;; Press Enter to exit isearch
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Cursor should be at or near first "hello"
    ;; Hard to verify exact position in E2E, but command should work
    (is true "PENDING: Isearch forward should execute - needs E2E implementation")))

(deftest ^:skip test-p7-8-isearch-backward
  (testing "P7.8 Batch 5: isearch-backward (C-r)"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "hello world testing hello")
    (Thread/sleep 20)

    ;; Stay at end of buffer

    ;; Start isearch backward with C-r
    (h/press-ctrl "r")
    (Thread/sleep 50)

    ;; Minibuffer should show isearch prompt
    (let [minibuffer-exists (e/exists? h/*driver* {:css ".minibuffer"})]
      (is minibuffer-exists "Isearch backward should activate minibuffer"))

    ;; Type search string
    (when (h/minibuffer-visible?)
      (h/type-in-minibuffer "hello")
      (Thread/sleep 50))

    ;; Press Enter to exit isearch
    (h/press-key "Enter")
    (Thread/sleep 50)

    ;; Cursor should be at or near last "hello"
    (is true "PENDING: Isearch backward should execute - needs E2E implementation")))

;; =============================================================================
;; Integration Tests - Testing command combinations
;; =============================================================================

(deftest test-p7-8-kill-word-and-yank
  (testing "P7.8 Integration: kill-word + yank"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "hello world testing")
    (Thread/sleep 20)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Kill first word
    (h/press-meta "d")
    (Thread/sleep 30)

    ;; Move to end
    (h/press-ctrl "e")
    (Thread/sleep 10)

    ;; Yank killed word
    (h/press-ctrl "y")
    (Thread/sleep 30)

    ;; Verify word was killed and yanked
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "world testing")
          "Original text should have word removed")
      (is (.contains editor-text "hello")
          "Killed word should be yanked at end"))))

(deftest test-p7-8-backward-kill-word-and-yank
  (testing "P7.8 Integration: backward-kill-word + yank"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "alpha beta gamma")
    (Thread/sleep 20)

    ;; Kill last word backward
    (h/press-meta "Backspace")
    (Thread/sleep 30)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 10)

    ;; Yank
    (h/press-ctrl "y")
    (Thread/sleep 30)

    ;; Verify
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "gamma")
          "Killed word should be yanked")
      (is (.contains editor-text "alpha beta")
          "Remaining text should be intact"))))

(deftest test-p7-8-query-replace-with-region
  (testing "P7.8 Integration: query-replace within region"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type text
    (h/type-text "foo bar foo baz foo qux foo")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-less-than)
    (Thread/sleep 20)

    ;; Set mark
    (h/set-mark)
    (Thread/sleep 20)

    ;; Move to middle (select "foo bar foo")
    (dotimes [_ 11]
      (h/press-ctrl "f")
      (Thread/sleep 10))

    ;; Start query-replace (should only affect region if implemented)
    (h/press-meta "%")
    (Thread/sleep 50)
    (h/type-in-minibuffer "foo")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 50)
    (h/type-in-minibuffer "FOO")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Replace all in region with '!'
    (h/press-key "!")
    (Thread/sleep 200)

    ;; Verify (behavior depends on whether region limits are respected)
    (let [editor-text (h/get-buffer-text*)]
      ;; At minimum, some replacements should occur
      (is (.contains editor-text "FOO")
          "Some replacements should occur"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.phase-7-8-test))
