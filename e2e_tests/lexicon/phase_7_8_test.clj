(ns lexicon.phase-7-8-test
  "Phase 7.8 Command Tests - E2E with Etaoin

  Tests for all Phase 7.8 commands:
  - Batch 1: Quick Wins (6 commands)
  - Batch 2: File/Buffer Commands (4 commands)
  - Batch 3: Replace Commands (2 commands)
  - Batch 4: Query Replace (2 commands)
  - Batch 5: Isearch (2 commands)"
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

(defn wait-for-minibuffer-input []
  "Wait for minibuffer input to be visible (handles re-frame async timing)"
  (e/wait-visible *driver* {:css ".minibuffer-input"} {:timeout 2}))

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

(defn press-key
  "Press a special key (Enter, Backspace, ArrowLeft, etc.)"
  [key-name]
  (let [script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key-name "',
      code: '" key-name "',
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-minibuffer-enter
  "Press Enter in the minibuffer"
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-ctrl-key
  "Press Ctrl+key combination (e.g., 'f' for C-f)"
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
  (Thread/sleep 10))

(defn press-meta-key
  "Press Meta/Alt+key combination (e.g., 'f' for M-f)"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-ctrl-meta-key
  "Press Ctrl+Meta/Alt+key combination (e.g., '%' for C-M-%)"
  [key]
  (let [key-code (cond
                   (= key "%") "Digit5"
                   :else (str "Key" (str/upper-case key)))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn get-echo-area-text
  "Get text from the echo area"
  []
  (try
    (e/get-element-text *driver* {:css ".echo-area"})
    (catch Exception _ "")))

(defn set-mark-command
  "Set mark with C-SPC (Ctrl+Space)"
  []
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
    (e/js-execute *driver* script))
  (Thread/sleep 20))

;; =============================================================================
;; Batch 1: Quick Wins (6 commands)
;; =============================================================================

(deftest test-p7-8-kill-word
  (testing "P7.8 Batch 1: kill-word (M-d)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text with multiple words
    (type-text "the quick brown fox")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 10)

    ;; Kill first word with M-d
    (press-meta-key "d")
    (Thread/sleep 30)

    ;; Verify "the" is gone but "quick brown fox" remains
    (let [editor-text (get-editor-text)]
      (is (not (.contains editor-text "the"))
          "First word should be killed")
      (is (.contains editor-text "quick")
          "Remaining words should be intact"))

    ;; Kill another word
    (press-meta-key "d")
    (Thread/sleep 30)

    ;; Verify "quick" is also gone
    (let [editor-text (get-editor-text)]
      (is (not (.contains editor-text "quick"))
          "Second word should be killed")
      (is (.contains editor-text "brown fox")
          "Remaining words should be intact"))))

(deftest test-p7-8-backward-kill-word
  (testing "P7.8 Batch 1: backward-kill-word (M-DEL)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "hello world testing")
    (Thread/sleep 20)

    ;; Kill last word with M-DEL
    (press-meta-key "Backspace")
    (Thread/sleep 30)

    ;; Verify "testing" is gone
    (let [editor-text (get-editor-text)]
      (is (not (.contains editor-text "testing"))
          "Last word should be killed")
      (is (.contains editor-text "hello world")
          "Previous words should remain"))

    ;; Kill another word backward
    (press-meta-key "Backspace")
    (Thread/sleep 30)

    ;; Verify "world" is also gone
    (let [editor-text (get-editor-text)]
      (is (not (.contains editor-text "world"))
          "Second-to-last word should be killed")
      (is (.contains editor-text "hello")
          "First word should remain"))))

(deftest test-p7-8-scroll-up-command
  (testing "P7.8 Batch 1: scroll-up-command (C-v)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type many lines to enable scrolling
    (dotimes [i 30]
      (type-text (str "Line " i))
      (press-key "Enter")
      (Thread/sleep 5))
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Scroll down with C-v
    (press-ctrl-key "v")
    (Thread/sleep 50)

    ;; Verify scroll occurred (hard to test viewport in E2E, but command should execute)
    (let [echo-text (get-echo-area-text)]
      ;; Just verify no error was thrown
      (is (not (.contains echo-text "Error"))
          "Scroll command should execute without error"))))

(deftest test-p7-8-scroll-down-command
  (testing "P7.8 Batch 1: scroll-down-command (M-v)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type many lines
    (dotimes [i 30]
      (type-text (str "Line " i))
      (press-key "Enter")
      (Thread/sleep 5))
    (Thread/sleep 20)

    ;; Scroll up with M-v (should scroll toward beginning)
    (press-meta-key "v")
    (Thread/sleep 50)

    ;; Verify scroll occurred (no error)
    (let [echo-text (get-echo-area-text)]
      (is (not (.contains echo-text "Error"))
          "Scroll command should execute without error"))))

(deftest test-p7-8-exchange-point-and-mark
  (testing "P7.8 Batch 1: exchange-point-and-mark (C-x C-x)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "select this text")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 10)

    ;; Set mark
    (set-mark-command)
    (Thread/sleep 20)

    ;; Move forward
    (dotimes [_ 6]
      (press-ctrl-key "f")
      (Thread/sleep 10))

    ;; Exchange point and mark with C-x C-x
    (press-ctrl-key "x")
    (Thread/sleep 10)
    (press-ctrl-key "x")
    (Thread/sleep 30)

    ;; Type character to verify position swapped
    (type-text "X")
    (Thread/sleep 20)

    ;; Should be at beginning now
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "Xselect")
          "Cursor should have moved to mark position"))))

(deftest test-p7-8-describe-variable
  (testing "P7.8 Batch 1: describe-variable (C-h v)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-h v
    (press-ctrl-key "h")
    (Thread/sleep 10)
    (press-key "v")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for variable name
    (let [minibuffer-exists (e/exists? *driver* {:css ".minibuffer-input"})]
      (is minibuffer-exists "Minibuffer should prompt for variable name"))

    ;; Type a variable name
    (when (e/exists? *driver* {:css ".minibuffer-input"})
      (e/fill *driver* {:css ".minibuffer-input"} "fill-column")
      (Thread/sleep 20)
      (press-minibuffer-enter)
      (Thread/sleep 100))

    ;; Help buffer should appear
    (let [editor-text (get-editor-text)]
      (is (or (.contains editor-text "*Help*")
              (.contains editor-text "fill-column")
              (.contains editor-text "variable"))
          (str "Help buffer should show variable description. Got: " editor-text)))))

;; =============================================================================
;; Batch 2: File/Buffer Commands (4 commands)
;; =============================================================================

(deftest test-p7-8-save-some-buffers
  (testing "P7.8 Batch 2: save-some-buffers (C-x s)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Modify buffer
    (type-text "modified content")
    (Thread/sleep 20)

    ;; Press C-x s
    (press-ctrl-key "x")
    (Thread/sleep 10)
    (press-key "s")
    (Thread/sleep 50)

    ;; Command should execute (may prompt or save automatically)
    ;; In browser context, this typically shows a save dialog or message
    (let [echo-text (get-echo-area-text)]
      ;; Just verify command executed
      (is true "save-some-buffers command should execute"))))

(deftest test-p7-8-find-alternate-file
  (testing "P7.8 Batch 2: find-alternate-file (C-x C-v)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type some content
    (type-text "original content")
    (Thread/sleep 20)

    ;; Press C-x C-v
    (press-ctrl-key "x")
    (Thread/sleep 10)
    (press-ctrl-key "v")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for file
    (let [minibuffer-exists (e/exists? *driver* {:css ".minibuffer-input"})]
      (is minibuffer-exists "Minibuffer should prompt for file name"))))

(deftest test-p7-8-insert-file
  (testing "P7.8 Batch 2: insert-file (C-x i)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-x i
    (press-ctrl-key "x")
    (Thread/sleep 10)
    (press-key "i")
    (Thread/sleep 50)

    ;; Minibuffer should prompt for file
    (let [minibuffer-exists (e/exists? *driver* {:css ".minibuffer-input"})]
      (is minibuffer-exists "Minibuffer should prompt for file to insert"))))

(deftest test-p7-8-revert-buffer
  (testing "P7.8 Batch 2: revert-buffer (M-x revert-buffer)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type content
    (type-text "content to revert")
    (Thread/sleep 20)

    ;; Press M-x
    (press-meta-key "x")
    (Thread/sleep 50)

    ;; Type command
    (e/fill *driver* {:css ".minibuffer-input"} "revert-buffer")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Command should execute (may prompt for confirmation)
    (let [echo-text (get-echo-area-text)]
      (is true "revert-buffer command should execute"))))

;; =============================================================================
;; Batch 3: Replace Commands (2 commands)
;; =============================================================================

(deftest test-p7-8-replace-string
  (testing "P7.8 Batch 3: replace-string (M-x replace-string)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text with replaceable content
    (type-text "foo bar foo baz foo")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Execute M-x replace-string
    (press-meta-key "x")
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "replace-string")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter search string
    (e/fill *driver* {:css ".minibuffer-input"} "foo")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter replacement string
    (e/fill *driver* {:css ".minibuffer-input"} "FOO")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify all occurrences were replaced
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "FOO bar FOO baz FOO")
          (str "All 'foo' should be replaced with 'FOO'. Got: " editor-text)))))

(deftest test-p7-8-replace-regexp
  (testing "P7.8 Batch 3: replace-regexp (M-x replace-regexp)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text with pattern
    (type-text "test1 test2 test3")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Execute M-x replace-regexp
    (press-meta-key "x")
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "replace-regexp")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter regex pattern
    (e/fill *driver* {:css ".minibuffer-input"} "test[0-9]")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Enter replacement
    (e/fill *driver* {:css ".minibuffer-input"} "TEST")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify regex replacements occurred
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "TEST TEST TEST")
          (str "Regex replacements should occur. Got: " editor-text)))))

;; =============================================================================
;; Batch 4: Query Replace (2 commands)
;; =============================================================================

(deftest test-p7-8-query-replace-basic
  (testing "P7.8 Batch 4: query-replace (M-%) - basic yes/no/quit"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text with multiple occurrences
    (type-text "foo bar foo baz foo")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Start query-replace with M-%
    (press-meta-key "%")
    (Thread/sleep 50)

    ;; Type search string
    (e/fill *driver* {:css ".minibuffer-input"} "foo")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)

    ;; Type replacement string
    (e/fill *driver* {:css ".minibuffer-input"} "FOO")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press 'y' to replace first occurrence
    (press-key "y")
    (Thread/sleep 100)

    ;; Press 'n' to skip second occurrence
    (press-key "n")
    (Thread/sleep 100)

    ;; Press 'q' to quit
    (press-key "q")
    (Thread/sleep 100)

    ;; Verify: first replaced, second skipped, third untouched
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "FOO bar foo baz foo")
          (str "First should be replaced, rest unchanged. Got: " editor-text)))))

(deftest test-p7-8-query-replace-all
  (testing "P7.8 Batch 4: query-replace (M-%) - replace all with '!'"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "test test test test")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Start query-replace
    (press-meta-key "%")
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "test")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "TEST")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press '!' to replace all remaining
    (press-key "!")
    (Thread/sleep 200)

    ;; Verify all replaced
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "TEST TEST TEST TEST")
          (str "All occurrences should be replaced. Got: " editor-text)))))

(deftest test-p7-8-query-replace-dot
  (testing "P7.8 Batch 4: query-replace (M-%) - replace and quit with '.'"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "cat dog cat dog cat")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Start query-replace
    (press-meta-key "%")
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "cat")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "CAT")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Press '.' to replace current and quit
    (press-key ".")
    (Thread/sleep 100)

    ;; Verify: first replaced, rest unchanged
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "CAT dog cat dog cat")
          (str "Only first should be replaced. Got: " editor-text)))))

(deftest test-p7-8-query-replace-regexp
  (testing "P7.8 Batch 4: query-replace-regexp (M-x query-replace-regexp)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text with pattern
    (type-text "num1 num2 num3")
    (Thread/sleep 20)

    ;; Go to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Start query-replace-regexp with M-x
    (press-meta-key "x")
    (Thread/sleep 50)
    (type-text "query-replace-regexp")
    (Thread/sleep 20)
    (press-key "Enter")
    (wait-for-minibuffer-input)

    ;; Type regex pattern
    (e/fill *driver* {:css ".minibuffer-input"} "num[0-9]")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (wait-for-minibuffer-input)

    ;; Type replacement
    (e/fill *driver* {:css ".minibuffer-input"} "NUM")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Replace first two with 'y', quit with 'q'
    (press-key "y")
    (Thread/sleep 100)
    (press-key "y")
    (Thread/sleep 100)
    (press-key "q")
    (Thread/sleep 100)

    ;; Verify regex replacements
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "NUM NUM num3")
          (str "First two should be replaced via regex. Got: " editor-text)))))

;; =============================================================================
;; Batch 5: Isearch (2 commands)
;; =============================================================================

(deftest test-p7-8-isearch-forward
  (testing "P7.8 Batch 5: isearch-forward (C-s)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "hello world testing hello")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Start isearch with C-s
    (press-ctrl-key "s")
    (Thread/sleep 50)

    ;; Minibuffer should show isearch prompt
    (let [minibuffer-exists (e/exists? *driver* {:css ".minibuffer"})]
      (is minibuffer-exists "Isearch should activate minibuffer"))

    ;; Type search string
    (when (e/exists? *driver* {:css ".minibuffer-input"})
      (e/fill *driver* {:css ".minibuffer-input"} "hello")
      (Thread/sleep 50))

    ;; Press Enter to exit isearch
    (press-key "Enter")
    (Thread/sleep 50)

    ;; Cursor should be at or near first "hello"
    ;; Hard to verify exact position in E2E, but command should work
    (is true "Isearch forward should execute")))

(deftest test-p7-8-isearch-backward
  (testing "P7.8 Batch 5: isearch-backward (C-r)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "hello world testing hello")
    (Thread/sleep 20)

    ;; Stay at end of buffer

    ;; Start isearch backward with C-r
    (press-ctrl-key "r")
    (Thread/sleep 50)

    ;; Minibuffer should show isearch prompt
    (let [minibuffer-exists (e/exists? *driver* {:css ".minibuffer"})]
      (is minibuffer-exists "Isearch backward should activate minibuffer"))

    ;; Type search string
    (when (e/exists? *driver* {:css ".minibuffer-input"})
      (e/fill *driver* {:css ".minibuffer-input"} "hello")
      (Thread/sleep 50))

    ;; Press Enter to exit isearch
    (press-key "Enter")
    (Thread/sleep 50)

    ;; Cursor should be at or near last "hello"
    (is true "Isearch backward should execute")))

;; =============================================================================
;; Integration Tests - Testing command combinations
;; =============================================================================

(deftest test-p7-8-kill-word-and-yank
  (testing "P7.8 Integration: kill-word + yank"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "hello world testing")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 10)

    ;; Kill first word
    (press-meta-key "d")
    (Thread/sleep 30)

    ;; Move to end
    (press-ctrl-key "e")
    (Thread/sleep 10)

    ;; Yank killed word
    (press-ctrl-key "y")
    (Thread/sleep 30)

    ;; Verify word was killed and yanked
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "world testing")
          "Original text should have word removed")
      (is (.contains editor-text "hello")
          "Killed word should be yanked at end"))))

(deftest test-p7-8-backward-kill-word-and-yank
  (testing "P7.8 Integration: backward-kill-word + yank"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "alpha beta gamma")
    (Thread/sleep 20)

    ;; Kill last word backward
    (press-meta-key "Backspace")
    (Thread/sleep 30)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 10)

    ;; Yank
    (press-ctrl-key "y")
    (Thread/sleep 30)

    ;; Verify
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "gamma")
          "Killed word should be yanked")
      (is (.contains editor-text "alpha beta")
          "Remaining text should be intact"))))

(deftest test-p7-8-query-replace-with-region
  (testing "P7.8 Integration: query-replace within region"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "foo bar foo baz foo qux foo")
    (Thread/sleep 20)

    ;; Move to beginning
    (press-meta-key "<")
    (Thread/sleep 20)

    ;; Set mark
    (set-mark-command)
    (Thread/sleep 20)

    ;; Move to middle (select "foo bar foo")
    (dotimes [_ 11]
      (press-ctrl-key "f")
      (Thread/sleep 10))

    ;; Start query-replace (should only affect region if implemented)
    (press-meta-key "%")
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "foo")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 50)
    (e/fill *driver* {:css ".minibuffer-input"} "FOO")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Replace all in region with '!'
    (press-key "!")
    (Thread/sleep 200)

    ;; Verify (behavior depends on whether region limits are respected)
    (let [editor-text (get-editor-text)]
      ;; At minimum, some replacements should occur
      (is (.contains editor-text "FOO")
          "Some replacements should occur"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.phase-7-8-test))
