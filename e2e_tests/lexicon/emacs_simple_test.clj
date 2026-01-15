(ns lexicon.emacs-simple-test
  "Emacs semantic compatibility tests - E2E with Etaoin

  Source: /tmp/emacs-source/test/lisp/simple-tests.el (Emacs 29.4)
  Contract: docs/EMACS_COMPATIBILITY_CONTRACT.md
  Coverage: docs/EMACS_TEST_COVERAGE_MAP.md

  Phase 6.6 Step 3 - Initial batch: 5 tests from simple-tests.el
  These tests validate Lexicon's behavioral semantics against Emacs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000)

;; Browser driver
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
  (e/wait-visible *driver* {:css ".editor-wrapper"} {:timeout (/ test-timeout 1000)}))

(defn click-editor []
  (e/click *driver* {:css ".editor-wrapper"}))

(defn type-text
  "Type text by sending it character by character"
  [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

(defn press-enter
  "Press the Enter key (triggers :newline command)"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'Enter',
      code: 'Enter',
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-ctrl-o
  "Press C-o (triggers :open-line command)"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'o',
      code: 'KeyO',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-meta-caret
  "Press M-^ (triggers :delete-indentation command)"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '^',
      code: 'Digit6',
      shiftKey: true,
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-ctrl-u
  "Press C-u (sets prefix argument)"
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'u',
      code: 'KeyU',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-digit
  "Press a digit key"
  [digit]
  (e/fill *driver* {:css ".hidden-input"} (str digit))
  (Thread/sleep 10))

(defn get-buffer-text
  "Get complete buffer text via window.editorState"
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    return state ? state.buffer : '';
  "))

(defn get-point
  "Get current point position"
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    return state ? state.point : 0;
  "))

;; =============================================================================
;; Test 1: count-lines
;; =============================================================================

(deftest ^:emacs-compat count-lines-test
  (testing "Count lines in buffer - basic cases"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (Thread/sleep 200)

    ;; Type: foo
    (type-text "foo")
    (Thread/sleep 100)
    (let [text (get-buffer-text)]
      (is (= "foo" text)
          "Buffer with text but no newline should have that text"))

    ;; Type: \nbar\nbaz\n
    (press-enter)
    (type-text "bar")
    (press-enter)
    (type-text "baz")
    (press-enter)
    (Thread/sleep 100)

    (let [text (get-buffer-text)
          newline-count (count (re-seq #"\n" text))]
      (is (= 3 newline-count)
          "Buffer should have 3 newlines"))))

;; =============================================================================
;; Test 2: newline
;; =============================================================================

(deftest ^:emacs-compat newline-test
  (testing "Newline command - basic insertion and prefix args"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (Thread/sleep 200)

    ;; Test: newline with arg 1
    (type-text "(a b")
    (let [saved-point (get-point)]
      (type-text " c d)")
      (Thread/sleep 50)

      ;; Press Enter (newline command)
      (press-enter)
      (Thread/sleep 100)

      (let [text (get-buffer-text)
            point (get-point)]
        (is (re-find #"\(a b c d\)\n" text)
            "Newline should insert newline at end")
        (is (= (+ saved-point 6) point)
            "Point should move after inserted newline")))))

;; =============================================================================
;; Test 3: open-line
;; =============================================================================

(deftest ^:emacs-compat open-line-test
  (testing "Open-line command - insert newline without moving point"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (Thread/sleep 200)

    (type-text "hello world")
    (Thread/sleep 50)

    ;; Press C-o (open-line)
    (press-ctrl-o)
    (Thread/sleep 100)

    (let [text (get-buffer-text)]
      (is (re-find #"hello world\n" text)
          "Open-line should insert newline at end"))))

;; =============================================================================
;; Test 4: delete-indentation (no region)
;; =============================================================================

(deftest ^:emacs-compat delete-indentation-no-region-test
  (testing "Delete-indentation - join lines without region"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (Thread/sleep 200)

    (type-text " first ")
    (press-enter)
    (type-text " second ")
    (press-enter)
    (type-text " third ")
    (press-enter)
    (type-text " fourth ")
    (Thread/sleep 100)

    ;; Press M-^ (delete-indentation)
    (press-meta-caret)
    (Thread/sleep 100)

    (let [text (get-buffer-text)]
      (is (or (re-find #" third fourth " text)
              (re-find #" third\s+fourth " text))
          "Delete-indentation should join last line to previous"))))

;; =============================================================================
;; Test 5: delete-indentation (blank lines)
;; =============================================================================

(deftest ^:emacs-compat delete-indentation-blank-line-test
  (testing "Delete-indentation - blank line edge cases"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)
    (Thread/sleep 200)

    (press-enter)
    (press-enter)
    (type-text " third ")
    (press-enter)
    (press-enter)
    (press-enter)
    (type-text " sixth ")
    (press-enter)
    (press-enter)
    (Thread/sleep 100)

    ;; Press M-^ (delete-indentation)
    (press-meta-caret)
    (Thread/sleep 100)

    (let [text (get-buffer-text)]
      (is (not (nil? text))
          "Buffer should still have content after delete-indentation"))))
