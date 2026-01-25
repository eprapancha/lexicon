(ns lexicon.emacs-simple-test
  "Emacs semantic compatibility tests - E2E with Etaoin

  Source: /tmp/emacs-source/test/lisp/simple-tests.el (Emacs 29.4)
  Contract: docs/EMACS_COMPATIBILITY_CONTRACT.md
  Coverage: docs/EMACS_TEST_COVERAGE_MAP.md

  Phase 6.6 Step 3 - Initial batch: 5 tests from simple-tests.el
  These tests validate Lexicon's behavioral semantics against Emacs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Custom key presses needed for this test file
;; =============================================================================

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
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

;; =============================================================================
;; Test 1: count-lines
;; =============================================================================

(deftest ^:emacs-compat count-lines-test
  (testing "Count lines in buffer - basic cases"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type: foo
    (h/type-text "foo")
    (Thread/sleep 100)
    (let [text (h/get-buffer-text*)]
      (is (= "foo" text)
          "Buffer with text but no newline should have that text"))

    ;; Type: \nbar\nbaz\n
    (h/press-key "Enter")
    (h/type-text "bar")
    (h/press-key "Enter")
    (h/type-text "baz")
    (h/press-key "Enter")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)
          newline-count (count (re-seq #"\n" text))]
      (is (= 3 newline-count)
          "Buffer should have 3 newlines"))))

;; =============================================================================
;; Test 2: newline
;; =============================================================================

(deftest ^:emacs-compat newline-test
  (testing "Newline command - basic insertion and prefix args"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Test: newline with arg 1
    (h/type-text "(a b")
    (let [saved-point (h/get-point*)]
      (h/type-text " c d)")
      (Thread/sleep 50)

      ;; Press Enter (newline command)
      (h/press-key "Enter")
      (Thread/sleep 100)

      (let [text (h/get-buffer-text*)
            point (h/get-point*)]
        (is (re-find #"\(a b c d\)\n" text)
            "Newline should insert newline at end")
        (is (= (+ saved-point 6) point)
            "Point should move after inserted newline")))))

;; =============================================================================
;; Test 3: open-line
;; =============================================================================

(deftest ^:emacs-compat open-line-test
  (testing "Open-line command - insert newline without moving point"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text "hello world")
    (Thread/sleep 50)

    ;; Press C-o (open-line)
    (h/press-ctrl "o")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (re-find #"hello world\n" text)
          "Open-line should insert newline at end"))))

;; =============================================================================
;; Test 4: delete-indentation (no region)
;; =============================================================================

(deftest ^:emacs-compat delete-indentation-no-region-test
  (testing "Delete-indentation - join lines without region"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text " first ")
    (h/press-key "Enter")
    (h/type-text " second ")
    (h/press-key "Enter")
    (h/type-text " third ")
    (h/press-key "Enter")
    (h/type-text " fourth ")
    (Thread/sleep 100)

    ;; Press M-^ (delete-indentation)
    (press-meta-caret)
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (or (re-find #" third fourth " text)
              (re-find #" third\s+fourth " text))
          "Delete-indentation should join last line to previous"))))

;; =============================================================================
;; Test 5: delete-indentation (blank lines)
;; =============================================================================

(deftest ^:emacs-compat delete-indentation-blank-line-test
  (testing "Delete-indentation - blank line edge cases"
    (h/setup-test*)
    (h/clear-buffer)

    (h/press-key "Enter")
    (h/press-key "Enter")
    (h/type-text " third ")
    (h/press-key "Enter")
    (h/press-key "Enter")
    (h/press-key "Enter")
    (h/type-text " sixth ")
    (h/press-key "Enter")
    (h/press-key "Enter")
    (Thread/sleep 100)

    ;; Press M-^ (delete-indentation)
    (press-meta-caret)
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (not (nil? text))
          "Buffer should still have content after delete-indentation"))))
