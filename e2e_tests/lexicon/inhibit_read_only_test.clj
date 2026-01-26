(ns lexicon.inhibit-read-only-test
  "E2E tests for read-only buffers - tests that USER TYPING is blocked.

  Read-only buffers block user keyboard input but modes can still update.
  Tests verify user-visible behavior via keyboard simulation.
  API-specific tests (inhibit-read-only) are placeholders for unit tests."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Read-Only Blocks User Keyboard Input
;; Note: Making buffer read-only requires M-x toggle-read-only or C-x C-q
;; =============================================================================

(deftest test-read-only-blocks-user-typing
  (testing "User typing is blocked in read-only buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types initial text
    (h/type-text "Initial content")
    (Thread/sleep 100)

    ;; Make buffer read-only via keyboard (C-x C-q = toggle-read-only)
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Try to type more
    (h/type-text "XXX")
    (Thread/sleep 100)

    ;; Content should be unchanged
    (is (= "Initial content" (h/get-buffer-text*))
        "User typing blocked in read-only buffer")

    ;; Toggle back to writable
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)))

(deftest test-read-only-blocks-user-backspace
  (testing "Backspace is blocked in read-only buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Delete me")
    (Thread/sleep 100)

    ;; Make buffer read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Try to delete with backspace
    (h/press-key "Backspace")
    (Thread/sleep 50)
    (h/press-key "Backspace")
    (Thread/sleep 50)
    (h/press-key "Backspace")
    (Thread/sleep 100)

    ;; Content should be unchanged
    (is (= "Delete me" (h/get-buffer-text*))
        "Backspace blocked in read-only buffer")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

(deftest test-read-only-blocks-user-delete-key
  (testing "Delete key is blocked in read-only buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Delete me")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Make buffer read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Try to delete with Delete key
    (h/press-key "Delete")
    (Thread/sleep 50)
    (h/press-key "Delete")
    (Thread/sleep 50)
    (h/press-key "Delete")
    (Thread/sleep 100)

    ;; Content should be unchanged
    (is (= "Delete me" (h/get-buffer-text*))
        "Delete key blocked in read-only buffer")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

(deftest test-read-only-allows-cursor-movement
  (testing "Cursor movement works in read-only buffer"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Make buffer read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; Cursor movement should work
    (h/press-ctrl "a")  ; Go to beginning
    (Thread/sleep 50)
    (is (= 0 (h/get-point*)) "Ctrl+A works in read-only")

    (h/press-ctrl "e")  ; Go to end
    (Thread/sleep 50)
    (is (= 11 (h/get-point*)) "Ctrl+E works in read-only")

    (h/press-ctrl "b")  ; Backward
    (Thread/sleep 50)
    (is (= 10 (h/get-point*)) "Ctrl+B works in read-only")

    ;; Clean up
    (h/press-ctrl-x "C-q")))

;; =============================================================================
;; inhibit-read-only API Tests - Placeholders for Unit Tests
;; =============================================================================

(deftest test-inhibit-read-only-allows-lisp-insert
  (testing "Programmatic insert succeeds with inhibit-read-only"
    ;; inhibit-read-only is a Lisp API feature
    (is false "inhibit-read-only tested via unit tests")))

(deftest test-inhibit-read-only-allows-lisp-delete
  (testing "Programmatic delete succeeds with inhibit-read-only"
    ;; Lisp API feature
    (is false "inhibit-read-only delete tested via unit tests")))

(deftest test-protection-restored-after-inhibit-scope
  (testing "Protection restored after inhibit scope"
    ;; Lisp API scope behavior
    (is false "inhibit-read-only scope tested via unit tests")))

(deftest test-nested-inhibit-read-only
  (testing "Nested inhibit scopes work"
    ;; Lisp API nesting behavior
    (is false "Nested inhibit-read-only tested via unit tests")))

(deftest test-inhibit-with-errors
  (testing "Protection restored despite error"
    ;; Lisp API error handling
    (is false "inhibit-read-only error handling tested via unit tests")))

;; =============================================================================
;; Mode Patterns - Placeholders for Unit Tests
;; =============================================================================

(deftest test-dired-refresh-pattern
  (testing "Dired refresh pattern - update read-only buffer"
    ;; Dired uses inhibit-read-only internally
    (is false "Dired refresh pattern tested via unit tests")))

(deftest test-help-buffer-pattern
  (testing "Help buffer pattern - link insertion"
    ;; Help buffer uses inhibit-read-only internally
    (is false "Help buffer pattern tested via unit tests")))

;; =============================================================================
;; Query and Toggle
;; =============================================================================

(deftest test-toggle-read-only-keyboard
  (testing "C-x C-q toggles read-only state"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Initially writable - user can type
    (h/type-text "Test")
    (Thread/sleep 100)
    (is (= "Test" (h/get-buffer-text*)) "Initially writable")

    ;; Toggle to read-only
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; User typing should be blocked
    (h/type-text "XXX")
    (Thread/sleep 100)
    (is (= "Test" (h/get-buffer-text*)) "Typing blocked after toggle")

    ;; Toggle back to writable
    (h/press-ctrl-x "C-q")
    (Thread/sleep 100)

    ;; User typing should work now
    (h/type-text "123")
    (Thread/sleep 100)
    (is (= "Test123" (h/get-buffer-text*)) "Typing works after toggle back")))
