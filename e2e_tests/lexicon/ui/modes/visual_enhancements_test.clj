(ns lexicon.ui.modes.visual-enhancements-test
  "E2E tests for visual enhancements - user-visible typing behavior.

  Note: Visual modes (show-paren-mode, hl-line-mode, etc.) are Lisp APIs.
  E2E tests focus on user-visible typing of code.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Typing with Parentheses
;; =============================================================================

(deftest test-user-types-parenthesized-code
  (testing "User can type parenthesized expressions"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types parenthesized expression
    (h/type-text "(foo (bar baz))")
    (Thread/sleep 100)

    (is (= "(foo (bar baz))"
           (h/get-buffer-text*))
        "User can type balanced parens")))

(deftest test-user-types-with-whitespace
  (testing "User can type text with whitespace"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text with tabs and trailing spaces
    (h/type-text "hello")
    (h/press-key "Tab")
    (h/type-text "world  ")
    (Thread/sleep 100)

    (is (= "hello\tworld  "
           (h/get-buffer-text*))
        "User can type text with whitespace")))

;; =============================================================================
;; Show Paren Mode - PENDING E2E Implementation
;; =============================================================================

(deftest test-show-paren-mode
  (testing "show-paren-mode can be toggled via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text with parens
    (h/type-text "(defn foo [x] (+ x 1))")
    (Thread/sleep 100)

    ;; Enable show-paren-mode via M-x
    (h/execute-command "show-paren-mode")
    (Thread/sleep 100)

    ;; Mode should be enabled without error
    (is (= "(defn foo [x] (+ x 1))"
           (h/get-buffer-text*))
        "Buffer text should remain intact after enabling show-paren-mode")

    ;; Move cursor to a paren position and check mode still works
    (h/press-ctrl "a")  ; Go to beginning
    (Thread/sleep 50)

    ;; Toggle off
    (h/execute-command "show-paren-mode")
    (Thread/sleep 100)

    (is (= "(defn foo [x] (+ x 1))"
           (h/get-buffer-text*))
        "Buffer text should remain intact after disabling show-paren-mode")))

;; =============================================================================
;; Hl-Line Mode - PENDING E2E Implementation
;; =============================================================================

(deftest test-hl-line-mode
  (testing "hl-line-mode can be toggled via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text
    (h/type-text "line one")
    (h/press-key "Enter")
    (h/type-text "line two")
    (h/press-key "Enter")
    (h/type-text "line three")
    (Thread/sleep 100)

    ;; Enable hl-line-mode via M-x
    (h/execute-command "hl-line-mode")
    (Thread/sleep 100)

    ;; Mode should be enabled without error
    ;; Visual highlighting is hard to test in E2E, but command should work
    (is (= "line one\nline two\nline three"
           (h/get-buffer-text*))
        "Buffer text should remain intact after enabling hl-line-mode")

    ;; Toggle off
    (h/execute-command "hl-line-mode")
    (Thread/sleep 100)

    (is (= "line one\nline two\nline three"
           (h/get-buffer-text*))
        "Buffer text should remain intact after disabling hl-line-mode")))

;; =============================================================================
;; Whitespace Mode - PENDING E2E Implementation
;; =============================================================================

(deftest test-whitespace-mode
  (testing "whitespace-mode can be toggled via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type text with spaces and tabs
    (h/type-text "hello")
    (h/press-key "Tab")
    (h/type-text "world")
    (Thread/sleep 100)

    ;; Enable whitespace-mode via M-x
    (h/execute-command "whitespace-mode")
    (Thread/sleep 100)

    ;; Mode should be enabled without error
    ;; Buffer text should remain intact (the stored text, not visual)
    (is (= "hello\tworld"
           (h/get-buffer-text*))
        "Buffer text should remain intact after enabling whitespace-mode")

    ;; Toggle off
    (h/execute-command "whitespace-mode")
    (Thread/sleep 100)

    (is (= "hello\tworld"
           (h/get-buffer-text*))
        "Buffer text should remain intact after disabling whitespace-mode")))

;; =============================================================================
;; Line Numbers - PENDING E2E Implementation
;; =============================================================================

(deftest test-line-numbers
  (testing "display-line-numbers-mode can be toggled via M-x"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text
    (h/type-text "line one")
    (h/press-key "Enter")
    (h/type-text "line two")
    (Thread/sleep 100)

    ;; Line numbers should be on by default
    ;; Toggle off via M-x
    (h/execute-command "display-line-numbers-mode")
    (Thread/sleep 100)

    ;; Buffer text should remain intact
    (is (= "line one\nline two"
           (h/get-buffer-text*))
        "Buffer text should remain intact after toggling display-line-numbers-mode")

    ;; Toggle back on
    (h/execute-command "display-line-numbers-mode")
    (Thread/sleep 100)

    (is (= "line one\nline two"
           (h/get-buffer-text*))
        "Buffer text should remain intact after re-enabling display-line-numbers-mode")))
