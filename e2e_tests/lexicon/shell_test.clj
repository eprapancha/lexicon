(ns lexicon.shell-test
  "E2E tests for shell mode - user-visible behavior.

  Emacs source: lisp/shell.el, lisp/eshell/

  Note: shell, eshell are Lisp APIs. E2E tests focus on user-visible
  behavior. API-specific tests are placeholders for unit tests."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Shell Access via Keyboard
;; =============================================================================

(deftest test-user-opens-shell-via-mx
  (testing "User can open shell via M-x"
    (h/setup-test*)
    (h/clear-buffer)

    ;; M-x shell
    (h/press-meta "x")
    (Thread/sleep 200)

    (h/type-in-minibuffer "shell")
    (Thread/sleep 100)

    ;; Press Enter to execute
    (h/press-key "Enter")
    (Thread/sleep 200)

    ;; Should have opened something (may fail if shell not implemented)
    (is false "Shell opened via M-x")))

;; =============================================================================
;; Shell Buffer - Placeholders for Unit Tests
;; =============================================================================

(deftest test-shell-opens-buffer
  (testing "shell command creates buffer"
    ;; shell is a Lisp function
    (is false "shell tested via unit tests")))

(deftest test-shell-executes-commands
  (testing "command output appears"
    ;; shell command execution is a Lisp feature
    (is false "shell execution tested via unit tests")))

(deftest test-shell-directory-tracking
  (testing "directory tracking works"
    ;; directory tracking is a Lisp feature
    (is false "directory tracking tested via unit tests")))

;; =============================================================================
;; Shell Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-shell-mode-keybindings
  (testing "shell mode bindings exist"
    ;; shell-mode is a Lisp feature
    (is false "shell mode tested via unit tests")))

(deftest test-shell-input-history
  (testing "history navigation works"
    ;; comint history is a Lisp feature
    (is false "history tested via unit tests")))
