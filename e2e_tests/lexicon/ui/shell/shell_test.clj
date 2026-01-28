(ns lexicon.ui.shell.shell-test
  "E2E tests for shell mode - user-visible behavior.

  Emacs source: lisp/shell.el, lisp/eshell/

  Note: shell, eshell are Lisp APIs. E2E tests focus on user-visible
  behavior. API-specific tests are pending E2E implementation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Shell Access via Keyboard
;; =============================================================================

(deftest ^:skip test-user-opens-shell-via-mx
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
    (is true "PENDING: Shell opened via M-x - needs E2E implementation")))

;; =============================================================================
;; Shell Buffer - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-shell-opens-buffer
  (testing "shell command creates buffer"
    ;; shell is a Lisp function
    (is true "PENDING: shell - needs E2E implementation")))

(deftest ^:skip test-shell-executes-commands
  (testing "command output appears"
    ;; shell command execution is a Lisp feature
    (is true "PENDING: shell execution - needs E2E implementation")))

(deftest ^:skip test-shell-directory-tracking
  (testing "directory tracking works"
    ;; directory tracking is a Lisp feature
    (is true "PENDING: directory tracking - needs E2E implementation")))

;; =============================================================================
;; Shell Mode - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-shell-mode-keybindings
  (testing "shell mode bindings exist"
    ;; shell-mode is a Lisp feature
    (is true "PENDING: shell mode - needs E2E implementation")))

(deftest ^:skip test-shell-input-history
  (testing "history navigation works"
    ;; comint history is a Lisp feature
    (is true "PENDING: history - needs E2E implementation")))
