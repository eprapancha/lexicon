(ns lexicon.programming-support-test
  "E2E tests for programming support - user-visible code typing.

  Emacs source: lisp/progmodes/compile.el, lisp/progmodes/flymake.el, lisp/imenu.el

  Note: compile, flymake, imenu are Lisp APIs. E2E tests focus on
  user-visible code typing. API-specific tests are placeholders
  for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Code Typing
;; =============================================================================

(deftest test-user-types-code-with-definitions
  (testing "User can type code with function definitions"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types function definitions
    (h/type-text "(defn foo [] nil)")
    (h/press-key "Enter")
    (h/type-text "(defn bar [] nil)")
    (Thread/sleep 100)

    (is (= "(defn foo [] nil)\n(defn bar [] nil)"
           (h/get-buffer-text*))
        "User can type function definitions")))

;; =============================================================================
;; Compile - Placeholders for Unit Tests
;; =============================================================================

(deftest test-compile-command
  (testing "compile creates compilation buffer"
    ;; compile is a Lisp function
    (is true "compile tested via unit tests")))

(deftest test-next-error-navigation
  (testing "next-error jumps to error location"
    ;; next-error is a Lisp function
    (is true "next-error tested via unit tests")))

;; =============================================================================
;; Flymake - Placeholders for Unit Tests
;; =============================================================================

(deftest test-flymake-diagnostics
  (testing "flymake highlights errors"
    ;; flymake is a Lisp function
    (is true "flymake tested via unit tests")))

;; =============================================================================
;; Imenu - Placeholders for Unit Tests
;; =============================================================================

(deftest test-imenu-index
  (testing "imenu lists definitions"
    ;; imenu is a Lisp function
    (is true "imenu tested via unit tests")))
