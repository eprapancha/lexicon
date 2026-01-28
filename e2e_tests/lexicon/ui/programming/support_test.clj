(ns lexicon.ui.programming.support-test
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
;; Compile - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-compile-command
  (testing "compile creates compilation buffer"
    ;; compile is a Lisp function
    (is true "PENDING: compile - needs E2E implementation")))

(deftest ^:skip test-next-error-navigation
  (testing "next-error jumps to error location"
    ;; next-error is a Lisp function
    (is true "PENDING: next-error - needs E2E implementation")))

;; =============================================================================
;; Flymake - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-flymake-diagnostics
  (testing "flymake highlights errors"
    ;; flymake is a Lisp function
    (is true "PENDING: flymake - needs E2E implementation")))

;; =============================================================================
;; Imenu - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-imenu-index
  (testing "imenu lists definitions"
    ;; imenu is a Lisp function
    (is true "PENDING: imenu - needs E2E implementation")))
