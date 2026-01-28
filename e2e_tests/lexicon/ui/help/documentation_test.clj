(ns lexicon.ui.help.documentation-test
  "E2E tests for documentation features - user-visible help access.

  Note: eldoc, apropos, describe-* are Lisp APIs. E2E tests focus on
  user-visible behavior. API-specific tests are placeholders for
  unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Typing in Code Context
;; =============================================================================

(deftest test-user-types-function-call
  (testing "User can type function call (eldoc context)"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types a function call
    (h/type-text "(insert \"hello\")")
    (Thread/sleep 100)

    (is (= "(insert \"hello\")"
           (h/get-buffer-text*))
        "User can type function calls")))

;; =============================================================================
;; Eldoc - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-eldoc-display
  (testing "eldoc displays function signature"
    ;; eldoc-documentation-function is a Lisp function
    (is true "PENDING: eldoc - needs E2E implementation")))

;; =============================================================================
;; Apropos - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-apropos-search
  (testing "apropos-command finds matches"
    ;; apropos-command is a Lisp function
    (is true "PENDING: apropos - needs E2E implementation")))

;; =============================================================================
;; Describe Functions - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-describe-function
  (testing "describe-function creates help buffer"
    ;; describe-function is a Lisp function
    (is true "PENDING: describe-function - needs E2E implementation")))

(deftest ^:skip test-describe-key
  (testing "describe-key shows command for key"
    ;; describe-key is a Lisp function
    (is true "PENDING: describe-key - needs E2E implementation")))
