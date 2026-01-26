(ns lexicon.font-lock-test
  "E2E tests for syntax highlighting - user-visible code typing behavior.

  Note: Font-lock APIs (font-lock-mode, font-lock-keywords, faces) are Lisp
  functions. E2E tests focus on user-visible typing of highlightable code.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Code Typing
;; =============================================================================

(deftest ^:skip test-user-types-highlightable-code
  (testing "User can type code that would be syntax highlighted"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types a function definition
    (h/type-text "(defn my-function []")
    (h/press-key "Enter")
    (h/type-text "  (+ 1 2))")
    (Thread/sleep 100)

    (is (= "(defn my-function []\n  (+ 1 2))"
           (h/get-buffer-text*))
        "User can type code structure")))

(deftest ^:skip test-user-types-keywords
  (testing "User can type code with keywords"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types code with keywords that would be highlighted
    (h/type-text "if (condition) {")
    (h/press-key "Enter")
    (h/type-text "  return true;")
    (h/press-key "Enter")
    (h/type-text "}")
    (Thread/sleep 100)

    (is (= "if (condition) {\n  return true;\n}"
           (h/get-buffer-text*))
        "User can type code with keywords")))

;; =============================================================================
;; Font Lock Mode Activation - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-font-lock-mode-activation
  (testing "font-lock-mode can be enabled"
    ;; font-lock-mode is a Lisp function
    (is true "PENDING: font-lock-mode - needs E2E implementation")))

;; =============================================================================
;; Font Lock Keywords - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-font-lock-keywords
  (testing "font-lock-keywords list is accessible"
    ;; font-lock-keywords is a Lisp variable
    (is true "PENDING: font-lock-keywords - needs E2E implementation")))

(deftest ^:skip test-font-lock-add-keywords
  (testing "font-lock-add-keywords extends rules"
    ;; font-lock-add-keywords is a Lisp function
    (is true "PENDING: font-lock-add-keywords - needs E2E implementation")))

;; =============================================================================
;; Font Lock Faces - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-font-lock-faces
  (testing "font-lock faces are defined"
    ;; face-attribute is a Lisp function
    (is true "PENDING: font-lock faces - needs E2E implementation")))

;; =============================================================================
;; Which Function Mode - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-which-function-mode
  (testing "which-function returns function name"
    ;; which-function is a Lisp function
    (is true "PENDING: which-function - needs E2E implementation")))

(deftest ^:skip test-which-func-mode-line
  (testing "which-func-mode shows in mode line"
    ;; which-func-mode is a Lisp function
    (is true "PENDING: which-func-mode - needs E2E implementation")))
