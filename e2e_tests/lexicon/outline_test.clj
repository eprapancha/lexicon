(ns lexicon.outline-test
  "E2E tests for outline and code folding - user-visible folding behavior.

  Note: Folding commands (outline-hide-subtree, hs-toggle-hiding) are Lisp APIs.
  E2E tests focus on user-visible typing behavior.
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Outline Mode with User-Typed Content
;; =============================================================================

(deftest test-outline-content-typing
  (testing "User can type outline-style headings"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types outline content
    ;; Note: Using lowercase after Enter due to WebDriver/Firefox bug with capitals
    ;; See: https://github.com/anthropics/lexicon/issues/XXX
    (h/type-text "* Heading 1")
    (h/press-key "Enter")
    (h/type-text "content here")  ;; lowercase to work around bug
    (h/press-key "Enter")
    (h/type-text "** Heading 2")
    (h/press-key "Enter")
    (h/type-text "more content")  ;; lowercase to work around bug
    (Thread/sleep 100)

    (is (= "* Heading 1\ncontent here\n** Heading 2\nmore content"
           (h/get-buffer-text*))
        "User can type outline-formatted content")))

(deftest ^:skip test-outline-mode-with-typed-headings
  (testing "outline headings identified in user-typed content"
    ;; outline-minor-mode is a Lisp function
    (is true "PENDING: outline-minor-mode - needs E2E implementation")))

(deftest ^:skip test-outline-heading-collapse
  (testing "user-typed heading can be collapsed"
    ;; outline-hide-subtree is a Lisp function
    (is true "PENDING: outline-hide-subtree - needs E2E implementation")))

(deftest ^:skip test-outline-navigation-in-typed-content
  (testing "navigate between user-typed headings"
    ;; outline-next-heading is a Lisp function
    (is true "PENDING: outline-next-heading - needs E2E implementation")))

;; =============================================================================
;; Hideshow (Code Folding) with User-Typed Code
;; =============================================================================

(deftest test-hs-minor-mode-code-typing
  (testing "User can type foldable code structure"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types code
    (h/type-text "function foo() {")
    (h/press-key "Enter")
    (h/type-text "  body")
    (h/press-key "Enter")
    (h/type-text "}")
    (Thread/sleep 100)

    (is (= "function foo() {\n  body\n}"
           (h/get-buffer-text*))
        "User can type foldable code")))

(deftest ^:skip test-hs-minor-mode-with-typed-code
  (testing "hs-minor-mode with user-typed code block"
    ;; hs-minor-mode is a Lisp function
    (is true "PENDING: hs-minor-mode - needs E2E implementation")))

(deftest ^:skip test-hs-hide-block-on-typed-code
  (testing "code block can be hidden"
    ;; hs-hide-block is a Lisp function
    (is true "PENDING: hs-hide-block - needs E2E implementation")))

(deftest ^:skip test-hs-toggle-hiding-on-typed-code
  (testing "toggle hiding on user-typed code"
    ;; hs-toggle-hiding is a Lisp function
    (is true "PENDING: hs-toggle-hiding - needs E2E implementation")))

;; =============================================================================
;; Integration with Text Properties - Placeholders
;; =============================================================================

(deftest ^:skip test-folding-uses-invisible-property-on-typed-content
  (testing "hidden user-typed text has invisible property"
    ;; invisible property is set via Lisp API
    (is true "PENDING: Invisible property - needs E2E implementation")))
