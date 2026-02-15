(ns lexicon.ui.modes.font-lock-test
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

(deftest test-user-types-highlightable-code
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

(deftest test-user-types-keywords
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
;; Font Lock API Tests
;;
;; NOTE: Font-lock APIs (font-lock-mode, font-lock-keywords, faces, which-func)
;; are Lisp functions. E2E tests focus on user-visible typing behavior.
;; API-specific tests belong in lexicon.lisp namespace when implemented.
;; =============================================================================
