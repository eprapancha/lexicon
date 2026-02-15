(ns lexicon.ui.modes.text-properties-test
  "E2E tests for text properties - user-visible effects of properties.

  Note: Text property APIs (put-text-property, get-text-property) are Lisp
  functions. E2E tests focus on user-visible effects (e.g., invisible text).
  API-specific tests are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Text Property Effects
;; =============================================================================

(deftest test-typing-works-normally
  (testing "Basic text typing works as expected"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    (is (= "Hello World" (h/get-buffer-text*))
        "User can type text normally")))

(deftest test-delete-and-retype
  (testing "Text can be deleted and retyped"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Delete "World" with backspace
    (dotimes [_ 5]
      (h/press-key "Backspace")
      (Thread/sleep 20))
    (Thread/sleep 100)

    (is (= "Hello " (h/get-buffer-text*))
        "Text can be deleted")

    ;; Retype
    (h/type-text "Everyone")
    (Thread/sleep 100)

    (is (= "Hello Everyone" (h/get-buffer-text*))
        "Text can be retyped")))

;; =============================================================================
;; Text Properties API Tests
;;
;; NOTE: Text property APIs (put-text-property, get-text-property, etc.) are
;; Lisp functions. E2E tests focus on user-visible effects. API-specific tests
;; belong in lexicon.lisp namespace when implemented.
;; =============================================================================
