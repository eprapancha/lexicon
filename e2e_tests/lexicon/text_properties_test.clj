(ns lexicon.text-properties-test
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
;; Text Properties API Tests - Placeholders for Unit Tests
;; =============================================================================

(deftest test-text-property-on-typed-text
  (testing "Properties can be set on user-typed text"
    ;; put-text-property and get-text-property are Lisp functions
    (is false "Text property setting tested via unit tests")))

(deftest test-text-property-invisible
  (testing "Invisible property hides text from display"
    ;; Invisible property is set via Lisp API
    (is false "Invisible property tested via unit tests")))

(deftest test-text-properties-adjust-on-keyboard-insert
  (testing "Property range shifts when user types before it"
    ;; Property adjustment requires Lisp API to verify
    (is false "Property adjustment tested via unit tests")))

(deftest test-text-properties-expand-on-keyboard-insert-within
  (testing "Property expands when user types within propertied range"
    ;; Property expansion requires Lisp API to verify
    (is false "Property expansion tested via unit tests")))

(deftest test-text-properties-shrink-on-keyboard-delete
  (testing "Property range shrinks when user deletes from it"
    ;; Property shrinking requires Lisp API to verify
    (is false "Property shrinking tested via unit tests")))

(deftest test-text-property-mouse-face
  (testing "Mouse-face property on typed text"
    ;; Mouse-face is a Lisp property
    (is false "Mouse-face property tested via unit tests")))

(deftest test-text-property-help-echo
  (testing "Help-echo property on typed text"
    ;; Help-echo is a Lisp property
    (is false "Help-echo property tested via unit tests")))

(deftest test-text-properties-multiple
  (testing "Multiple properties coexist on typed text"
    ;; Multiple properties require Lisp API
    (is false "Multiple properties tested via unit tests")))

(deftest test-text-properties-at
  (testing "Get all properties at once"
    ;; text-properties-at is a Lisp function
    (is false "text-properties-at tested via unit tests")))

(deftest test-text-properties-persist-across-undo
  (testing "Properties restored on undo"
    ;; Property restoration requires Lisp API to verify
    ;; User-visible undo is tested in undo_test.clj
    (is false "Property persistence with undo tested via unit tests")))

(deftest test-remove-text-properties
  (testing "remove-text-properties clears specific property"
    ;; remove-text-properties is a Lisp function
    (is false "remove-text-properties tested via unit tests")))

(deftest test-add-text-properties
  (testing "add-text-properties adds without removing"
    ;; add-text-properties is a Lisp function
    (is false "add-text-properties tested via unit tests")))
