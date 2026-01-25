(ns lexicon.keyboard-demo-test
  "Demo test showing the clean E2E test pattern.

  Uses the simplified helpers from test-helpers:
  - No *driver* var in test file
  - Just use h/with-driver fixture
  - Call h/type-text, h/press-ctrl, etc. directly"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

;; Single line: use the fixture, driver is managed internally
(use-fixtures :once h/with-driver)

(deftest test-typing-and-navigation
  (testing "Basic typing and cursor navigation"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 50)
    (is (= "Hello World" (h/get-buffer-text*)) "Text was typed")

    ;; Navigate to beginning with C-a
    (h/press-ctrl "a")
    (Thread/sleep 30)
    (is (= 0 (h/get-point*)) "C-a moves to beginning")

    ;; Navigate forward with C-f
    (h/press-ctrl "f")
    (Thread/sleep 30)
    (h/press-ctrl "f")
    (Thread/sleep 30)
    (is (= 2 (h/get-point*)) "C-f moves forward")))

(deftest test-backspace
  (testing "Backspace deletes character"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text "abcde")
    (Thread/sleep 50)
    (is (= "abcde" (h/get-buffer-text*)))

    (h/press-key "Backspace")
    (Thread/sleep 30)
    (h/press-key "Backspace")
    (Thread/sleep 30)

    (is (= "abc" (h/get-buffer-text*)) "Backspace removed characters")))

(deftest test-enter-creates-newline
  (testing "Enter creates newline"
    (h/setup-test*)
    (h/clear-buffer)

    (h/type-text "line1")
    (Thread/sleep 30)
    (h/press-key "Enter")
    (Thread/sleep 30)
    (h/type-text "line2")
    (Thread/sleep 50)

    (is (= "line1\nline2" (h/get-buffer-text*)) "Enter created newline")))
