(ns lexicon.editing-enhancements-test
  "E2E tests for editing enhancements - tests USER editing operations.

  Tests keyboard-based editing features:
  - delsel: Delete selection on insert (type over selection)
  - rect: Rectangle operations (C-x r prefix)
  - kmacro: Keyboard macros (F3/F4)
  - electric: Auto-pairing

  Uses keyboard simulation for all editing operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Delete Selection Mode
;; =============================================================================

(deftest test-delete-selection-mode
  (testing "selection replaced on insert"
    (h/setup-test*)
    (h/clear-buffer)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Set mark with C-space
    (h/press-ctrl " ")
    (Thread/sleep 50)

    ;; Move forward to select "Hello"
    (dotimes [_ 5]
      (h/press-key "ArrowRight")
      (Thread/sleep 10))
    (Thread/sleep 50)

    ;; Enable delete-selection-mode via M-x
    (h/execute-command "delete-selection-mode")
    (Thread/sleep 50)

    ;; Typing should replace selection
    (h/type-text "Hi")
    (Thread/sleep 100)

    ;; Should now have "Hi World" instead of "HelloHi World"
    (let [text (h/get-buffer-text*)]
      (is (= "Hi World" text)
          "delete-selection-mode should replace selection"))))

;; =============================================================================
;; Rectangle Operations
;; =============================================================================

(deftest test-rectangle-kill-yank
  (testing "C-x r k kills rectangle"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type multi-line text for rectangle
    (h/type-text "abc")
    (h/press-key "Enter")
    (h/type-text "def")
    (h/press-key "Enter")
    (h/type-text "ghi")
    (Thread/sleep 50)

    ;; Rectangle operations are complex - test placeholder
    (is false "rectangle tested via integration")))

;; =============================================================================
;; Keyboard Macros
;; =============================================================================

(deftest test-keyboard-macro-record
  (testing "F3 starts recording, F4 stops/replays"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Type some text
    (h/type-text "test")
    (Thread/sleep 50)

    ;; Start macro recording with F3
    (h/press-key "F3")
    (Thread/sleep 50)

    ;; Record some keystrokes
    (h/type-text "!")
    (Thread/sleep 50)

    ;; Stop recording with F4
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Macro functionality placeholder
    (is false "kmacro tested via integration")))

;; =============================================================================
;; Electric Pair Mode
;; =============================================================================

(deftest test-electric-pair
  (testing "electric-pair-mode inserts closing bracket"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Enable electric-pair-mode via M-x
    (h/execute-command "electric-pair-mode")
    (Thread/sleep 50)

    ;; Type opening bracket - should auto-insert closing
    (h/type-text "(")
    (Thread/sleep 100)

    ;; Should have "()" in buffer
    (let [text (h/get-buffer-text*)]
      (is (= "()" text)
          "electric-pair should insert closing bracket"))))
