(ns lexicon.ui.editing.enhancements-test
  "E2E tests for editing enhancements - tests USER editing operations.

  Tests keyboard-based editing features:
  - kmacro: Keyboard macros (F3/F4)
  - electric: Auto-pairing

  Uses keyboard simulation for all editing operations.

  Note: Tests requiring Lisp setup (delete-selection, rectangles) are in
  lexicon.lisp.editing.primitives-test"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Keyboard Macros
;; =============================================================================

(deftest test-keyboard-macro-record
  (testing "F3 starts recording, F4 stops and replays"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Start macro recording via F3
    (h/press-key "F3")
    (Thread/sleep 100)

    ;; Type "Hi" - this will be recorded and inserted
    (h/type-text "Hi")
    (Thread/sleep 100)

    ;; Stop recording with F4
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Verify text is there
    (let [text1 (h/get-buffer-text*)]
      (is (= "Hi" text1) "Buffer should have 'Hi'"))

    ;; Replay the macro with F4
    (h/press-key "F4")
    (Thread/sleep 200)

    ;; Should now have "HiHi" (original + replay)
    (let [text2 (h/get-buffer-text*)]
      (is (= "HiHi" text2) "Replaying macro should append 'Hi'")))

  (testing "C-x e also replays last macro"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Record a simple macro
    (h/press-key "F3")
    (Thread/sleep 100)
    (h/type-text "X")
    (Thread/sleep 100)
    (h/press-key "F4")
    (Thread/sleep 100)

    ;; Clear buffer
    (h/clear-buffer)
    (Thread/sleep 50)

    ;; Use C-x e to replay
    (h/press-ctrl "x")
    (Thread/sleep 50)
    (h/press-key "e")
    (Thread/sleep 200)

    ;; Should have "X"
    (let [text (h/get-buffer-text*)]
      (is (= "X" text) "C-x e should replay last macro"))))

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
