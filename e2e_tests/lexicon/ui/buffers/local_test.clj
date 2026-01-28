(ns lexicon.ui.buffers.local-test
  "E2E tests for Phase 6.5 Week 5-6: Buffer-Local Environments.

  Tests that the infrastructure is in place and commands execute without errors.
  Detailed behavioral testing will require additional UI features."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; Tests

(deftest test-minor-mode-commands-registered
  (testing "Minor mode commands are registered - line-number-mode"
    (h/setup-test*)

    ;; Execute the command - if it's not registered, minibuffer would show error
    (h/execute-command "line-number-mode")
    (Thread/sleep 300)

    ;; If we get here without exceptions, the command executed
    ;; Check that we're back at the editor (not stuck in error state)
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain functional after line-number-mode command")))

(deftest test-auto-save-mode-command
  (testing "Minor mode commands are registered - auto-save-mode"
    (h/setup-test*)

    ;; Execute auto-save-mode
    (h/execute-command "auto-save-mode")
    (Thread/sleep 300)

    ;; Toggle it again
    (h/execute-command "auto-save-mode")
    (Thread/sleep 300)

    ;; System should remain stable
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain functional after toggling auto-save-mode")))

(deftest test-modes-namespace-loaded
  (testing "Modes infrastructure loaded without errors"
    (h/setup-test*)

    ;; Check that lexicon.modes namespace is available
    (let [result (e/js-execute h/*driver* "
      try {
        return typeof lexicon !== 'undefined' && typeof lexicon.modes !== 'undefined';
      } catch (e) {
        return false;
      }
    ")]
      (is (true? result) "lexicon.modes namespace should be loaded"))))

(deftest test-variables-namespace-loaded
  (testing "Variables infrastructure loaded without errors"
    (h/setup-test*)

    ;; Check that lexicon.variables namespace is available
    (let [result (e/js-execute h/*driver* "
      try {
        return typeof lexicon !== 'undefined' && typeof lexicon.variables !== 'undefined';
      } catch (e) {
        return false;
      }
    ")]
      (is (true? result) "lexicon.variables namespace should be loaded"))))

(deftest test-system-stability
  (testing "System remains stable with new infrastructure"
    (h/setup-test*)

    ;; Perform basic operations to ensure no regressions
    ;; Type some text
    (h/type-text "hello")
    (Thread/sleep 200)

    ;; Open a buffer
    (h/execute-command "find-file")
    (h/type-in-minibuffer "/tmp/test.txt")
    (h/press-minibuffer-enter)
    (Thread/sleep 300)

    ;; Toggle a mode
    (h/execute-command "line-number-mode")
    (Thread/sleep 200)

    ;; Type more text
    (h/type-text "world")
    (Thread/sleep 200)

    ;;  System should be fully functional
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain fully functional with all infrastructure loaded")))
