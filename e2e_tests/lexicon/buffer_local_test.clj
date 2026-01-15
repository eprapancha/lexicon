(ns lexicon.buffer-local-test
  "E2E tests for Phase 6.5 Week 5-6: Buffer-Local Environments.

  Tests that the infrastructure is in place and commands execute without errors.
  Detailed behavioral testing will require additional UI features."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

;; Use shared test helpers
(use-fixtures :once h/with-driver)

;; File-specific helper functions

(defn press-alt-key [key]
  (let [script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn type-in-minibuffer [text]
  (e/fill h/*driver* {:css ".minibuffer-input"} text)
  (Thread/sleep 100))

(defn press-enter []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        keyCode: 13,
        which: 13,
        bubbles: true,
        cancelable: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 200))

(defn execute-command [command-name]
  (press-alt-key "x")
  (Thread/sleep 100)
  (type-in-minibuffer command-name)
  (press-enter))

(defn get-minibuffer-text []
  (try
    (e/get-element-text h/*driver* {:css ".minibuffer-prompt"})
    (catch Exception _ "")))

;; Tests

(deftest test-minor-mode-commands-registered
  (testing "Minor mode commands are registered - line-number-mode"
    (h/setup-test!)

    ;; Execute the command - if it's not registered, minibuffer would show error
    (execute-command "line-number-mode")
    (Thread/sleep 300)

    ;; If we get here without exceptions, the command executed
    ;; Check that we're back at the editor (not stuck in error state)
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain functional after line-number-mode command")))

(deftest test-auto-save-mode-command
  (testing "Minor mode commands are registered - auto-save-mode"
    (h/setup-test!)

    ;; Execute auto-save-mode
    (execute-command "auto-save-mode")
    (Thread/sleep 300)

    ;; Toggle it again
    (execute-command "auto-save-mode")
    (Thread/sleep 300)

    ;; System should remain stable
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain functional after toggling auto-save-mode")))

(deftest test-modes-namespace-loaded
  (testing "Modes infrastructure loaded without errors"
    (h/setup-test!)

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
    (h/setup-test!)

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
    (h/setup-test!)

    ;; Perform basic operations to ensure no regressions
    ;; Type some text
    (e/fill h/*driver* {:css ".hidden-input"} "hello")
    (Thread/sleep 200)

    ;; Open a buffer
    (execute-command "find-file")
    (type-in-minibuffer "/tmp/test.txt")
    (press-enter)
    (Thread/sleep 300)

    ;; Toggle a mode
    (execute-command "line-number-mode")
    (Thread/sleep 200)

    ;; Type more text
    (e/fill h/*driver* {:css ".hidden-input"} "world")
    (Thread/sleep 200)

    ;;  System should be fully functional
    (is (e/exists? h/*driver* {:css ".editor-wrapper"})
        "Editor should remain fully functional with all infrastructure loaded")))
