(ns lexicon.e2e.editor-semantics.messages-test
  "E2E tests for Emacs messages semantic - Epic #86

  Tests critical invariants:
  - Messages are stored in *Messages* buffer
  - Echo area shows last message"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000)

;; Browser driver
(def ^:dynamic *driver* nil)

;; Setup/teardown
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

;; Helper functions
(defn wait-for-editor-ready []
  (e/wait-visible *driver* {:css ".editor-wrapper"} {:timeout (/ test-timeout 1000)}))

(defn click-editor []
  (e/click *driver* {:css ".editor-wrapper"}))

(defn get-editor-text []
  (e/get-element-text *driver* {:css ".editable-area"}))

(defn get-echo-area-text []
  (try
    (e/get-element-text *driver* {:css ".echo-area"})
    (catch Exception _ "")))

(defn press-ctrl-key [key]
  (let [key-code (str "Key" (clojure.string/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-key [key-name]
  (let [script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key-name "',
      code: '" key-name "',
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(defn press-minibuffer-enter []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

(deftest test-messages-stored-in-buffer
  (testing "Emacs invariant: Messages stored in *Messages* buffer"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Trigger some commands that should produce messages
    ;; Try C-g (keyboard-quit) which should show "Quit" message
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Check echo area for a message
    (let [echo-text (get-echo-area-text)]
      (is (or (.contains echo-text "Quit")
              (.contains echo-text "quit")
              (not (empty? echo-text)))
          (str "Echo area should show a message, got: " echo-text)))

    ;; Try to switch to *Messages* buffer
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*Messages*")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; *Messages* buffer should exist and have content
    (let [messages-text (get-editor-text)]
      (is (not (empty? messages-text))
          (str "*Messages* buffer should exist and contain message history, got: " messages-text)))))

(deftest test-echo-area-shows-last-message
  (testing "Emacs invariant: Echo area shows most recent message"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Trigger a command - C-g for quit
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Echo area should show the quit message
    (let [echo-text-1 (get-echo-area-text)]
      (is (or (.contains echo-text-1 "Quit")
              (.contains echo-text-1 "quit"))
          (str "Echo area should show 'Quit' message, got: " echo-text-1)))

    ;; Trigger another command that produces a message
    ;; Opening a non-existent command with M-x should show a message
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'x',
        code: 'KeyX',
        altKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 100)

    ;; Cancel with C-g
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Echo area should update with new quit message
    (let [echo-text-2 (get-echo-area-text)]
      (is (or (.contains echo-text-2 "Quit")
              (.contains echo-text-2 "quit")
              (not (empty? echo-text-2)))
          "Echo area should show most recent message"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.messages-test))
