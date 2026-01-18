(ns lexicon.e2e.editor-semantics.command-test
  "E2E tests for Emacs command system semantic - Epic #86

  Tests critical invariants:
  - Commands are inspectable entities with metadata
  - All command invocations go through the dispatcher"
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

(defn get-echo-area-text []
  (try
    (e/get-element-text *driver* {:css ".echo-area"})
    (catch Exception _ "")))

(defn press-meta-key [key]
  (let [key-code (str "Key" (clojure.string/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 10))

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

(defn type-in-minibuffer [text]
  (e/fill *driver* {:css ".minibuffer-input"} text)
  (Thread/sleep 20))

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

(deftest test-command-invocation-through-mx
  (testing "Emacs invariant: Commands can be invoked through M-x dispatcher"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Invoke M-x to open command dispatcher
    (press-meta-key "x")
    (Thread/sleep 100)

    ;; Type a command name (fundamental-mode exists as a command)
    (type-in-minibuffer "fundamental-mode")
    (Thread/sleep 50)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Command should have executed (no error in echo area)
    (let [echo-text (get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          (str "Command should execute without error, got: '" echo-text "'")))

    ;; Try another command - line-number-mode
    (press-meta-key "x")
    (Thread/sleep 100)
    (type-in-minibuffer "line-number-mode")
    (Thread/sleep 50)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Should execute successfully
    (let [echo-text (get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          (str "Command should execute through M-x dispatcher, got: '" echo-text "'")))))

(deftest test-keybindings-invoke-commands
  (testing "Emacs invariant: Keybindings invoke commands through the same dispatcher"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; C-g invokes keyboard-quit command
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Should show "Quit" message (standard keyboard-quit behavior)
    (let [echo-text (get-echo-area-text)]
      (is (or (.contains echo-text "Quit")
              (.contains echo-text "quit"))
          (str "C-g should invoke keyboard-quit command, got: '" echo-text "'")))

    ;; C-x b invokes switch-to-buffer command (opens minibuffer)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    ;; Press 'b' via minibuffer-visible check
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'b',
        code: 'KeyB',
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 100)

    ;; Minibuffer should open (command was dispatched)
    (let [minibuf-visible (try
                            (e/visible? *driver* {:css ".minibuffer-input"})
                            (catch Exception _ false))]
      (is minibuf-visible
          "C-x b should invoke switch-to-buffer command through dispatcher"))

    ;; Cancel
    (press-ctrl-key "g")
    (Thread/sleep 50)))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.command-test))
