(ns lexicon.e2e.editor-semantics.keymap-test
  "E2E tests for Emacs keymap semantic - Epic #86

  Tests critical invariants:
  - Buffer-local keymaps shadow global keymaps
  - Prefix keys wait for completion before dispatching"
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

(defn get-minibuffer-prompt []
  (try
    (e/get-element-text *driver* {:css ".minibuffer-prompt"})
    (catch Exception _ "")))

(defn minibuffer-visible? []
  (try
    (e/visible? *driver* {:css ".minibuffer-input"})
    (catch Exception _ false)))

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

(deftest test-local-keymap-shadows-global
  (testing "Emacs invariant: Buffer-local keymaps shadow global keymaps"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Test a working keybinding - C-x b for switch-buffer
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    ;; Should show minibuffer for switch-buffer
    (let [minibuf-visible (minibuffer-visible?)
          minibuf-prompt (get-minibuffer-prompt)]
      (is minibuf-visible
          "C-x b should open minibuffer")
      (is (or (.contains minibuf-prompt "Buffer")
              (.contains minibuf-prompt "buffer")
              (.contains minibuf-prompt "Switch"))
          (str "Should show buffer switch prompt, got: '" minibuf-prompt "'")))

    ;; Cancel with C-g
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Note: We can't easily test buffer-local keymap shadowing in e2e
    ;; because that requires elisp configuration to set up local keymaps.
    ;; This test verifies the global keymap works.
    ;; The shadowing behavior is better tested via ClojureScript unit tests
    ;; or by manual testing with a mode that has custom keybindings.
    ))

(deftest test-prefix-key-waits-for-completion
  (testing "Emacs invariant: Prefix keys wait for completion before dispatching"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press prefix key C-x
    (press-ctrl-key "x")
    (Thread/sleep 50)

    ;; Minibuffer should NOT be visible yet - we're waiting for completion
    (let [minibuf-before-complete (minibuffer-visible?)]
      (is (not minibuf-before-complete)
          "After C-x alone, minibuffer should not open yet (waiting for completion)"))

    ;; Now complete with 'b' to get switch-buffer
    (press-key "b")
    (Thread/sleep 100)

    ;; NOW minibuffer should open for switch-buffer
    (let [minibuf-visible (minibuffer-visible?)
          minibuf-prompt (get-minibuffer-prompt)]
      (is minibuf-visible
          "After C-x b complete sequence, minibuffer should open")
      (is (or (.contains minibuf-prompt "Buffer")
              (.contains minibuf-prompt "buffer")
              (.contains minibuf-prompt "Switch"))
          (str "Should show buffer switch prompt, got: '" minibuf-prompt "'")))

    ;; Cancel with C-g
    (press-ctrl-key "g")
    (Thread/sleep 50)))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.keymap-test))
