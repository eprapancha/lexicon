(ns lexicon.e2e.editor-semantics.minibuffer-test
  "E2E tests for Emacs minibuffer semantic - Epic #86

  Tests critical invariant:
  - The minibuffer is a real buffer, not a special widget"
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

(defn minibuffer-visible? []
  (try
    (e/visible? *driver* {:css ".minibuffer-input"})
    (catch Exception _ false)))

(defn type-in-minibuffer [text]
  (e/fill *driver* {:css ".minibuffer-input"} text)
  (Thread/sleep 20))

(defn get-minibuffer-text []
  (try
    (e/get-element-value *driver* {:css ".minibuffer-input"})
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

(deftest test-minibuffer-is-a-buffer
  (testing "Emacs invariant: The minibuffer is a real buffer, not a special widget"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Open minibuffer with C-x b
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    ;; Minibuffer should be visible as a real buffer
    (is (minibuffer-visible?)
        "Minibuffer should open when command invoked")

    ;; Should be able to type into minibuffer
    (type-in-minibuffer "test-buffer-name")
    (Thread/sleep 50)

    ;; Should be able to read minibuffer contents
    (let [mb-text (get-minibuffer-text)]
      (is (.contains mb-text "test-buffer-name")
          (str "Should be able to insert and read text from minibuffer, got: '" mb-text "'")))

    ;; Cancel with C-g
    (press-ctrl-key "g")
    (Thread/sleep 100)

    ;; Minibuffer should close
    (let [mb-visible-after (minibuffer-visible?)]
      (is (not mb-visible-after)
          "Minibuffer should close after cancellation"))

    ;; Open minibuffer again to verify buffer persists across activations
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (is (minibuffer-visible?)
        "Minibuffer should be able to reactivate")

    ;; Cancel again
    (press-ctrl-key "g")
    (Thread/sleep 50)))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.minibuffer-test))
