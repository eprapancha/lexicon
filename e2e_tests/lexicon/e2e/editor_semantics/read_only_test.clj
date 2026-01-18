(ns lexicon.e2e.editor-semantics.read-only-test
  "E2E tests for Emacs read-only buffer semantic - Epic #86

  Tests critical invariant: Read-only buffers prevent modification"
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

(defn type-text [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

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

(deftest test-read-only-buffer-prevents-modification
  (testing "Emacs invariant: Read-only buffers reject modifications"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Switch to *Messages* buffer which is typically read-only
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*Messages*")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Get initial content
    (let [initial-text (get-editor-text)]
      ;; Try to type in the read-only buffer
      (type-text "SHOULD-NOT-APPEAR")
      (Thread/sleep 100)

      ;; Text should not have changed
      (let [after-text (get-editor-text)
            echo-text (get-echo-area-text)]
        (is (not (.contains after-text "SHOULD-NOT-APPEAR"))
            (str "Read-only buffer should reject typing, got: " after-text))

        ;; Echo area might show error message
        (is (or (not (.contains after-text "SHOULD-NOT-APPEAR"))
                (.contains echo-text "read-only")
                (.contains echo-text "Read-only"))
            "Should reject edit or show read-only message")))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.read-only-test))
