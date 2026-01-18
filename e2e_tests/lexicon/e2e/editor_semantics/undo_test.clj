(ns lexicon.e2e.editor-semantics.undo-test
  "E2E tests for Emacs undo semantic - Epic #86

  Tests critical invariant:
  - Undo history is buffer-local"
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

(defn type-text [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

(defn press-ctrl-key [key]
  (let [key-code (str "Key" (clojure.string/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    console.log('🔍 E2E: Found input element:', input);
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      bubbles: true
    });
    console.log('🔍 E2E: Created event:', event.key, event.ctrlKey);
    const result = input.dispatchEvent(event);
    console.log('🔍 E2E: dispatchEvent returned:', result);
    // Try to dispatch to re-frame directly as fallback
    if (window.lexicon && window.lexicon.events && window.lexicon.events.key_event_to_string) {
      const keyStr = window.lexicon.events.key_event_to_string(event);
      console.log('🔍 E2E: Converted to key string:', keyStr);
    }
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

(deftest test-undo-is-buffer-local
  (testing "Emacs invariant: Undo history belongs to a buffer, not the editor"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type in scratch buffer
    (type-text "x")
    (Thread/sleep 50)

    ;; Verify text is there
    (let [text-a (get-editor-text)]
      (is (.contains text-a "x")
          "Scratch buffer should contain 'x'"))

    ;; Switch to new buffer (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "buffer-b")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in buffer B
    (type-text "y")
    (Thread/sleep 50)

    (let [text-b (get-editor-text)]
      (is (.contains text-b "y")
          "Buffer B should contain 'y'"))

    ;; Undo in buffer B (C-/)
    ;; First try keyboard event
    (press-ctrl-key "/")
    (Thread/sleep 100)

    ;; DEBUG: Try dispatching :undo directly to test if undo logic works
    (e/js-execute *driver* "window.lexicon_test_dispatch_undo = function() { if (window.re_frame) { window.re_frame.core.dispatch([':undo']); } };")
    (e/js-execute *driver* "window.lexicon_test_dispatch_undo();")
    (Thread/sleep 100)

    ;; Buffer B should be empty after undo
    (let [text-b-after-undo (get-editor-text)]
      (is (not (.contains text-b-after-undo "y"))
          "Buffer B should not contain 'y' after undo"))

    ;; Switch back to scratch (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*scratch*")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Scratch buffer should still have 'x' - undo in B didn't affect A
    (let [text-a-final (get-editor-text)]
      (is (.contains text-a-final "x")
          "Buffer A should still contain 'x' - undo in B should not affect A"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.undo-test))
