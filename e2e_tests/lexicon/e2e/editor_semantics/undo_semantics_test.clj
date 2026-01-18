(ns lexicon.e2e.editor-semantics.undo-semantics-test
  "E2E tests for Emacs undo semantic - Epic #86

  Tests critical invariant: Undo history is buffer-local"
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

(defn undo []
  "Press C-/ to undo"
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '/',
      code: 'Slash',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 20))

(deftest test-undo-is-buffer-local
  (testing "Emacs invariant: Undo history belongs to buffer, not editor"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type in scratch buffer
    (type-text "buffer-a")
    (Thread/sleep 50)

    ;; Verify initial text
    (let [text-a-before (get-editor-text)]
      (is (.contains text-a-before "buffer-a")
          "Buffer A should have 'buffer-a'"))

    ;; Switch to new buffer (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    ;; Type buffer name in minibuffer
    (e/fill *driver* {:css ".minibuffer-input"} "buffer-b")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in buffer B
    (type-text "buffer-b")
    (Thread/sleep 50)

    ;; Verify buffer B has different content
    (let [text-b (get-editor-text)]
      (is (.contains text-b "buffer-b")
          "Buffer B should have 'buffer-b'"))

    ;; Undo in buffer B
    (undo)
    (Thread/sleep 100)

    ;; Verify buffer B is now empty or has less text
    (let [text-b-after (get-editor-text)]
      (is (or (empty? text-b-after)
              (not (.contains text-b-after "buffer-b")))
          (str "Buffer B should be empty or have less text after undo, got: " text-b-after)))

    ;; Switch back to buffer A (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*scratch*")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify buffer A still has its content (undo in B didn't affect A)
    (let [text-a-after (get-editor-text)]
      (is (.contains text-a-after "buffer-a")
          (str "Buffer A should still have 'buffer-a' - undo in B shouldn't affect A, got: " text-a-after)))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.undo-semantics-test))
