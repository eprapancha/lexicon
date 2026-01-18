(ns lexicon.e2e.editor-semantics.point-mark-test
  "E2E tests for Emacs point/mark semantic - Epic #86

  Tests critical invariant: Point (cursor) is buffer-local"
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

(deftest test-point-is-buffer-local
  (testing "Emacs invariant: Cursor position preserved per-buffer"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text in scratch buffer
    (type-text "hello world")
    (Thread/sleep 50)

    ;; Move cursor to beginning (C-a)
    (press-ctrl-key "a")
    (Thread/sleep 50)

    ;; Move forward a few characters (C-f C-f C-f)
    (dotimes [_ 3]
      (press-ctrl-key "f")
      (Thread/sleep 20))

    ;; Type marker at current position
    (type-text "X")
    (Thread/sleep 50)

    ;; Should have "helXlo world"
    (let [text-a (e/get-element-text *driver* {:css ".editable-area"})]
      (is (.contains text-a "helXlo")
          "Cursor should be at position 3"))

    ;; Switch to new buffer (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "buffer-b")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in new buffer
    (type-text "xyz")
    (Thread/sleep 50)

    ;; Move to beginning and forward once
    (press-ctrl-key "a")
    (Thread/sleep 20)
    (press-ctrl-key "f")
    (Thread/sleep 20)

    ;; Type marker
    (type-text "Y")
    (Thread/sleep 50)

    ;; Should have "xYyz"
    (let [text-b (e/get-element-text *driver* {:css ".editable-area"})]
      (is (.contains text-b "xYyz")
          "Cursor in buffer B should be at position 1"))

    ;; Switch back to scratch (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*scratch*")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Cursor position in scratch should be preserved
    ;; Type another character - it should insert at saved position
    (type-text "Z")
    (Thread/sleep 50)

    (let [text-a-final (e/get-element-text *driver* {:css ".editable-area"})]
      ;; Cursor should still be around where we left it in buffer A
      ;; Z should be inserted right after X, showing cursor position was preserved
      (is (or (.contains text-a-final "helXZ")
              (.contains text-a-final "XZ"))
          (str "Buffer A cursor position should be preserved - Z after X, got: " text-a-final)))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.point-mark-test))
