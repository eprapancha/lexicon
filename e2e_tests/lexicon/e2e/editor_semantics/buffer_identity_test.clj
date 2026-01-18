(ns lexicon.e2e.editor-semantics.buffer-identity-test
  "E2E tests for Emacs buffer identity semantic - Epic #86

  Tests critical invariant: Buffer identity is stable across content changes"
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

(deftest test-buffer-identity-stable-across-content
  (testing "Emacs invariant: Buffer identity is stable independent of content changes"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type initial text
    (type-text "initial")
    (Thread/sleep 50)

    ;; Verify initial text
    (let [text-1 (get-editor-text)]
      (is (.contains text-1 "initial")
          "Should have 'initial' text"))

    ;; Type more text - this should append
    (type-text " more")
    (Thread/sleep 50)

    ;; Verify text appended correctly
    (let [text-2 (get-editor-text)]
      (is (.contains text-2 "initial more")
          (str "Text should be 'initial more', got: " text-2))
      (is (not (.contains text-2 " moreinitial"))
          "Text should NOT be in wrong order"))))

(deftest test-multiple-buffers-can-coexist
  (testing "Emacs invariant: Multiple independent buffers can exist simultaneously"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type in scratch buffer
    (type-text "content-a")
    (Thread/sleep 50)

    ;; Switch to new buffer (C-x b)
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event1 = new KeyboardEvent('keydown', {
        key: 'x', code: 'KeyX', ctrlKey: true, bubbles: true
      });
      input.dispatchEvent(event1);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 20)
    (e/fill *driver* {:css ".hidden-input"} "b")
    (Thread/sleep 100)

    ;; Type buffer name in minibuffer
    (e/fill *driver* {:css ".minibuffer-input"} "buffer-b")
    (Thread/sleep 20)
    (let [script "
      const input = document.querySelector('.minibuffer-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter', code: 'Enter', bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 100)

    ;; Type in new buffer
    (type-text "content-b")
    (Thread/sleep 50)

    ;; Verify new buffer has different content
    (let [text-b (get-editor-text)]
      (is (or (.contains text-b "content-b")
              (not (.contains text-b "content-a")))
          (str "Buffer B should have 'content-b', got: " text-b)))

    ;; Switch back to scratch (C-x b)
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event1 = new KeyboardEvent('keydown', {
        key: 'x', code: 'KeyX', ctrlKey: true, bubbles: true
      });
      input.dispatchEvent(event1);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 20)
    (e/fill *driver* {:css ".hidden-input"} "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "*scratch*")
    (Thread/sleep 20)
    (let [script "
      const input = document.querySelector('.minibuffer-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter', code: 'Enter', bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 100)

    ;; Verify scratch buffer preserved
    (let [text-a (get-editor-text)]
      (is (.contains text-a "content-a")
          (str "Scratch buffer should still have 'content-a', got: " text-a)))

    ;; Type more in buffer A
    (type-text " more")
    (Thread/sleep 50)

    ;; Verify appended correctly
    (let [text-final (get-editor-text)]
      (is (.contains text-final "content-a more")
          (str "Should have 'content-a more', got: " text-final))
      (is (not (.contains text-final " morecontent-a"))
          "Text should NOT be in wrong order"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.buffer-identity-test))
