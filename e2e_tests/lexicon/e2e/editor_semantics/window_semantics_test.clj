(ns lexicon.e2e.editor-semantics.window-semantics-test
  "E2E tests for Emacs window semantic - Epic #86

  Tests critical invariant: Buffer identity stable across multiple window views"
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

(deftest test-buffer-identity-stable-across-window-views
  (testing "Emacs invariant: Buffer identity stable when displayed in multiple windows"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type initial text
    (type-text "initial")
    (Thread/sleep 50)

    ;; Split window (C-x 2)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "2")
    (Thread/sleep 100)

    ;; Verify we have 2 windows
    (let [windows (e/query-all *driver* {:css ".window-pane"})]
      (is (>= (count windows) 2) "Should have at least 2 windows after split"))

    ;; Type more text - should appear in both windows
    (type-text " more")
    (Thread/sleep 100)

    ;; Verify text appears (both windows show same buffer)
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "initial more")
          (str "Buffer should show 'initial more', got: " editor-text)))))

(deftest test-window-deletion-preserves-buffer
  (testing "Emacs invariant: Deleting window doesn't kill the buffer"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type some content
    (type-text "content")
    (Thread/sleep 50)

    ;; Split window
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "2")
    (Thread/sleep 100)

    ;; Delete other windows (C-x 1)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "1")
    (Thread/sleep 100)

    ;; Verify buffer content still exists
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "content")
          (str "Buffer content should be preserved after window deletion, got: " editor-text)))

    ;; Verify we're back to 1 window
    (let [windows (e/query-all *driver* {:css ".window-pane"})]
      (is (= 1 (count windows))
          (str "Should have 1 window after C-x 1, got: " (count windows))))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.window-semantics-test))
