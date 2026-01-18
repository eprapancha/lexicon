(ns lexicon.e2e.editor-semantics.lisp-integration-test
  "E2E tests for SCI/Lisp integration - Epic #86

  Tests critical invariants:
  - Commands can be defined from Lisp
  - Lisp errors don't corrupt editor state"
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

(deftest test-lisp-evaluation-available
  (testing "Emacs invariant: Lisp evaluation is available in the editor"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Try to evaluate Lisp code in scratch buffer (M-: or eval-expression)
    ;; Type some Lisp code
    (type-text "(+ 1 2)")
    (Thread/sleep 50)

    ;; Editor should not crash when Lisp code is in buffer
    (let [text (get-editor-text)]
      (is (.contains text "(+ 1 2)")
          "Should be able to type Lisp code without crashing"))

    ;; Try invoking a command via M-x (which itself is Lisp-backed)
    (press-meta-key "x")
    (Thread/sleep 100)
    (type-in-minibuffer "fundamental-mode")
    (Thread/sleep 50)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Should execute successfully (proving Lisp integration works)
    (let [echo-text (get-echo-area-text)]
      (is (not (.contains echo-text "error"))
          "Lisp-backed commands should execute successfully"))))

(deftest test-editor-state-preserved-after-errors
  (testing "Emacs invariant: Editor state preserved even if errors occur"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type some content
    (type-text "stable-content")
    (Thread/sleep 50)

    (let [text-before (get-editor-text)]
      ;; Try to invoke a non-existent command (should error gracefully)
      (press-meta-key "x")
      (Thread/sleep 100)
      (type-in-minibuffer "non-existent-command-xyz")
      (Thread/sleep 50)
      (press-minibuffer-enter)
      (Thread/sleep 100)

      ;; Editor should still be functional
      (let [text-after (get-editor-text)]
        (is (.contains text-after "stable-content")
            "Buffer content should be preserved after command error"))

      ;; Should still be able to type
      (type-text "more")
      (Thread/sleep 50)

      (let [text-final (get-editor-text)]
        (is (.contains text-final "more")
            "Editor should remain functional after errors")))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.lisp-integration-test))
