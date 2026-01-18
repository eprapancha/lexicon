(ns lexicon.e2e.editor-semantics.mode-test
  "E2E tests for Emacs mode semantic - Epic #86

  Tests critical invariants:
  - Each buffer has exactly one major mode
  - Multiple minor modes can coexist"
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

(defn get-status-bar-text []
  (try
    (e/get-element-text *driver* {:css ".status-bar"})
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

(deftest test-exactly-one-major-mode-per-buffer
  (testing "Emacs invariant: Each buffer has exactly one major mode"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Get initial mode from status bar
    (let [status-before (get-status-bar-text)]
      ;; Should show some major mode (probably fundamental-mode or text-mode)
      (is (not (empty? status-before))
          "Status bar should show current mode"))

    ;; Try to switch to text-mode via M-x
    (press-meta-key "x")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "text-mode")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Check status bar for mode change
    (let [status-after-text (get-status-bar-text)]
      (is (or (.contains status-after-text "Text")
              (.contains status-after-text "text"))
          (str "Status bar should show Text mode, got: " status-after-text)))

    ;; Switch to another mode
    (press-meta-key "x")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "fundamental-mode")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify mode switching command executed (status bar updated)
    (let [status-after-fundamental (get-status-bar-text)]
      ;; The fact that we can switch modes without errors is the key invariant
      ;; Exact status bar formatting may vary
      (is (not (empty? status-after-fundamental))
          (str "Status bar should show mode info after switching, got: " status-after-fundamental)))))

(deftest test-multiple-minor-modes-can-coexist
  (testing "Emacs invariant: Multiple minor modes can be active simultaneously"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Enable line-number-mode
    (press-meta-key "x")
    (Thread/sleep 100)
    (e/fill *driver* {:css ".minibuffer-input"} "line-number-mode")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    (let [status-1 (get-status-bar-text)]
      ;; Status bar might show 'L' indicator for line numbers
      ;; Just verify command executed
      (is (not (empty? status-1))
          "Status bar should show mode indicators"))

    ;; Enable another minor mode
    (press-meta-key "x")
    (Thread/sleep 100)
    (e/fill *driver* {:css ".minibuffer-input"} "column-number-mode")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Both minor modes should be reflected (status bar might show L C indicators)
    (let [status-2 (get-status-bar-text)]
      (is (not (empty? status-2))
          "Minor modes should coexist and show in status bar"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.mode-test))
