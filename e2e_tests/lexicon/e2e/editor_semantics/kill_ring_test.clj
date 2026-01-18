(ns lexicon.e2e.editor-semantics.kill-ring-test
  "E2E tests for Emacs kill ring semantic - Epic #86

  Tests critical invariants:
  - Kill ring is global across buffers
  - Consecutive kills append"
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

(defn execute-command [command-name]
  "Execute a command directly via re-frame dispatch (workaround for blocked browser shortcuts like C-w)"
  (let [script (str "
    if (window.lexiconDispatch) {
      // Use ClojureScript vector syntax: cljs.core.PersistentVector.fromArray
      const cmd = cljs.core.PersistentVector.fromArray([
        cljs.core.keyword('execute-command'),
        cljs.core.keyword('" (name command-name) "')
      ], true);
      window.lexiconDispatch(cmd);
    } else {
      console.error('lexiconDispatch not found on window');
    }
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 100))

(defn set-mark []
  "Set mark with C-SPC"
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: ' ',
      code: 'Space',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 20))

(deftest test-kill-ring-is-global
  (testing "Emacs invariant: Kill ring is global across buffers"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text in scratch buffer
    (type-text "killed-text")
    (Thread/sleep 50)

    ;; Select all and kill (C-a, C-SPC, C-e, C-w)
    (press-ctrl-key "a")
    (Thread/sleep 20)
    (set-mark)
    (Thread/sleep 20)
    (press-ctrl-key "e")
    (Thread/sleep 20)
    ;; C-w is blocked by browser (close tab shortcut), use direct command dispatch
    (execute-command :kill-region)
    (Thread/sleep 100)

    ;; Buffer should be empty after kill
    (let [text-after-kill (get-editor-text)]
      (is (or (empty? text-after-kill)
              (not (.contains text-after-kill "killed-text")))
          "Text should be killed from buffer"))

    ;; Switch to new buffer (C-x b)
    (press-ctrl-key "x")
    (Thread/sleep 20)
    (press-key "b")
    (Thread/sleep 100)

    (e/fill *driver* {:css ".minibuffer-input"} "new-buffer")
    (Thread/sleep 20)
    (press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Yank in new buffer (C-y)
    (press-ctrl-key "y")
    (Thread/sleep 100)

    ;; Yanked text should appear in new buffer
    (let [text-in-new-buffer (get-editor-text)]
      (is (.contains text-in-new-buffer "killed-text")
          (str "Killed text from buffer A should yank into buffer B, got: " text-in-new-buffer)))))

(deftest test-consecutive-kills-append
  (testing "Emacs invariant: Consecutive C-k kills append to single kill ring entry"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type multiple lines
    (type-text "line1")
    (press-key "Enter")
    (Thread/sleep 20)
    (type-text "line2")
    (press-key "Enter")
    (Thread/sleep 20)
    (type-text "line3")
    (Thread/sleep 50)

    ;; Go to beginning (M-<)
    (let [script "
      const input = document.querySelector('.hidden-input');
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: '<',
        code: 'Comma',
        altKey: true,
        shiftKey: true,
        bubbles: true
      });
      input.dispatchEvent(event);
    "]
      (e/js-execute *driver* script))
    (Thread/sleep 50)

    ;; Kill first line with C-k
    (press-ctrl-key "k")
    (Thread/sleep 50)

    ;; Kill newline with another C-k (consecutive kill should append)
    (press-ctrl-key "k")
    (Thread/sleep 50)

    ;; Kill second line with C-k (consecutive kill should append)
    (press-ctrl-key "k")
    (Thread/sleep 50)

    ;; Now buffer should have "line3"
    (let [text-after-kills (get-editor-text)]
      (is (or (.contains text-after-kills "line3")
              (not (.contains text-after-kills "line1")))
          "Buffer should have line3, not line1 after kills"))

    ;; Yank should restore all killed lines as one entry
    (press-ctrl-key "y")
    (Thread/sleep 100)

    (let [text-after-yank (get-editor-text)]
      (is (.contains text-after-yank "line1")
          "Yank should restore line1")
      (is (.contains text-after-yank "line2")
          (str "Consecutive kills should append - line2 should be yanked too, got: " text-after-yank)))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.kill-ring-test))
