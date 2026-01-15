(ns lexicon.test-helpers
  "Shared E2E test helpers for Etaoin-based tests.

  Provides utilities for fast test mode (reuse browser) vs full mode (reload per test)."
  (:require [etaoin.api :as e]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def app-url "http://localhost:8080")
(def test-timeout 10000)

(def ^:dynamic *driver* nil)
(def ^:dynamic *fast-mode*
  "When true, reuse browser session between tests.
   When false, reload page for each test (full isolation)."
  (Boolean/parseBoolean (System/getProperty "test.fast" "true")))

;; =============================================================================
;; Browser Lifecycle
;; =============================================================================

(defn start-driver
  "Start headless Firefox browser."
  []
  (e/firefox {:headless true}))

(defn stop-driver
  "Stop browser and clean up."
  [driver]
  (when driver
    (e/quit driver)))

(defn wait-for-editor-ready
  "Wait for editor to be visible and ready."
  []
  (e/wait-visible *driver* {:css ".editor-wrapper"} {:timeout (/ test-timeout 1000)}))

(defn with-driver
  "Fixture to start browser once for all tests.
   In fast mode, also loads page once."
  [f]
  (let [driver (start-driver)]
    (try
      (binding [*driver* driver]
        ;; In fast mode, load page once here
        (when *fast-mode*
          (e/go *driver* app-url)
          (wait-for-editor-ready))
        (f))
      (finally
        (stop-driver driver)))))

;; =============================================================================
;; Editor Interaction
;; =============================================================================

(defn click-editor
  "Click the editor to focus it."
  []
  (e/click *driver* {:css ".editor-wrapper"}))

(defn reset-buffer!
  "Clear buffer content without reloading page.
   Fast mode: Use this between tests for speed.
   Full mode: Not needed (page reloads)."
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    const wasm = state.wasmInstance;
    if (wasm) {
      const len = wasm.length();
      if (len > 0) {
        wasm.delete(0, len);
      }
    }
    window.resetBufferForTests();
  ")
  (Thread/sleep 100))

(defn setup-test!
  "Setup for each test. Behavior depends on mode:
   - Fast mode: Just reset buffer (fast!)
   - Full mode: Reload page (full isolation)"
  []
  (if *fast-mode*
    (do
      ;; Fast mode: Just clear buffer
      (reset-buffer!)
      (click-editor))
    (do
      ;; Full mode: Reload page for isolation
      (e/go *driver* app-url)
      (wait-for-editor-ready)
      (click-editor)))
  (Thread/sleep 200))

;; =============================================================================
;; Input Helpers
;; =============================================================================

(defn type-text
  "Type text by sending it character by character."
  [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

(defn press-enter
  "Press the Enter key (triggers :newline command)."
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'Enter',
      code: 'Enter',
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-ctrl-o
  "Press C-o (triggers :open-line command)."
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'o',
      code: 'KeyO',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-meta-caret
  "Press M-^ (triggers :delete-indentation command)."
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '^',
      code: 'Digit6',
      shiftKey: true,
      altKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-ctrl-u
  "Press C-u (sets prefix argument)."
  []
  (let [script "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: 'u',
      code: 'KeyU',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  "]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-digit
  "Press a digit key."
  [digit]
  (e/fill *driver* {:css ".hidden-input"} (str digit))
  (Thread/sleep 10))

;; =============================================================================
;; State Inspection
;; =============================================================================

(defn get-buffer-text
  "Get complete buffer text via window.editorState."
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    return state ? state.buffer : '';
  "))

(defn get-point
  "Get current point position."
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    return state ? state.point : 0;
  "))
