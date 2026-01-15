(ns lexicon.test-helpers
  "Shared E2E test helpers for Lexicon tests.

  Provides utilities for:
  - Auto-printing *Messages* buffer on test failure (Issue #84)
  - Common browser interactions
  - State inspection"
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]
            [etaoin.api :as e]))

;; =============================================================================
;; Message Buffer Helpers (Issue #84)
;; =============================================================================

(defn get-messages-buffer
  "Fetch *Messages* buffer contents from window.editorState.

  Returns the text from buffer 2 (*Messages*), which contains all logged messages."
  [driver]
  (try
    (e/js-execute driver "
      const state = window.editorState;
      return state && state.messagesBuffer ? state.messagesBuffer : 'No messages logged yet';
    ")
    (catch Exception e
      (str "Failed to fetch *Messages* buffer: " (.getMessage e)))))

(defn print-messages-buffer
  "Print the *Messages* buffer contents to test output.

  Useful for debugging failed tests - shows all logged messages."
  [driver]
  (when-let [messages (get-messages-buffer driver)]
    (println "\n" (str/join "" (repeat 80 "=")) "\n")
    (println "*Messages* buffer contents:")
    (println (str/join "" (repeat 80 "-")))
    (println messages)
    (println (str/join "" (repeat 80 "=")) "\n")))

;; =============================================================================
;; Enhanced Assertions (Issue #84)
;; =============================================================================

(defmacro is-with-messages
  "Enhanced assertion that prints *Messages* buffer on failure.

  Usage:
    (is-with-messages driver (= expected actual) \"assertion message\")

  If the assertion fails, automatically prints the *Messages* buffer
  to help with debugging."
  [driver form & [msg]]
  `(let [result# ~form]
     (when-not result#
       (println "\n⚠️  Assertion failed, printing *Messages* buffer:")
       (print-messages-buffer ~driver))
     (is result# ~msg)))

;; =============================================================================
;; Automatic Test Failure Reporting
;; =============================================================================

;; Track whether we've seen a failure in this test run
(def ^:dynamic *test-failed?* false)

(defn report-with-messages
  "Custom test report function that prints *Messages* buffer on failures.

  Wraps clojure.test's default :fail and :error reporting to automatically
  dump the *Messages* buffer when tests fail."
  [driver original-report]
  (fn [m]
    ;; Call the original reporter first
    (original-report m)

    ;; Track failures and print messages
    (case (:type m)
      :fail
      (do
        (set! *test-failed?* true)
        (println "\n⚠️  Test failed - automatically printing *Messages* buffer:")
        (print-messages-buffer driver))

      :error
      (do
        (set! *test-failed?* true)
        (println "\n⚠️  Test error - automatically printing *Messages* buffer:")
        (print-messages-buffer driver))

      ;; For other event types, do nothing extra
      nil)))

;; =============================================================================
;; Common Test Fixtures
;; =============================================================================

(defn start-driver
  "Start a headless Firefox driver"
  []
  (e/firefox {:headless true}))

(defn stop-driver
  "Stop the driver and clean up"
  [driver]
  (when driver
    (e/quit driver)))

(defn with-driver-and-messages
  "Standard E2E test fixture that:
   - Starts/stops Firefox driver
   - Binds *driver* dynamic var
   - Sets up automatic *Messages* buffer printing on test failure

   Usage in test file:
     (def ^:dynamic *driver* nil)
     (use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))"
  [driver-var f]
  (let [driver (start-driver)
        ;; Save the original reporter before we replace it
        original-report (deref #'clojure.test/report)]
    (try
      ;; Use push-thread-bindings since we're binding a var passed as parameter
      (push-thread-bindings {driver-var driver
                             #'*test-failed?* false
                             #'clojure.test/report (report-with-messages driver original-report)})
      (f)
      (finally
        (pop-thread-bindings)
        (stop-driver driver)))))

;; =============================================================================
;; Common Test Helpers
;; =============================================================================

(defn get-buffer-text
  "Get complete buffer text via window.editorState"
  [driver]
  (e/js-execute driver "
    const state = window.editorState;
    return state ? state.buffer : '';
  "))

(defn get-point
  "Get current point position"
  [driver]
  (e/js-execute driver "
    const state = window.editorState;
    return state ? state.point : 0;
  "))

(defn wait-for-editor-ready
  "Wait for editor to be ready (with configurable timeout)"
  ([driver] (wait-for-editor-ready driver 10))
  ([driver timeout-secs]
   (e/wait-visible driver {:css ".editor-wrapper"} {:timeout timeout-secs})))

(defn click-editor
  "Click the editor to focus it"
  [driver]
  (e/click driver {:css ".editor-wrapper"}))

(defn setup-test
  "Standard test setup: navigate to app and wait for editor"
  [driver app-url]
  (e/go driver app-url)
  (wait-for-editor-ready driver)
  (click-editor driver)
  (Thread/sleep 200))
