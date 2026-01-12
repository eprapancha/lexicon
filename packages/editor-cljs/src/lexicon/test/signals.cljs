(ns lexicon.test.signals
  "Signals for editor lifecycle observability (Issue #74).

  Purpose:
  - Expose semantic milestones (WASM loaded, buffers ready, etc.)
  - Enable debugging, logging, profiling
  - Provide hooks for tooling

  NOT for:
  - Detecting global 'idle' state (too complex, couples to internals)
  - Test synchronization (tests should wait on observable conditions)

  Architecture:
  - Signals track discrete lifecycle events
  - Exposed via window.__LEXICON_SIGNALS for introspection
  - Similar to Emacs hooks (observability, not orchestration)

  Test strategy (revised):
  Instead of waiting for 'idle', tests wait for specific observable conditions:
    (wait-for #(= expected-text (get-editor-text)))
    (wait-for #(cursor-at? 5 10))

  This is faster, deterministic, and decoupled from implementation.")

;; -- Signal State --

;; Map of signal keywords to boolean values
;; {:wasm-loaded true, :buffers-ready true, :ui-ready true, :idle true}
(defonce signals (atom {}))

;; -- Pending Command Tracking --

;; Counter for commands currently executing
;; When this reaches 0, we signal :idle
(defonce pending-commands (atom 0))

;; -- Signal API --

(defn signal!
  "Emit a signal indicating a semantic milestone has been reached.

  Common signals:
  - :wasm-loaded - WASM module loaded and initialized
  - :buffers-ready - Buffers created and ready
  - :ui-ready - UI rendered and interactive
  - :command-idle - No commands currently executing (NOT global idle!)

  Usage:
  (signal! :wasm-loaded)
  (signal! :command-idle)"
  [signal-key]
  (swap! signals assoc signal-key true)
  ;; Also update the plain JS object for browser access
  (when ^boolean js/goog.DEBUG
    (when-let [js-signals (.-__LEXICON_SIGNALS js/window)]
      (aset js-signals (name signal-key) true)))
  nil)

(defn clear-signal!
  "Clear a specific signal.

  Usage:
  (clear-signal! :idle)"
  [signal-key]
  (swap! signals dissoc signal-key)
  ;; Also update the plain JS object for browser access
  (when ^boolean js/goog.DEBUG
    (when-let [js-signals (.-__LEXICON_SIGNALS js/window)]
      (js-delete js-signals (name signal-key))))
  nil)

(defn reset-signals!
  "Clear all signals (typically used between tests).

  Usage:
  (reset-signals!)"
  []
  (reset! signals {})
  (reset! pending-commands 0)
  nil)

(defn get-signals
  "Get current signal state (for inspection/debugging).

  Returns:
  {:wasm-loaded true, :idle true, ...}"
  []
  @signals)

;; -- Pending Command Tracking --

(defn command-started!
  "Mark that a command has started executing.
  Clears :command-idle signal."
  []
  (swap! pending-commands inc)
  (clear-signal! :command-idle)
  nil)

(defn command-finished!
  "Mark that a command has finished executing.
  If no commands remain pending, signal :command-idle."
  []
  (let [remaining (swap! pending-commands dec)]
    (when (<= remaining 0)
      (reset! pending-commands 0)  ; Prevent negative counts
      (signal! :command-idle)))
  nil)

;; -- Browser Exposure --

;; Expose signals to browser for test access as a plain JavaScript object
;; Only in dev/test builds (goog.DEBUG is true in development)
(when ^boolean js/goog.DEBUG
  ;; Create a plain JS object that's easy to access from tests
  (set! (.-__LEXICON_SIGNALS js/window) #js {})
  ;; Initialize with current signal state
  (doseq [[k v] @signals]
    (aset (.-__LEXICON_SIGNALS js/window) (name k) v)))

;; -- Initialization --

;; Start with command-idle signal (before any commands execute)
(signal! :command-idle)
