(ns lexicon.test.signals
  "Test signals for event-driven test synchronization (Issue #74).

  Core Principle: Tests should wait on editor STATE, not time.

  Instead of:
    await page.waitForTimeout(500);  // Hope this is long enough!

  Use:
    await page.waitForFunction(() => {
      const s = window.__LEXICON_SIGNALS;
      return s && s.idle === true;
    });

  This waits:
  - Exactly as long as needed
  - No longer
  - Deterministically

  Architecture:
  - Signals atom tracks semantic editor states
  - Exposed to browser via window.__LEXICON_SIGNALS
  - Tests poll signals instead of sleeping
  - :idle signal is most important (no pending commands/renders)

  Expected impact:
  - 9 minutes → ~90-120 seconds test runtime
  - Flaky failures → meaningful failures
  - Debug time dramatically reduced")

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
  - :idle - No pending commands, renders, or async operations

  Usage:
  (signal! :wasm-loaded)
  (signal! :idle)"
  [signal-key]
  (swap! signals assoc signal-key true)
  nil)

(defn clear-signal!
  "Clear a specific signal.

  Usage:
  (clear-signal! :idle)"
  [signal-key]
  (swap! signals dissoc signal-key)
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
  Clears :idle signal."
  []
  (swap! pending-commands inc)
  (clear-signal! :idle)
  nil)

(defn command-finished!
  "Mark that a command has finished executing.
  If no commands remain pending, signal :idle."
  []
  (let [remaining (swap! pending-commands dec)]
    (when (<= remaining 0)
      (reset! pending-commands 0)  ; Prevent negative counts
      (signal! :idle)))
  nil)

;; -- Browser Exposure --

;; Expose signals to browser for test access
;; Only in dev/test builds (goog.DEBUG is true in development)
(when ^boolean js/goog.DEBUG
  (set! (.-__LEXICON_SIGNALS js/window) signals))

;; -- Initialization --

;; Start with idle signal (before any commands execute)
(signal! :idle)
