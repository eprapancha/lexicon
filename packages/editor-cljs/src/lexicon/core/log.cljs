(ns lexicon.core.log
  "Lifecycle-independent logging system using sink pattern.

  Core Principle: Logging must never care whether the Messages buffer exists.

  Architecture:
  - Log emission is always safe (never throws, never blocks)
  - Logs are recorded to history
  - Sinks attach/detach independently
  - Messages buffer is just another sink

  This matches Emacs semantics: `message` always succeeds regardless of editor state."
  (:require [clojure.string :as str]))

;; -- Global State (Minimal & Controlled) --

;; Append-only history of all log entries. Enables replay for late-attaching sinks.
(defonce ^:private log-history (atom []))

;; Set of sink functions. Each sink receives log entries: (fn [entry] ...)
;; Sinks are responsible for their own error handling.
(defonce ^:private sinks (atom #{}))

;; -- Core Logging API --

(defn- now
  "Get current timestamp in ISO format."
  []
  (.toISOString (js/Date.)))

(defn log!
  "Emit a log entry. Never throws, never blocks, always succeeds.

  Entry format:
  {:level   :info|:warn|:error|:debug
   :text    \"Log message\"
   :context {optional context map}
   :ts      \"2026-01-12T...\" (added automatically)}

  Usage:
  (log! {:text \"Buffer created\"})
  (log! {:level :error :text \"WASM load failed\" :context {:stage :bootstrap}})"
  [{:keys [level text context]
    :or   {level :info}}]
  (let [entry {:ts      (now)
               :level   level
               :text    text
               :context context}]
    ;; Always record to history
    (swap! log-history conj entry)

    ;; Best-effort delivery to sinks
    (doseq [sink @sinks]
      (try
        (sink entry)
        (catch :default _
          ;; Sink errors are silently ignored to prevent logging from breaking
          nil))))
  nil)

;; -- Convenience Helpers --

(defn info
  "Log info-level message."
  [text]
  (log! {:level :info :text text}))

(defn warn
  "Log warning-level message."
  [text]
  (log! {:level :warn :text text}))

(defn error
  "Log error-level message."
  ([text]
   (log! {:level :error :text text}))
  ([text context]
   (log! {:level :error :text text :context context})))

(defn debug
  "Log debug-level message."
  [text]
  (log! {:level :debug :text text}))

;; -- Sink Management --

(defn attach-sink!
  "Add a sink to receive log entries.
  Sink function signature: (fn [entry] ...)
  Returns nil."
  [sink-fn]
  (swap! sinks conj sink-fn)
  nil)

(defn detach-sink!
  "Remove a sink from receiving log entries.
  Returns nil."
  [sink-fn]
  (swap! sinks disj sink-fn)
  nil)

(defn get-history
  "Get all logged entries (for replay or inspection)."
  []
  @log-history)

(defn clear-history!
  "Clear log history (typically only used in tests).
  Returns nil."
  []
  (reset! log-history [])
  nil)

;; -- Built-in Sinks --

(defn console-sink
  "Sink that writes to browser console.
  Format: [timestamp] [LEVEL] message"
  [{:keys [level text ts]}]
  (js/console.log
    (str "[" ts "] "
         "[" (str/upper-case (name level)) "] "
         text)))

(defn- format-timestamp
  "Format timestamp for Messages buffer display.
  Converts ISO timestamp to HH:MM:SS format."
  [iso-ts]
  (let [date (js/Date. iso-ts)
        hours (.getHours date)
        minutes (.getMinutes date)
        seconds (.getSeconds date)
        pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
    (str (pad hours) ":" (pad minutes) ":" (pad seconds))))

(defn messages-buffer-sink
  "Create a sink that writes to Messages buffer via append-fn.

  append-fn: Function that appends text to buffer, e.g.:
  (fn [text] (rf/dispatch [:buffer/append-to-messages text]))

  Returns a sink function suitable for attach-sink!

  Usage:
  (attach-sink! (messages-buffer-sink append-fn))"
  [append-fn]
  (fn [{:keys [text ts]}]
    (let [timestamp (format-timestamp ts)
          formatted-msg (str "[" timestamp "] " text "\n")]
      (append-fn formatted-msg))))

;; -- Messages Buffer Integration --

(defn attach-messages-buffer!
  "Attach Messages buffer as a sink with history replay.

  This should be called once the *Messages* buffer is ready.
  It will:
  1. Replay all historical log entries to the buffer
  2. Attach sink for future entries

  append-fn: Function that appends text to Messages buffer

  Usage:
  (attach-messages-buffer!
    (fn [text] (rf/dispatch [:buffer/append-to-messages text])))"
  [append-fn]
  (let [sink (messages-buffer-sink append-fn)]
    ;; Replay all history to Messages buffer
    (doseq [entry @log-history]
      (sink entry))

    ;; Attach for future logs
    (attach-sink! sink)

    (info "Messages buffer attached to logging system"))
  nil)

;; -- Boot Initialization --

;; Attach console sink immediately at namespace load
;; This ensures logging works before WASM, buffers, or UI exist
(attach-sink! console-sink)
