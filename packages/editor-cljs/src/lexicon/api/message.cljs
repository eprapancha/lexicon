(ns lexicon.api.message
  "Emacs-compatible message API (Issue #47, #73).

  Provides (message) function that:
  - Uses log bus for lifecycle-independent logging
  - Appends to *Messages* buffer with timestamp (via log bus)
  - Flashes in minibuffer for 2 seconds
  - Returns the message string"
  (:require [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.log :as log]))

(defn message
  "Display a message in the minibuffer and append to *Messages* buffer.

  This is the Emacs-compatible message API. Usage:
    (message \"Hello, world!\")
    (message \"Processing %s items\" item-count)

  Implementation (Issue #73):
  - Uses log bus for lifecycle-independent logging
  - Log bus handles Messages buffer attachment and replay
  - Always succeeds, even before Messages buffer exists

  Args:
    format-str - Message string (format string support TODO)
    & args     - Format arguments (not yet implemented)

  Returns:
    The formatted message string"
  [format-str & args]
  (let [;; TODO: Implement proper format string support like Emacs
        ;; For now, just concatenate args as strings
        msg (if (seq args)
              (str format-str " " (clojure.string/join " " args))
              format-str)]
    ;; Log to log bus (will replay to Messages buffer when it's ready)
    (log/info msg)

    ;; Flash in minibuffer (separate from logging)
    ;; Directly update db to avoid dispatch-sync issues when called from event handlers
    (swap! rfdb/app-db assoc-in [:minibuffer :message] msg)
    msg))

(comment
  ;; Usage examples:
  (message "File saved")
  (message "Loaded" 42 "packages")
  (message "Error: Could not open file"))
