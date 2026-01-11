(ns lexicon.api.message
  "Emacs-compatible message API (Issue #47).

  Provides (message) function that:
  - Appends to *Messages* buffer with timestamp
  - Flashes in minibuffer for 2 seconds
  - Returns the message string"
  (:require [re-frame.core :as rf]))

(defn message
  "Display a message in the minibuffer and append to *Messages* buffer.

  This is the Emacs-compatible message API. Usage:
    (message \"Hello, world!\")
    (message \"Processing %s items\" item-count)

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
    ;; Dispatch event to append to *Messages* and flash in minibuffer
    (rf/dispatch [:message/display msg])
    msg))

(comment
  ;; Usage examples:
  (message "File saved")
  (message "Loaded" 42 "packages")
  (message "Error: Could not open file"))
