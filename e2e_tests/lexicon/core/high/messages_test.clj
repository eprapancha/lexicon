(ns lexicon.core.high.messages-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high messages-are-buffer-backed-editor-state
  "Emacs invariant:
   Messages are editor-visible state, stored in a buffer (*Messages*).

   Required for:
   - debugging
   - user feedback
   - trust

   Status: ACTIVE"
  (h/with-editor
    (h/message \"hello\")
    (is (h/buffer-exists? \"*Messages*\"))
    (is (re-find #"hello" (h/buffer-text \"*Messages*\")))))

(deftest ^:high message-appends-to-history-but-updates-echo-area
  "Emacs invariant:
   The echo area shows the last message, but the messages buffer
   retains history.

   Status: PENDING"
  (is false))
