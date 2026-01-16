(ns lexicon.semantic.messages-test
  "Emacs semantic compatibility tests for message system.

  Tests from Epic #86, Issue #87 - Core Editor Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:high messages-are-buffer-backed-editor-state
  (testing "Emacs invariant: Messages are editor-visible state, stored in a buffer (*Messages*)"
    (h/reset-editor-db!)
    (h/message "hello")
    (is (h/buffer-exists? "*Messages*") "*Messages* buffer should exist")
    (is (re-find #"hello" (h/buffer-text "*Messages*")) "Message should appear in *Messages* buffer")))

(deftest ^:high message-appends-to-history-but-updates-echo-area
  (testing "Emacs invariant: The echo area shows the last message, but the messages buffer retains history"
    (h/reset-editor-db!)
    (h/message "first")
    (h/message "second")
    (let [messages-text (h/buffer-text "*Messages*")]
      (is (re-find #"first" messages-text) "First message should be in history")
      (is (re-find #"second" messages-text) "Second message should be in history")
      (is (= "second" (h/echo-area-text)) "Echo area should show last message"))))
