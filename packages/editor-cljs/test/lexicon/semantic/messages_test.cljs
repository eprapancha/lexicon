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
