(ns lexicon.semantic.minibuffer-test
  "Emacs semantic compatibility tests for minibuffer system.

  Tests from Epic #86, Issue #92 - VERTICO GATE"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical minibuffer-is-a-buffer
  (testing "Emacs invariant: The minibuffer is a real buffer, not a special widget"
    (h/reset-editor-db!)
    ;; Activate minibuffer
    (let [mb-buf-id (h/activate-minibuffer "Test prompt: ")]
      ;; The minibuffer should be a real buffer
      (is (some? mb-buf-id) "Minibuffer should have a buffer ID")
      (is (h/buffer-exists? " *Minibuf-0*") "Minibuffer buffer should exist with standard name")

      ;; Should be able to insert text into minibuffer buffer
      (h/minibuffer-insert "hello")
      (is (= "hello" (h/buffer-text mb-buf-id)) "Minibuffer should contain inserted text")

      ;; Should be able to get minibuffer contents
      (is (= "hello" (h/minibuffer-contents)) "Minibuffer contents should be accessible")

      ;; Deactivating should preserve the buffer (in Emacs, minibuffer buffers persist)
      (h/deactivate-minibuffer)
      (is (h/buffer-exists? " *Minibuf-0*") "Minibuffer buffer should persist after deactivation"))))
