(ns lexicon.semantic.read-only-test
  "Emacs semantic compatibility tests for read-only buffers.

  Tests from Epic #86, Issue #90 - Interaction Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:high read-only-buffer-prevents-modification
  (testing "Emacs invariant: Read-only buffers reject edits but allow navigation"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "help")]
      (h/set-read-only buf true)
      (h/insert-text buf "x")
      (is (= "" (h/buffer-text buf)) "Read-only buffer should reject insertions"))))
