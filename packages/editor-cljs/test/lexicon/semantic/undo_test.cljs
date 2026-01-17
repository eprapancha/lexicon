(ns lexicon.semantic.undo-test
  "Emacs semantic compatibility tests for undo system.

  Tests from Epic #86, Issue #91 - History & State Preservation"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical undo-is-buffer-local
  (testing "Emacs invariant: Undo history belongs to a buffer, not the editor"
    (h/reset-editor-db!)
    (let [a (h/create-buffer "a")
          b (h/create-buffer "b")]
      (h/insert-text a "x")
      (h/insert-text b "y")
      (h/undo a)
      (is (= "" (h/buffer-text a)) "Undo in buffer a should clear its text")
      (is (= "y" (h/buffer-text b)) "Buffer b should be unaffected by undo in a"))))
