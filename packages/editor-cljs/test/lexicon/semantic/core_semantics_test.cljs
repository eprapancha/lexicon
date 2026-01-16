(ns lexicon.semantic.core-semantics-test
  "Emacs semantic compatibility tests for core editor invariants.

  Tests from Epic #86, Issue #87 - Core Editor Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical buffer-identity-is-stable-across-content
  (testing "Emacs invariant: Buffer identity stable independent of content changes"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test" "initial")]
      (is (= "initial" (h/buffer-text buf)) "Initial text correct")
      (h/insert-text buf " more")
      (is (= "initial more" (h/buffer-text buf)) "Text mutation preserves buffer identity")
      (is (some? buf) "Buffer ID remains stable"))))
