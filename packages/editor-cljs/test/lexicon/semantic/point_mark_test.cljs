(ns lexicon.semantic.point-mark-test
  "Emacs semantic compatibility tests for point/mark semantics.

  Tests from Epic #86, Issue #89 - Point & Mark Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical point-is-buffer-local
  (testing "Emacs invariant: Point belongs to buffer, not window"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "a" "hello world")]
      (h/set-point buf 5)
      (is (= 5 (h/point buf)) "Point should be set to position 5"))))
