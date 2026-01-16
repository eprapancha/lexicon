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

(deftest ^:critical point-is-independent-per-buffer
  (testing "Emacs invariant: Each buffer has its own independent point"
    (h/reset-editor-db!)
    (let [buf-a (h/create-buffer "a" "abc")
          buf-b (h/create-buffer "b" "xyz")]
      (h/set-point buf-a 2)
      (h/set-point buf-b 1)
      (is (= 2 (h/point buf-a)) "Buffer A point should be 2")
      (is (= 1 (h/point buf-b)) "Buffer B point should be 1")
      (h/set-point buf-a 0)
      (is (= 0 (h/point buf-a)) "Buffer A point changed to 0")
      (is (= 1 (h/point buf-b)) "Buffer B point unchanged"))))
