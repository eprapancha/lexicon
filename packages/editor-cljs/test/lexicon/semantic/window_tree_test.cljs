(ns lexicon.semantic.window-tree-test
  "Emacs semantic compatibility tests for window tree.

  Tests from Epic #86, Issue #90 - Interaction Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]
            [re-frame.db :as rfdb]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical window-tree-is-a-binary-partition
  (testing "Emacs invariant: Windows form a recursive tree of splits"
    (h/reset-editor-db!)
    ;; Initially: single window (leaf)
    (let [tree (get-in @rfdb/app-db [:window-tree])]
      (is (= :leaf (:type tree)) "Initial window should be a leaf"))

    ;; Split once: creates binary split
    (h/split-window-horizontally)
    (let [tree (get-in @rfdb/app-db [:window-tree])]
      (is (= :split (:type tree)) "After split, root should be a split node")
      (is (some? (:first tree)) "Split should have first child")
      (is (some? (:second tree)) "Split should have second child")
      (is (= :leaf (:type (:first tree))) "First child should be a leaf")
      (is (= :leaf (:type (:second tree))) "Second child should be a leaf"))

    ;; Split again: creates nested binary tree
    (h/split-window-horizontally)
    (let [tree (get-in @rfdb/app-db [:window-tree])]
      (is (= :split (:type tree)) "Root should still be a split")
      ;; One of the children should now be a split (nested)
      (let [children [(:first tree) (:second tree)]
            split-child (first (filter #(= :split (:type %)) children))]
        (is (some? split-child) "Should have a nested split")))))
