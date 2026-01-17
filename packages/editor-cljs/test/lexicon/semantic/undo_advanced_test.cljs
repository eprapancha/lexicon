(ns lexicon.semantic.undo-advanced-test
  "Advanced undo system tests from Epic #86.

  Tests undo boundaries, redo, and point restoration."
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

;; =============================================================================
;; Undo Boundary Tests
;; =============================================================================

(deftest ^:critical commands-create-undo-boundaries
  (testing "Emacs invariant: Undo groups operations by command invocation"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")]
      ;; Simulate a single command that does multiple inserts
      ;; In real implementation, command boundaries create undo boundaries
      (h/insert-text buf "a")
      (h/insert-text buf "b")
      ;; One undo should remove both "a" and "b" if they're in same boundary
      (h/undo buf)
      ;; For now, our undo removes one operation at a time
      ;; This test will fail until we implement undo boundaries
      (is (= "" (h/buffer-text buf))
          "Single undo should remove all operations within command boundary"))))

(deftest ^:critical undo-boundary-can-be-manually-inserted
  (testing "Emacs invariant: Code can explicitly insert undo boundaries"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")]
      (h/insert-text buf "a")
      ;; TODO: Add helper for manual undo boundary insertion
      ;; (h/undo-boundary buf)
      (h/insert-text buf "b")
      ;; After manual boundary, undo should only remove "b"
      (h/undo buf)
      ;; This will fail until undo-boundary is implemented
      (is (= "a" (h/buffer-text buf))
          "Undo should stop at manual boundary"))))

;; =============================================================================
;; Undo Integration Tests
;; =============================================================================

(deftest ^:high undo-restores-point-and-mark
  (testing "Emacs invariant: Undo restores cursor and region state"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")]
      (h/insert-text buf "hello")
      (h/set-point buf 2)
      (let [point-before (h/point buf)]
        (h/insert-text buf " world")
        ;; Point should move after insert
        (is (not= point-before (h/point buf))
            "Point should change after insert")
        ;; Undo should restore point
        (h/undo buf)
        ;; This may fail if undo doesn't restore point
        (is (= point-before (h/point buf))
            "Undo should restore point to pre-operation position")))))

(deftest ^:high undo-is-not-destructive
  (testing "Emacs invariant: Undo preserves redo capability"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test")]
      (h/insert-text buf "x")
      (is (= "x" (h/buffer-text buf)))
      ;; Undo the insert
      (h/undo buf)
      (is (= "" (h/buffer-text buf))
          "Text should be undone")
      ;; TODO: Add redo helper
      ;; (h/redo buf)
      ;; (is (= "x" (h/buffer-text buf))
      ;;     "Redo should restore undone operation")
      ;; For now, just verify undo worked
      (is (= "" (h/buffer-text buf))
          "Undo should work (redo not yet implemented)"))))
