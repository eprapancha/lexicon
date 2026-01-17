(ns lexicon.semantic.window-test
  "Emacs semantic compatibility tests for window system.

  Tests from Epic #86, Issue #90 - Interaction Semantics"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical buffer-identity-is-stable-across-views
  (testing "Emacs invariant: Buffer has stable identity independent of how many windows display it"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "test" "initial")]
      (h/show-buffer-in-two-windows buf)
      (h/insert-text buf " more")
      (is (= "initial more" (h/window-text 1)) "Window 1 should show updated buffer")
      (is (= "initial more" (h/window-text 2)) "Window 2 should show same buffer - mutations visible through all views"))))

(deftest ^:critical window-deletion-preserves-buffer-identity
  (testing "Emacs invariant: Deleting a window never kills the buffer by default"
    (h/reset-editor-db!)
    (let [buf (h/create-buffer "x" "content")]
      (h/show-buffer-in-new-window buf)
      (h/delete-other-windows)
      (is (h/buffer-exists? "x") "Buffer should still exist after deleting its window"))))
