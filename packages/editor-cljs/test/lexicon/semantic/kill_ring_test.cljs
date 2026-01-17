(ns lexicon.semantic.kill-ring-test
  "Emacs semantic compatibility tests for kill ring.

  Tests from Epic #86, Issue #91 - History & State Preservation"
  (:require [cljs.test :refer [deftest is testing use-fixtures]]
            [lexicon.semantic.helpers :as h])
  (:require-macros [lexicon.semantic.helpers-macros :refer [with-buffer]]))

;; Wait for WASM before running tests
(use-fixtures :once h/with-wasm)

(deftest ^:critical kill-ring-is-global
  (testing "Emacs invariant: The kill ring is global across buffers"
    (h/reset-editor-db!)
    (with-buffer "a"
      (h/insert-text (h/current-buffer) "x")
      (h/kill-region 0 1))
    (with-buffer "b"
      (h/yank)
      (is (seq (h/buffer-text "b")) "Yanked text should appear in buffer b")
      (is (= "x" (h/buffer-text "b")) "Should yank the text that was killed in buffer a"))))

(deftest ^:critical consecutive-kills-append
  (testing "Emacs invariant: Consecutive kills append to form single kill ring entry"
    (h/reset-editor-db!)
    (with-buffer "test"
      (h/insert-text (h/current-buffer) "abcdef")
      ;; Kill "ab" - leaves "cdef", point at 0
      (h/kill-region 0 2)
      ;; Kill "cd" consecutively - should append to get "abcd" in kill ring
      ;; Leaves "ef", point at 0
      (h/kill-region 0 2)
      ;; Yank at point 0 inserts "abcd" before "ef" -> "abcdef"
      (h/yank)
      (is (= "abcdef" (h/buffer-text "test"))
          "Consecutive kills should append and yank should restore them"))))
