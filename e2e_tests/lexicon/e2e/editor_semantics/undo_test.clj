(ns lexicon.e2e.editor-semantics.undo-test
  "E2E tests for Emacs undo semantic - Epic #86

  Tests critical invariant:
  - Undo history is buffer-local"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-undo-is-buffer-local
  (testing "Emacs invariant: Undo history belongs to a buffer, not the editor"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type in scratch buffer
    (h/type-text "x")
    (Thread/sleep 50)

    ;; Verify text is there
    (let [text-a (h/get-buffer-text*)]
      (is (.contains text-a "x")
          "Scratch buffer should contain 'x'"))

    ;; Switch to new buffer (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "buffer-b")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in buffer B
    (h/type-text "y")
    (Thread/sleep 50)

    (let [text-b (h/get-buffer-text*)]
      (is (.contains text-b "y")
          "Buffer B should contain 'y'"))

    ;; Undo in buffer B (C-/)
    (h/press-ctrl "/")
    (Thread/sleep 100)

    ;; Buffer B should be empty after undo
    (let [text-b-after-undo (h/get-buffer-text*)]
      (is (not (.contains text-b-after-undo "y"))
          "Buffer B should not contain 'y' after undo"))

    ;; Switch back to scratch (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "*scratch*")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Scratch buffer should still have 'x' - undo in B didn't affect A
    (let [text-a-final (h/get-buffer-text*)]
      (is (.contains text-a-final "x")
          "Buffer A should still contain 'x' - undo in B should not affect A"))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.undo-test))
