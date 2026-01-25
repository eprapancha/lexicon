(ns lexicon.e2e.editor-semantics.undo-semantics-test
  "E2E tests for Emacs undo semantic - Epic #86

  Tests critical invariant: Undo history is buffer-local"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-undo-is-buffer-local
  (testing "Emacs invariant: Undo history belongs to buffer, not editor"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type in scratch buffer
    (h/type-text "buffer-a")
    (Thread/sleep 50)

    ;; Verify initial text
    (let [text-a-before (h/get-buffer-text*)]
      (is (.contains text-a-before "buffer-a")
          "Buffer A should have 'buffer-a'"))

    ;; Switch to new buffer (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    ;; Type buffer name in minibuffer
    (h/type-in-minibuffer "buffer-b")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in buffer B
    (h/type-text "buffer-b")
    (Thread/sleep 50)

    ;; Verify buffer B has different content
    (let [text-b (h/get-buffer-text*)]
      (is (.contains text-b "buffer-b")
          "Buffer B should have 'buffer-b'"))

    ;; Undo in buffer B
    (h/press-ctrl "/")
    (Thread/sleep 100)

    ;; Verify buffer B is now empty or has less text
    (let [text-b-after (h/get-buffer-text*)]
      (is (or (empty? text-b-after)
              (not (.contains text-b-after "buffer-b")))
          (str "Buffer B should be empty or have less text after undo, got: " text-b-after)))

    ;; Switch back to buffer A (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "*scratch*")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify buffer A still has its content (undo in B didn't affect A)
    (let [text-a-after (h/get-buffer-text*)]
      (is (.contains text-a-after "buffer-a")
          (str "Buffer A should still have 'buffer-a' - undo in B shouldn't affect A, got: " text-a-after)))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.undo-semantics-test))
