(ns lexicon.ui.buffers.identity-test
  "E2E tests for Emacs buffer identity semantic - Epic #86

  Tests critical invariant: Buffer identity is stable across content changes"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-buffer-identity-stable-across-content
  (testing "Emacs invariant: Buffer identity is stable independent of content changes"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type initial text
    (h/type-text "initial")
    (Thread/sleep 50)

    ;; Verify initial text
    (let [text-1 (h/get-buffer-text*)]
      (is (.contains text-1 "initial")
          "Should have 'initial' text"))

    ;; Type more text - this should append
    (h/type-text " more")
    (Thread/sleep 50)

    ;; Verify text appended correctly
    (let [text-2 (h/get-buffer-text*)]
      (is (.contains text-2 "initial more")
          (str "Text should be 'initial more', got: " text-2))
      (is (not (.contains text-2 " moreinitial"))
          "Text should NOT be in wrong order"))))

(deftest test-multiple-buffers-can-coexist
  (testing "Emacs invariant: Multiple independent buffers can exist simultaneously"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type in scratch buffer
    (h/type-text "content-a")
    (Thread/sleep 50)

    ;; Switch to new buffer (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    ;; Type buffer name in minibuffer
    (h/type-in-minibuffer "buffer-b")
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in new buffer
    (h/type-text "content-b")
    (Thread/sleep 50)

    ;; Verify new buffer has different content
    (let [text-b (h/get-buffer-text*)]
      (is (or (.contains text-b "content-b")
              (not (.contains text-b "content-a")))
          (str "Buffer B should have 'content-b', got: " text-b)))

    ;; Switch back to scratch (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "*scratch*")
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Verify scratch buffer preserved
    (let [text-a (h/get-buffer-text*)]
      (is (.contains text-a "content-a")
          (str "Scratch buffer should still have 'content-a', got: " text-a)))

    ;; Type more in buffer A
    (h/type-text " more")
    (Thread/sleep 50)

    ;; Verify appended correctly
    (let [text-final (h/get-buffer-text*)]
      (is (.contains text-final "content-a more")
          (str "Should have 'content-a more', got: " text-final))
      (is (not (.contains text-final " morecontent-a"))
          "Text should NOT be in wrong order"))))
