(ns lexicon.e2e.editor-semantics.point-mark-test
  "E2E tests for Emacs point/mark semantic - Epic #86

  Tests critical invariant: Point (cursor) is buffer-local"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-point-is-buffer-local
  (testing "Emacs invariant: Cursor position preserved per-buffer"
    (h/setup-test*)

    ;; Type text in scratch buffer
    (h/type-text "hello world")
    (Thread/sleep 50)

    ;; Move cursor to beginning (C-a)
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; Move forward a few characters (C-f C-f C-f)
    (dotimes [_ 3]
      (h/press-ctrl "f")
      (Thread/sleep 20))

    ;; Type marker at current position
    (h/type-text "X")
    (Thread/sleep 50)

    ;; Should have "helXlo world"
    (let [text-a (h/get-buffer-text*)]
      (is (.contains text-a "helXlo")
          "Cursor should be at position 3"))

    ;; Switch to new buffer (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "buffer-b")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Type in new buffer
    (h/type-text "xyz")
    (Thread/sleep 50)

    ;; Move to beginning and forward once
    (h/press-ctrl "a")
    (Thread/sleep 20)
    (h/press-ctrl "f")
    (Thread/sleep 20)

    ;; Type marker
    (h/type-text "Y")
    (Thread/sleep 50)

    ;; Should have "xYyz"
    (let [text-b (h/get-buffer-text*)]
      (is (.contains text-b "xYyz")
          "Cursor in buffer B should be at position 1"))

    ;; Switch back to scratch (C-x b)
    (h/press-ctrl-x "b")
    (Thread/sleep 100)

    (h/type-in-minibuffer "*scratch*")
    (Thread/sleep 20)
    (h/press-minibuffer-enter)
    (Thread/sleep 100)

    ;; Cursor position in scratch should be preserved
    ;; Type another character - it should insert at saved position
    (h/type-text "Z")
    (Thread/sleep 50)

    (let [text-a-final (h/get-buffer-text*)]
      ;; Cursor should still be around where we left it in buffer A
      ;; Z should be inserted right after X, showing cursor position was preserved
      (is (or (.contains text-a-final "helXZ")
              (.contains text-a-final "XZ"))
          (str "Buffer A cursor position should be preserved - Z after X, got: " text-a-final)))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.e2e.editor-semantics.point-mark-test))
