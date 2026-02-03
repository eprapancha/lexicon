(ns lexicon.ui.windows.semantics-test
  "E2E tests for Emacs window semantic - Epic #86

  Tests critical invariant: Buffer identity stable across multiple window views"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

(deftest test-buffer-identity-stable-across-window-views
  (testing "Emacs invariant: Buffer identity stable when displayed in multiple windows"
    (h/setup-test*)

    ;; Type initial text
    (h/type-text "initial")
    (Thread/sleep 50)

    ;; Split window (C-x 2)
    (h/press-ctrl-x "2")
    (Thread/sleep 100)

    ;; Verify we have 2 windows
    (let [windows (e/query-all h/*driver* {:css ".window-pane"})]
      (is (>= (count windows) 2) "Should have at least 2 windows after split"))

    ;; Type more text - should appear in both windows
    (h/type-text " more")
    (Thread/sleep 100)

    ;; Verify text appears (both windows show same buffer)
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "initial more")
          (str "Buffer should show 'initial more', got: " editor-text)))))

(deftest test-window-deletion-preserves-buffer
  (testing "Emacs invariant: Deleting window doesn't kill the buffer"
    (h/setup-test*)

    ;; Type some content
    (h/type-text "content")
    (Thread/sleep 50)

    ;; Split window
    (h/press-ctrl-x "2")
    (Thread/sleep 100)

    ;; Delete other windows (C-x 1)
    (h/press-ctrl-x "1")
    (Thread/sleep 100)

    ;; Verify buffer content still exists
    (let [editor-text (h/get-buffer-text*)]
      (is (.contains editor-text "content")
          (str "Buffer content should be preserved after window deletion, got: " editor-text)))

    ;; Verify we're back to 1 window
    (let [windows (e/query-all h/*driver* {:css ".window-pane"})]
      (is (= 1 (count windows))
          (str "Should have 1 window after C-x 1, got: " (count windows))))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.ui.windows.semantics-test))
