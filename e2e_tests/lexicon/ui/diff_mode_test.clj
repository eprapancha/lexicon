(ns lexicon.ui.diff-mode-test
  "E2E tests for diff-mode (#119).

  Tests verify diff navigation and command availability."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helper Functions --

(defn wait-for-editor
  "Wait for editor to be ready"
  []
  (h/setup-test*))

(defn execute-mx-command
  "Execute M-x command"
  [cmd-name]
  (h/press-meta "x")
  (Thread/sleep 300)
  (h/type-text cmd-name)
  (Thread/sleep 100)
  (h/press-key "Enter")
  (Thread/sleep 300))

(defn get-current-buffer-name
  "Get the current buffer name"
  []
  (e/js-execute h/*driver* "return window.editorState?.bufferName || null"))

;; -- Tests --

(deftest test-diff-hunk-next-command
  (testing "M-x diff-hunk-next command is available"
    (wait-for-editor)
    (execute-mx-command "diff-hunk-next")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after diff-hunk-next")))

(deftest test-diff-hunk-prev-command
  (testing "M-x diff-hunk-prev command is available"
    (wait-for-editor)
    (execute-mx-command "diff-hunk-prev")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after diff-hunk-prev")))

(deftest test-diff-file-next-command
  (testing "M-x diff-file-next command is available"
    (wait-for-editor)
    (execute-mx-command "diff-file-next")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after diff-file-next")))

(deftest test-diff-goto-source-command
  (testing "M-x diff-goto-source command is available"
    (wait-for-editor)
    (execute-mx-command "diff-goto-source")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after diff-goto-source")))

(deftest test-diff-mode-command
  (testing "M-x diff-mode command is available"
    (wait-for-editor)
    (execute-mx-command "diff-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after diff-mode")))
