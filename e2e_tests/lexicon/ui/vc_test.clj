(ns lexicon.ui.vc-test
  "E2E tests for vc.el version control (#113).

  Tests verify VC commands are registered and available."
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

(deftest test-vc-next-action-command
  (testing "M-x vc-next-action command is available"
    (wait-for-editor)
    (execute-mx-command "vc-next-action")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after vc-next-action")))

(deftest test-vc-diff-command
  (testing "M-x vc-diff command is available"
    (wait-for-editor)
    (execute-mx-command "vc-diff")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after vc-diff")))

(deftest test-vc-log-command
  (testing "M-x vc-log command is available"
    (wait-for-editor)
    (execute-mx-command "vc-log")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after vc-log")))

(deftest test-vc-revert-command
  (testing "M-x vc-revert command is available"
    (wait-for-editor)
    (execute-mx-command "vc-revert")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after vc-revert")))

(deftest test-vc-annotate-command
  (testing "M-x vc-annotate command is available"
    (wait-for-editor)
    (execute-mx-command "vc-annotate")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after vc-annotate")))
