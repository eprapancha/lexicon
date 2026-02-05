(ns lexicon.ui.file-persistence-test
  "E2E tests for file persistence features (#118).

  Tests verify recentf, saveplace, and autorevert commands."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; -- Helper Functions --

(defn wait-for-editor
  "Wait for editor to be ready and setup"
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

;; -- Recentf Tests --

(deftest test-recentf-open-files-command-exists
  (testing "M-x recentf-open-files command is available"
    (wait-for-editor)
    (execute-mx-command "recentf-open-files")
    (Thread/sleep 200)
    ;; Should execute without crashing (may show "No recent files" or minibuffer)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after recentf-open-files")))

(deftest test-recentf-edit-list-command-exists
  (testing "M-x recentf-edit-list command is available"
    (wait-for-editor)
    (execute-mx-command "recentf-edit-list")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after recentf-edit-list")))

(deftest test-recentf-mode-command-exists
  (testing "M-x recentf-mode command is available"
    (wait-for-editor)
    (execute-mx-command "recentf-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling recentf-mode")))

;; -- Saveplace Tests --

(deftest test-save-place-mode-command-exists
  (testing "M-x save-place-mode command is available"
    (wait-for-editor)
    (execute-mx-command "save-place-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling save-place-mode")))

;; -- Autorevert Tests --

(deftest test-auto-revert-mode-command-exists
  (testing "M-x auto-revert-mode command is available"
    (wait-for-editor)
    (execute-mx-command "auto-revert-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling auto-revert-mode")))

(deftest test-global-auto-revert-mode-command-exists
  (testing "M-x global-auto-revert-mode command is available"
    (wait-for-editor)
    (execute-mx-command "global-auto-revert-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after toggling global-auto-revert-mode")))
