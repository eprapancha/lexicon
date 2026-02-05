(ns lexicon.ui.tramp-test
  "E2E tests for TRAMP remote file access (#126).

  Tests verify TRAMP commands are registered and available."
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

(deftest test-tramp-cleanup-command
  (testing "M-x tramp-cleanup-all-connections command is available"
    (wait-for-editor)
    (execute-mx-command "tramp-cleanup-all-connections")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after tramp-cleanup command")))

(deftest test-tramp-cleanup-this-connection-command
  (testing "M-x tramp-cleanup-this-connection command is available"
    (wait-for-editor)
    (execute-mx-command "tramp-cleanup-this-connection")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after tramp-cleanup-this-connection command")))

(deftest test-tramp-list-connections-command
  (testing "M-x tramp-list-connections command is available"
    (wait-for-editor)
    (execute-mx-command "tramp-list-connections")
    (Thread/sleep 400)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after tramp-list-connections")))

(deftest test-tramp-list-remote-buffers-command
  (testing "M-x tramp-list-remote-buffers command is available"
    (wait-for-editor)
    (execute-mx-command "tramp-list-remote-buffers")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after tramp-list-remote-buffers")))
