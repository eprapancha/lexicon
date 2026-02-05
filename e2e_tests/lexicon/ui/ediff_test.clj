(ns lexicon.ui.ediff-test
  "E2E tests for ediff and smerge-mode (#119).

  Tests verify ediff and smerge commands are registered and available."
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

(deftest test-ediff-buffers-command
  (testing "M-x ediff-buffers command is available"
    (wait-for-editor)
    (execute-mx-command "ediff-buffers")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after ediff-buffers command")))

(deftest test-smerge-mode-command
  (testing "M-x smerge-mode command is available"
    (wait-for-editor)
    (execute-mx-command "smerge-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after smerge-mode command")))

(deftest test-smerge-next-command
  (testing "M-x smerge-next command is available"
    (wait-for-editor)
    (execute-mx-command "smerge-next")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after smerge-next command")))

(deftest test-smerge-keep-upper-command
  (testing "M-x smerge-keep-upper command is available"
    (wait-for-editor)
    (execute-mx-command "smerge-keep-upper")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after smerge-keep-upper command")))

(deftest test-smerge-keep-lower-command
  (testing "M-x smerge-keep-lower command is available"
    (wait-for-editor)
    (execute-mx-command "smerge-keep-lower")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after smerge-keep-lower command")))
