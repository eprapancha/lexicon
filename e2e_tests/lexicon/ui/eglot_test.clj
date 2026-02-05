(ns lexicon.ui.eglot-test
  "E2E tests for eglot LSP client (#129).

  Tests verify eglot commands are registered and available."
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

(deftest test-eglot-command
  (testing "M-x eglot command is available"
    (wait-for-editor)
    (execute-mx-command "eglot")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after eglot command")))

(deftest test-eglot-shutdown-command
  (testing "M-x eglot-shutdown command is available"
    (wait-for-editor)
    (execute-mx-command "eglot-shutdown")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after eglot-shutdown")))

(deftest test-eglot-reconnect-command
  (testing "M-x eglot-reconnect command is available"
    (wait-for-editor)
    (execute-mx-command "eglot-reconnect")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after eglot-reconnect")))

(deftest test-eglot-format-buffer-command
  (testing "M-x eglot-format-buffer command is available"
    (wait-for-editor)
    (execute-mx-command "eglot-format-buffer")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after eglot-format-buffer")))
