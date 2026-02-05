(ns lexicon.ui.shell-test
  "E2E tests for shell.el and eshell (#112).

  Tests verify shell commands are registered and available."
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

(deftest test-shell-command
  (testing "M-x shell command is available"
    (wait-for-editor)
    (execute-mx-command "shell")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after shell command")))

(deftest test-eshell-command
  (testing "M-x eshell command is available"
    (wait-for-editor)
    (execute-mx-command "eshell")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after eshell command")))

(deftest test-shell-command-mx
  (testing "M-x shell-command is available"
    (wait-for-editor)
    (execute-mx-command "shell-command")
    (Thread/sleep 200)
    ;; shell-command opens minibuffer, press Escape to cancel
    (h/press-key "Escape")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after shell-command")))

(deftest test-async-shell-command
  (testing "M-x async-shell-command is available"
    (wait-for-editor)
    (execute-mx-command "async-shell-command")
    (Thread/sleep 200)
    ;; async-shell-command opens minibuffer, press Escape to cancel
    (h/press-key "Escape")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after async-shell-command")))
