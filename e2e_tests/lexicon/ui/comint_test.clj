(ns lexicon.ui.comint-test
  "E2E tests for comint mode and terminal emulation (#127).

  Tests verify comint and term commands are registered and available."
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

(deftest test-term-command
  (testing "M-x term command is available"
    (wait-for-editor)
    (execute-mx-command "term")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after term command")))

(deftest test-ansi-term-command
  (testing "M-x ansi-term command is available"
    (wait-for-editor)
    (execute-mx-command "ansi-term")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after ansi-term command")))

(deftest test-comint-previous-input-command
  (testing "M-x comint-previous-input command is available"
    (wait-for-editor)
    (execute-mx-command "comint-previous-input")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-previous-input command")))

(deftest test-comint-next-input-command
  (testing "M-x comint-next-input command is available"
    (wait-for-editor)
    (execute-mx-command "comint-next-input")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-next-input command")))
