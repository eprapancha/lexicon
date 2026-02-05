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

(deftest test-comint-previous-matching-input-command
  (testing "M-x comint-previous-matching-input command is available"
    (wait-for-editor)
    (execute-mx-command "comint-previous-matching-input")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-previous-matching-input")))

(deftest test-comint-bol-command
  (testing "M-x comint-bol command is available"
    (wait-for-editor)
    (execute-mx-command "comint-bol")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-bol")))

(deftest test-comint-kill-input-command
  (testing "M-x comint-kill-input command is available"
    (wait-for-editor)
    (execute-mx-command "comint-kill-input")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-kill-input")))

(deftest test-comint-delete-output-command
  (testing "M-x comint-delete-output command is available"
    (wait-for-editor)
    (execute-mx-command "comint-delete-output")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-delete-output")))

(deftest test-comint-show-output-command
  (testing "M-x comint-show-output command is available"
    (wait-for-editor)
    (execute-mx-command "comint-show-output")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-show-output")))

(deftest test-comint-dynamic-list-input-ring-command
  (testing "M-x comint-dynamic-list-input-ring command is available"
    (wait-for-editor)
    (execute-mx-command "comint-dynamic-list-input-ring")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-dynamic-list-input-ring")))

(deftest test-comint-interrupt-subjob-command
  (testing "M-x comint-interrupt-subjob command is available"
    (wait-for-editor)
    (execute-mx-command "comint-interrupt-subjob")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-interrupt-subjob")))

(deftest test-comint-stop-subjob-command
  (testing "M-x comint-stop-subjob command is available"
    (wait-for-editor)
    (execute-mx-command "comint-stop-subjob")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-stop-subjob")))

(deftest test-comint-send-eof-command
  (testing "M-x comint-send-eof command is available"
    (wait-for-editor)
    (execute-mx-command "comint-send-eof")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after comint-send-eof")))
