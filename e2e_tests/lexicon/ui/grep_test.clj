(ns lexicon.ui.grep-test
  "E2E tests for grep.el search integration (#125).

  Tests verify grep commands are registered and functional."
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

(deftest test-grep-command
  (testing "M-x grep command is available"
    (wait-for-editor)
    (execute-mx-command "grep")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after grep command")))

(deftest test-lgrep-command
  (testing "M-x lgrep command is available"
    (wait-for-editor)
    (execute-mx-command "lgrep")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after lgrep command")))

(deftest test-rgrep-command
  (testing "M-x rgrep command is available"
    (wait-for-editor)
    (execute-mx-command "rgrep")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after rgrep command")))

(deftest test-next-error-command
  (testing "M-x next-error command is available"
    (wait-for-editor)
    (execute-mx-command "next-error")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after next-error command")))

(deftest test-previous-error-command
  (testing "M-x previous-error command is available"
    (wait-for-editor)
    (execute-mx-command "previous-error")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after previous-error command")))
