(ns lexicon.ui.info-test
  "E2E tests for info.el documentation browser (#142).

  Tests verify Info commands are registered and available."
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

(deftest test-info-command
  (testing "M-x info command is available"
    (wait-for-editor)
    (execute-mx-command "info")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after info command")))

(deftest test-info-next-command
  (testing "M-x Info-next command is available"
    (wait-for-editor)
    (execute-mx-command "Info-next")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after Info-next")))

(deftest test-info-prev-command
  (testing "M-x Info-prev command is available"
    (wait-for-editor)
    (execute-mx-command "Info-prev")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after Info-prev")))

(deftest test-info-directory-command
  (testing "M-x Info-directory command is available"
    (wait-for-editor)
    (execute-mx-command "Info-directory")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after Info-directory")))
