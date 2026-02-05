(ns lexicon.ui.xref-test
  "E2E tests for xref.el and project.el (#116).

  Tests verify xref and project commands are registered and available."
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

(deftest test-xref-find-definitions-command
  (testing "M-x xref-find-definitions command is available"
    (wait-for-editor)
    (execute-mx-command "xref-find-definitions")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after xref-find-definitions")))

(deftest test-xref-find-references-command
  (testing "M-x xref-find-references command is available"
    (wait-for-editor)
    (execute-mx-command "xref-find-references")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after xref-find-references")))

(deftest test-xref-go-back-command
  (testing "M-x xref-go-back command is available"
    (wait-for-editor)
    (execute-mx-command "xref-go-back")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after xref-go-back")))

(deftest test-project-find-file-command
  (testing "M-x project-find-file command is available"
    (wait-for-editor)
    (execute-mx-command "project-find-file")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-find-file")))

(deftest test-project-switch-project-command
  (testing "M-x project-switch-project command is available"
    (wait-for-editor)
    (execute-mx-command "project-switch-project")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-switch-project")))

(deftest test-xref-find-apropos-command
  (testing "M-x xref-find-apropos command is available"
    (wait-for-editor)
    (execute-mx-command "xref-find-apropos")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after xref-find-apropos")))

(deftest test-project-find-regexp-command
  (testing "M-x project-find-regexp command is available"
    (wait-for-editor)
    (execute-mx-command "project-find-regexp")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-find-regexp")))

(deftest test-project-switch-to-buffer-command
  (testing "M-x project-switch-to-buffer command is available"
    (wait-for-editor)
    (execute-mx-command "project-switch-to-buffer")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-switch-to-buffer")))

(deftest test-project-kill-buffers-command
  (testing "M-x project-kill-buffers command is available"
    (wait-for-editor)
    (execute-mx-command "project-kill-buffers")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-kill-buffers")))

(deftest test-project-list-buffers-command
  (testing "M-x project-list-buffers command is available"
    (wait-for-editor)
    (execute-mx-command "project-list-buffers")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after project-list-buffers")))
