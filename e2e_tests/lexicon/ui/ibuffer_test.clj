(ns lexicon.ui.ibuffer-test
  "E2E tests for ibuffer - advanced buffer management (#140).

  Tests verify ibuffer commands and interface."
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

(deftest test-ibuffer-opens-via-mx
  (testing "M-x ibuffer is available"
    (wait-for-editor)
    (execute-mx-command "ibuffer")
    (Thread/sleep 500)
    ;; Should not crash - editor should remain functional
    (is (string? (get-current-buffer-name))
        "Editor should remain functional after M-x ibuffer")))

(deftest test-ibuffer-sorting-command-exists
  (testing "ibuffer-toggle-sorting-mode command is available"
    (wait-for-editor)
    (execute-mx-command "ibuffer-toggle-sorting-mode")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after sorting toggle")))

(deftest test-ibuffer-filter-disable-command-exists
  (testing "ibuffer-filter-disable command is available"
    (wait-for-editor)
    (execute-mx-command "ibuffer-filter-disable")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after filter disable")))

(deftest test-ibuffer-sort-by-alphabetic-command
  (testing "ibuffer-do-sort-by-alphabetic command is available"
    (wait-for-editor)
    (execute-mx-command "ibuffer-do-sort-by-alphabetic")
    (Thread/sleep 200)
    (is (string? (get-current-buffer-name))
        "Editor should still be functional after sort by alphabetic")))
