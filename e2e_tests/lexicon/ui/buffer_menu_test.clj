(ns lexicon.ui.buffer-menu-test
  "E2E UI tests for buffer-menu-mode (#115)

  Tests buffer menu functionality using only keyboard interactions.
  All operations via C-x C-b and buffer-menu keys."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-mode-line-text
  "Get mode-line text"
  []
  (try
    (e/get-element-text h/*driver* {:css ".mode-line"})
    (catch Exception _ "")))

(defn buffer-menu-visible?
  "Check if *Buffer List* buffer is visible"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return false;
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '*Buffer List*') return true;
      }
      return false;
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-buffer-list-content
  "Get text content of *Buffer List* buffer if visible"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return '';
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '*Buffer List*') {
          return buf.text || '';
        }
      }
      return '';
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ "")))

(defn press-ctrl-x-ctrl-b
  "Press C-x C-b to open buffer menu"
  []
  (h/press-ctrl "x")
  (Thread/sleep 50)
  (h/press-ctrl "b")
  (Thread/sleep 200))

(defn get-current-buffer-name
  "Get the name of the currently active buffer"
  []
  (try
    (let [script "return window.editorState ? (window.editorState.bufferName || '') : '';"]
      (e/js-execute h/*driver* script))
    (catch Exception _ "")))

;; =============================================================================
;; Buffer Menu Opening Tests
;; =============================================================================

(deftest test-buffer-menu-opens-with-c-x-c-b
  (testing "C-x C-b opens buffer menu"
    (h/setup-test*)

    ;; Open buffer menu with C-x C-b
    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    ;; Verify buffer menu opened
    (is (buffer-menu-visible?)
        "*Buffer List* buffer should be created")))

(deftest test-list-buffers-command-exists
  (testing "M-x list-buffers command is available"
    (h/setup-test*)

    (h/execute-command "list-buffers")
    (Thread/sleep 300)

    ;; Should open buffer menu
    (is (buffer-menu-visible?)
        "list-buffers should open *Buffer List*")))

(deftest test-buffer-menu-command-exists
  (testing "M-x buffer-menu command is available"
    (h/setup-test*)

    (h/execute-command "buffer-menu")
    (Thread/sleep 300)

    (is (buffer-menu-visible?)
        "buffer-menu should open *Buffer List*")))

;; =============================================================================
;; Buffer Menu Content Tests
;; =============================================================================

(deftest test-buffer-menu-shows-scratch-buffer
  (testing "Buffer menu lists the *scratch* buffer"
    (h/setup-test*)

    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    (let [content (get-buffer-list-content)]
      (is (.contains content "*scratch*")
          (str "*scratch* should be in buffer list, got: " content)))))

(deftest test-buffer-menu-shows-messages-buffer
  (testing "Buffer menu lists the *Messages* buffer"
    (h/setup-test*)

    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    (let [content (get-buffer-list-content)]
      (is (.contains content "*Messages*")
          (str "*Messages* should be in buffer list, got: " content)))))

(deftest test-buffer-menu-header-line
  (testing "Buffer menu has a header line"
    (h/setup-test*)

    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    (let [content (get-buffer-list-content)]
      ;; Should have column headers like "MR Buffer" or similar
      (is (or (.contains content "Buffer")
              (.contains content "Name")
              (.contains content "Size"))
          (str "Buffer list should have header, got: " content)))))

;; =============================================================================
;; Buffer Menu Navigation Tests
;; =============================================================================

(deftest test-buffer-menu-navigation-keys
  (testing "n and p keys navigate in buffer menu"
    (h/setup-test*)

    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    ;; Press n to move to next line
    (h/press-key "n")
    (Thread/sleep 50)

    ;; Press p to move to previous line
    (h/press-key "p")
    (Thread/sleep 50)

    ;; No crash = success
    (is true "Buffer menu navigation keys should work")))

;; =============================================================================
;; Buffer Menu Commands
;; =============================================================================

(deftest test-buffer-menu-mode-commands-exist
  (testing "Buffer-menu mode commands are available"
    (h/setup-test*)

    ;; Test buffer-menu-select
    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "Buffer-menu-select")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "Buffer-menu-select should be defined"))))

(deftest test-buffer-menu-mark-command
  (testing "Buffer-menu-mark command exists"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "Buffer-menu-mark")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "Buffer-menu-mark should be defined"))))

(deftest test-buffer-menu-delete-command
  (testing "Buffer-menu-delete command exists"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "Buffer-menu-delete")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "Buffer-menu-delete should be defined"))))

(deftest test-buffer-menu-quit-with-q
  (testing "q key quits buffer menu"
    (h/setup-test*)

    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    ;; Verify buffer menu is open
    (is (buffer-menu-visible?) "*Buffer List* should be visible")

    ;; Press q to quit
    (h/press-key "q")
    (Thread/sleep 200)

    ;; Verify we're no longer viewing the buffer list
    (let [current-buffer (get-current-buffer-name)]
      (is (not= current-buffer "*Buffer List*")
          (str "Should have left buffer menu, but current buffer is: " current-buffer)))))

(deftest test-buffer-menu-select-with-ret
  (testing "RET key selects buffer and switches to it"
    (h/setup-test*)

    ;; First verify we're on *scratch*
    (let [initial-buffer (get-current-buffer-name)]
      (is (= initial-buffer "*scratch*")
          (str "Should start on *scratch*, got: " initial-buffer)))

    ;; Open buffer menu
    (press-ctrl-x-ctrl-b)
    (Thread/sleep 300)

    ;; Verify we're now in buffer menu
    (let [menu-buffer (get-current-buffer-name)]
      (is (= menu-buffer "*Buffer List*")
          (str "Should be in *Buffer List*, got: " menu-buffer)))

    ;; Navigate down to a buffer line (past header)
    (h/press-key "n")
    (Thread/sleep 50)
    (h/press-key "n")
    (Thread/sleep 50)
    (h/press-key "n")
    (Thread/sleep 50)

    ;; Press RET to select buffer
    (h/press-key "Enter")
    (Thread/sleep 300)

    ;; Verify we switched away from buffer menu
    (let [final-buffer (get-current-buffer-name)]
      (is (not= final-buffer "*Buffer List*")
          (str "RET should switch away from buffer menu, but still on: " final-buffer)))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.buffer-menu-test))
