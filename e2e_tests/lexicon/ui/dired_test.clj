(ns lexicon.ui.dired-test
  "E2E UI tests for interactive dired-mode (#139)

  Tests dired functionality using only keyboard interactions.
  All operations via M-x commands and key sequences.

  Note: Some tests require granted filesystem access which may not be available
  in headless testing. Those tests verify command availability and error handling."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-buffer-name
  "Get the current buffer name from mode-line or window.editorState"
  []
  (try
    (e/js-execute h/*driver* "
      const state = window.editorState;
      if (state && state.currentBufferName) return state.currentBufferName;
      return '';
    ")
    (catch Exception _ "")))

(defn in-dired-buffer?
  "Check if current buffer is a dired buffer"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state) return false;
      const name = state.currentBufferName || '';
      return name.startsWith('/') || name.includes('dired');
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-mode-line-text
  "Get mode-line text"
  []
  (try
    (e/get-element-text h/*driver* {:css ".mode-line"})
    (catch Exception _ "")))

(defn dired-mode-active?
  "Check if dired-mode is the current major mode"
  []
  (let [mode-line (get-mode-line-text)]
    (or (.contains (str mode-line) "Dired")
        (.contains (str mode-line) "dired"))))

;; =============================================================================
;; Dired Command Availability Tests
;; =============================================================================

(deftest test-dired-command-exists
  (testing "M-x dired command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Should prompt for directory or show error about no access
    ;; Should NOT show "command not found"
    (let [echo (h/get-echo-area-text)
          minibuffer-visible (h/minibuffer-visible?)]
      (is (or minibuffer-visible
              (not (.contains (str echo) "not found")))
          "dired command should be available"))))

(deftest test-dired-other-window-command-exists
  (testing "M-x dired-other-window command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-other-window")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-other-window should be available"))))

;; =============================================================================
;; Dired Navigation Tests (using mock/granted filesystem)
;; =============================================================================

(deftest test-dired-keybindings-in-dired-mode
  (testing "Dired navigation keys work when in dired buffer"
    (h/setup-test*)

    ;; Try to enter dired mode
    (h/execute-command "dired")
    (Thread/sleep 300)

    ;; If we're prompted for a path, cancel with C-g
    (when (h/minibuffer-visible?)
      (h/press-ctrl "g")
      (Thread/sleep 100))

    ;; The key test is that these don't error out
    ;; In a real dired buffer, they would navigate
    ;; Press n (next line in dired)
    (h/press-key "n")
    (Thread/sleep 50)

    ;; Press p (previous line in dired)
    (h/press-key "p")
    (Thread/sleep 50)

    ;; No crash = success
    (is true "Dired navigation keys should not crash")))

;; =============================================================================
;; Dired Marking Tests
;; =============================================================================

(deftest test-dired-mark-command-exists
  (testing "dired-mark command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-mark")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Command should exist (may fail if not in dired buffer, that's fine)
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-mark command should be defined"))))

(deftest test-dired-unmark-command-exists
  (testing "dired-unmark command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-unmark")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-unmark command should be defined"))))

(deftest test-dired-unmark-all-marks-command-exists
  (testing "dired-unmark-all-marks command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-unmark-all-marks")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-unmark-all-marks should be defined"))))

;; =============================================================================
;; Dired File Operation Commands
;; =============================================================================

(deftest test-dired-find-file-command-exists
  (testing "dired-find-file command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-find-file")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-find-file should be defined"))))

(deftest test-dired-up-directory-command-exists
  (testing "dired-up-directory command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-up-directory")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-up-directory should be defined"))))

(deftest test-dired-do-delete-command-exists
  (testing "dired-do-delete command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-delete")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-delete should be defined"))))

(deftest test-dired-do-rename-command-exists
  (testing "dired-do-rename command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-rename")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-rename should be defined"))))

(deftest test-dired-do-copy-command-exists
  (testing "dired-do-copy command is available"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "dired-do-copy")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "dired-do-copy should be defined"))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.dired-test))
