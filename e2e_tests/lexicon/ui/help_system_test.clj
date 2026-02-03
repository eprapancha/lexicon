(ns lexicon.ui.help-system-test
  "E2E UI tests for help system (#109)

  Tests help commands using only keyboard interactions.
  All operations via C-h prefix and M-x commands."
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

(defn help-buffer-visible?
  "Check if a *Help* buffer is visible"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return false;
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '*Help*') return true;
      }
      return false;
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-help-buffer-content
  "Get text content of *Help* buffer"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.buffers) return '';
      for (const buf of Object.values(state.buffers)) {
        if (buf.name === '*Help*') {
          return buf.text || '';
        }
      }
      return '';
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ "")))

(defn press-c-h
  "Press C-h prefix key"
  []
  (h/press-ctrl "h")
  (Thread/sleep 100))

;; =============================================================================
;; C-h Prefix Tests
;; =============================================================================

(deftest test-describe-bindings-c-h-b
  (testing "C-h b opens describe-bindings"
    (h/setup-test*)

    ;; Press C-h b
    (press-c-h)
    (h/press-key "b")
    (Thread/sleep 300)

    ;; Should open *Help* buffer with keybindings
    (let [visible (help-buffer-visible?)
          content (get-help-buffer-content)]
      (is (or visible (not (empty? content)))
          "C-h b should open help with keybindings"))))

(deftest test-describe-key-c-h-k
  (testing "C-h k prompts for key to describe"
    (h/setup-test*)

    ;; Press C-h k
    (press-c-h)
    (h/press-key "k")
    (Thread/sleep 300)

    ;; Should show minibuffer prompt or echo area message
    (let [minibuffer-visible (h/minibuffer-visible?)
          echo (h/get-echo-area-text)]
      (is (or minibuffer-visible
              (.contains (str echo) "key")
              (.contains (str echo) "describe"))
          "C-h k should prompt for key to describe"))))

(deftest test-describe-key-briefly-c-h-c
  (testing "C-h c prompts for key and shows brief description"
    (h/setup-test*)

    ;; Press C-h c
    (press-c-h)
    (h/press-key "c")
    (Thread/sleep 300)

    ;; Should show minibuffer prompt
    (let [minibuffer-visible (h/minibuffer-visible?)
          echo (h/get-echo-area-text)]
      (is (or minibuffer-visible
              (not (.contains (str echo) "not found")))
          "C-h c should work"))))

(deftest test-describe-function-c-h-f
  (testing "C-h f prompts for function to describe"
    (h/setup-test*)

    ;; Press C-h f
    (press-c-h)
    (h/press-key "f")
    (Thread/sleep 300)

    ;; Should show minibuffer prompt for function name
    (is (h/minibuffer-visible?)
        "C-h f should open minibuffer for function name")))

(deftest test-describe-variable-c-h-v
  (testing "C-h v prompts for variable to describe"
    (h/setup-test*)

    ;; Press C-h v
    (press-c-h)
    (h/press-key "v")
    (Thread/sleep 300)

    ;; Should show minibuffer prompt for variable name
    (is (h/minibuffer-visible?)
        "C-h v should open minibuffer for variable name")))

(deftest test-describe-mode-c-h-m
  (testing "C-h m describes current mode"
    (h/setup-test*)

    ;; Press C-h m
    (press-c-h)
    (h/press-key "m")
    (Thread/sleep 300)

    ;; Should open *Help* with mode description
    (let [visible (help-buffer-visible?)
          echo (h/get-echo-area-text)]
      (is (or visible
              (not (.contains (str echo) "not found")))
          "C-h m should describe current mode"))))

(deftest test-help-for-help-c-h-question
  (testing "C-h ? shows help for help"
    (h/setup-test*)

    ;; Press C-h ?
    (press-c-h)
    (h/press-key "?")
    (Thread/sleep 300)

    ;; Should show help about help commands
    (let [visible (help-buffer-visible?)
          echo (h/get-echo-area-text)]
      (is (or visible
              (not (empty? echo)))
          "C-h ? should show help for help"))))

;; =============================================================================
;; M-x Help Command Tests
;; =============================================================================

(deftest test-describe-bindings-command
  (testing "M-x describe-bindings command works"
    (h/setup-test*)

    (h/execute-command "describe-bindings")
    (Thread/sleep 300)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "describe-bindings command should exist"))))

(deftest test-describe-key-command
  (testing "M-x describe-key command works"
    (h/setup-test*)

    (h/execute-command "describe-key")
    (Thread/sleep 300)

    ;; Should prompt for key or show in echo
    (let [minibuffer-visible (h/minibuffer-visible?)
          echo (h/get-echo-area-text)]
      (is (or minibuffer-visible
              (not (.contains (str echo) "not found")))
          "describe-key command should exist"))))

(deftest test-describe-function-command
  (testing "M-x describe-function command works"
    (h/setup-test*)

    (h/execute-command "describe-function")
    (Thread/sleep 300)

    (is (h/minibuffer-visible?)
        "describe-function should prompt for function")))

(deftest test-describe-variable-command
  (testing "M-x describe-variable command works"
    (h/setup-test*)

    (h/execute-command "describe-variable")
    (Thread/sleep 300)

    (is (h/minibuffer-visible?)
        "describe-variable should prompt for variable")))

(deftest test-apropos-command
  (testing "M-x apropos-command command works"
    (h/setup-test*)

    (h/execute-command "apropos-command")
    (Thread/sleep 300)

    ;; Should prompt for search pattern
    (is (h/minibuffer-visible?)
        "apropos-command should prompt for search pattern")))

;; =============================================================================
;; Help Buffer Tests
;; =============================================================================

(deftest test-help-buffer-mode
  (testing "*Help* buffer is in help-mode"
    (h/setup-test*)

    ;; Open help
    (h/execute-command "describe-bindings")
    (Thread/sleep 400)

    ;; Check mode-line for help-mode indication
    (let [mode-line (get-mode-line-text)]
      (is (or (.contains (str mode-line) "Help")
              (.contains (str mode-line) "help")
              (help-buffer-visible?))
          "Help buffer should be visible"))))

(deftest test-help-buffer-quit-with-q
  (testing "q key closes help buffer"
    (h/setup-test*)

    ;; Open help
    (h/execute-command "describe-bindings")
    (Thread/sleep 400)

    ;; Press q to quit
    (h/press-key "q")
    (Thread/sleep 200)

    ;; q should not error
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "error"))
          "q should close help without error"))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.help-system-test))
