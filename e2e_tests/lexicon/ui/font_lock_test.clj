(ns lexicon.ui.font-lock-test
  "E2E UI tests for font-lock syntax highlighting (#130)

  Tests font-lock and which-func mode using only keyboard interactions.
  All operations via M-x commands and key sequences."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-mode-line-text
  "Get the mode-line text"
  []
  (try
    (e/get-element-text h/*driver* {:css ".mode-line"})
    (catch Exception _ "")))

(defn buffer-has-highlighting?
  "Check if any syntax highlighting spans exist in the editor.
   Looks for spans with face-related classes."
  []
  (try
    (let [script "
      const spans = document.querySelectorAll('.editor-line span[class*=\"face\"]');
      return spans.length > 0;
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-highlighted-keywords
  "Get text content of keyword-highlighted spans"
  []
  (try
    (let [script "
      const spans = document.querySelectorAll('.editor-line span[class*=\"keyword\"]');
      return Array.from(spans).map(s => s.textContent);
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ [])))

;; =============================================================================
;; Font-Lock Mode Tests
;; =============================================================================

(deftest test-font-lock-mode-command-exists
  (testing "font-lock-mode command is available via M-x"
    (h/setup-test*)

    ;; Try to execute font-lock-mode
    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "font-lock-mode")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Should not show "command not found" error
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "font-lock-mode command should be available"))))

(deftest test-global-font-lock-mode-command-exists
  (testing "global-font-lock-mode command is available via M-x"
    (h/setup-test*)

    ;; Try to execute global-font-lock-mode
    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "global-font-lock-mode")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    ;; Should not show "command not found" error
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "global-font-lock-mode command should be available"))))

(deftest test-font-lock-mode-toggle
  (testing "font-lock-mode can be toggled multiple times without error"
    (h/setup-test*)

    ;; Toggle off
    (h/execute-command "font-lock-mode")
    (Thread/sleep 100)

    ;; Toggle on
    (h/execute-command "font-lock-mode")
    (Thread/sleep 100)

    ;; Toggle off again
    (h/execute-command "font-lock-mode")
    (Thread/sleep 100)

    ;; No errors should occur
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "error"))
          "font-lock-mode should toggle without errors"))))

;; =============================================================================
;; Which-Func Mode Tests
;; =============================================================================

(deftest test-which-func-mode-command-exists
  (testing "which-func-mode command is available via M-x"
    (h/setup-test*)

    (h/press-meta "x")
    (Thread/sleep 100)
    (h/type-in-minibuffer "which-func-mode")
    (h/press-minibuffer-enter)
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "which-func-mode command should be available"))))

(deftest test-which-func-mode-toggle
  (testing "which-func-mode can be toggled"
    (h/setup-test*)

    ;; Enable
    (h/execute-command "which-func-mode")
    (Thread/sleep 100)

    ;; Disable
    (h/execute-command "which-func-mode")
    (Thread/sleep 100)

    ;; No errors
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "error"))
          "which-func-mode should toggle without errors"))))

;; =============================================================================
;; Major Mode Syntax Highlighting Tests
;; =============================================================================

(deftest test-clojure-mode-sets-major-mode
  (testing "M-x clojure-mode sets the major mode"
    (h/setup-test*)

    ;; Switch to clojure-mode
    (h/execute-command "clojure-mode")
    (Thread/sleep 200)

    ;; Check mode-line for Clojure indication
    (let [mode-line (get-mode-line-text)]
      (is (or (.contains (str mode-line) "Clojure")
              (.contains (str mode-line) "clojure"))
          (str "Mode-line should show Clojure, got: " mode-line)))))

(deftest test-javascript-mode-sets-major-mode
  (testing "M-x javascript-mode sets the major mode"
    (h/setup-test*)

    ;; Switch to javascript-mode
    (h/execute-command "javascript-mode")
    (Thread/sleep 200)

    ;; Check mode-line
    (let [mode-line (get-mode-line-text)]
      (is (or (.contains (str mode-line) "JavaScript")
              (.contains (str mode-line) "javascript")
              (.contains (str mode-line) "JS"))
          (str "Mode-line should show JavaScript, got: " mode-line)))))

(deftest test-fundamental-mode-resets-mode
  (testing "M-x fundamental-mode resets to basic mode"
    (h/setup-test*)

    ;; First set a specific mode
    (h/execute-command "clojure-mode")
    (Thread/sleep 100)

    ;; Then reset to fundamental
    (h/execute-command "fundamental-mode")
    (Thread/sleep 200)

    ;; Check mode-line
    (let [mode-line (get-mode-line-text)]
      (is (or (.contains (str mode-line) "Fundamental")
              (.contains (str mode-line) "fundamental"))
          (str "Mode-line should show Fundamental, got: " mode-line)))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.font-lock-test))
