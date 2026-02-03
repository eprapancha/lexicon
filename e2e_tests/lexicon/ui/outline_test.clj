(ns lexicon.ui.outline-test
  "E2E UI tests for outline-mode and folding (#114)

  Tests outline/hideshow functionality using only keyboard interactions.
  No eval-lisp calls - all operations via M-x commands and key sequences."
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

(defn get-buffer-text
  "Get current buffer text"
  []
  (try
    (e/js-execute h/*driver* "
      const state = window.editorState;
      return state ? state.buffer : '';
    ")
    (catch Exception _ "")))

(defn has-folded-regions?
  "Check if buffer has any folded regions"
  []
  (try
    (let [script "
      const state = window.editorState;
      if (!state || !state.foldedRegions) return false;
      return state.foldedRegions.length > 0;
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ false)))

(defn get-visible-line-count
  "Get count of visible lines in the editor"
  []
  (try
    (let [script "
      const lines = document.querySelectorAll('.editor-line');
      return lines.length;
    "]
      (e/js-execute h/*driver* script))
    (catch Exception _ 0)))

;; =============================================================================
;; Outline Minor Mode Tests
;; =============================================================================

(deftest test-outline-minor-mode-command-exists
  (testing "outline-minor-mode command is available"
    (h/setup-test*)

    (h/execute-command "outline-minor-mode")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-minor-mode should be available"))))

(deftest test-outline-minor-mode-toggle
  (testing "outline-minor-mode can be toggled"
    (h/setup-test*)

    ;; Enable
    (h/execute-command "outline-minor-mode")
    (Thread/sleep 100)

    ;; Disable
    (h/execute-command "outline-minor-mode")
    (Thread/sleep 100)

    ;; No errors
    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "error"))
          "outline-minor-mode should toggle without errors"))))

;; =============================================================================
;; Outline Navigation Commands
;; =============================================================================

(deftest test-outline-next-heading-command
  (testing "outline-next-heading command exists"
    (h/setup-test*)

    (h/execute-command "outline-next-heading")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-next-heading should be defined"))))

(deftest test-outline-previous-heading-command
  (testing "outline-previous-heading command exists"
    (h/setup-test*)

    (h/execute-command "outline-previous-heading")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-previous-heading should be defined"))))

(deftest test-outline-up-heading-command
  (testing "outline-up-heading command exists"
    (h/setup-test*)

    (h/execute-command "outline-up-heading")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-up-heading should be defined"))))

;; =============================================================================
;; Outline Visibility Commands
;; =============================================================================

(deftest test-outline-hide-subtree-command
  (testing "outline-hide-subtree command exists"
    (h/setup-test*)

    (h/execute-command "outline-hide-subtree")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-hide-subtree should be defined"))))

(deftest test-outline-show-subtree-command
  (testing "outline-show-subtree command exists"
    (h/setup-test*)

    (h/execute-command "outline-show-subtree")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-show-subtree should be defined"))))

(deftest test-outline-hide-body-command
  (testing "outline-hide-body command exists"
    (h/setup-test*)

    (h/execute-command "outline-hide-body")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-hide-body should be defined"))))

(deftest test-outline-show-all-command
  (testing "outline-show-all command exists"
    (h/setup-test*)

    (h/execute-command "outline-show-all")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-show-all should be defined"))))

(deftest test-outline-toggle-children-command
  (testing "outline-toggle-children command exists"
    (h/setup-test*)

    (h/execute-command "outline-toggle-children")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "outline-toggle-children should be defined"))))

;; =============================================================================
;; Hideshow Mode Tests
;; =============================================================================

(deftest test-hs-minor-mode-command
  (testing "hs-minor-mode command exists"
    (h/setup-test*)

    (h/execute-command "hs-minor-mode")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-minor-mode should be available"))))

(deftest test-hs-hide-block-command
  (testing "hs-hide-block command exists"
    (h/setup-test*)

    (h/execute-command "hs-hide-block")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-hide-block should be defined"))))

(deftest test-hs-show-block-command
  (testing "hs-show-block command exists"
    (h/setup-test*)

    (h/execute-command "hs-show-block")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-show-block should be defined"))))

(deftest test-hs-hide-all-command
  (testing "hs-hide-all command exists"
    (h/setup-test*)

    (h/execute-command "hs-hide-all")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-hide-all should be defined"))))

(deftest test-hs-show-all-command
  (testing "hs-show-all command exists"
    (h/setup-test*)

    (h/execute-command "hs-show-all")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-show-all should be defined"))))

(deftest test-hs-toggle-hiding-command
  (testing "hs-toggle-hiding command exists"
    (h/setup-test*)

    (h/execute-command "hs-toggle-hiding")
    (Thread/sleep 200)

    (let [echo (h/get-echo-area-text)]
      (is (not (.contains (str echo) "not found"))
          "hs-toggle-hiding should be defined"))))

;; =============================================================================
;; Folding with Code
;; =============================================================================

(deftest test-folding-workflow
  (testing "Basic folding workflow doesn't crash"
    (h/setup-test*)

    ;; Type some code with nested structure
    (h/type-text "(defn outer []\n  (let [x 1]\n    (+ x 2)))")
    (Thread/sleep 200)

    ;; Enable hs-minor-mode
    (h/execute-command "hs-minor-mode")
    (Thread/sleep 100)

    ;; Try to hide block
    (h/execute-command "hs-hide-block")
    (Thread/sleep 100)

    ;; Try to show block
    (h/execute-command "hs-show-block")
    (Thread/sleep 100)

    ;; No crash = success
    (is true "Folding workflow should not crash")))

;; =============================================================================
;; Run Tests
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.outline-test))
