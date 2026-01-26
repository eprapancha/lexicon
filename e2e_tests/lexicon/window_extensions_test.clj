(ns lexicon.window-extensions-test
  "E2E tests for window extensions - user-visible window behavior.

  Emacs source: lisp/windmove.el, lisp/winner.el, lisp/tab-bar.el

  Note: windmove, winner, tab-bar are Lisp APIs. E2E tests focus on
  user-visible window manipulation via keyboard. API-specific tests
  are placeholders for unit test coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; User-Visible Window Manipulation
;; =============================================================================

(deftest test-user-splits-window
  (testing "User can split window with keyboard"
    (h/setup-test*)
    (h/clear-buffer)

    ;; C-x 2 splits window horizontally
    (h/press-ctrl-x "2")
    (Thread/sleep 200)

    (is (>= (h/get-window-count*) 2) "Should have 2+ windows after split")

    ;; Clean up - C-x 1 deletes other windows
    (h/press-ctrl-x "1")
    (Thread/sleep 100)))

(deftest test-user-switches-windows
  (testing "User can switch between windows"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Split window
    (h/press-ctrl-x "2")
    (Thread/sleep 200)

    ;; C-x o switches to other window
    (h/press-ctrl-x "o")
    (Thread/sleep 100)

    ;; We should still be in a valid state
    (is (>= (h/get-window-count*) 2) "Should still have windows")

    ;; Clean up
    (h/press-ctrl-x "1")
    (Thread/sleep 100)))

;; =============================================================================
;; Windmove - Placeholders for Unit Tests
;; =============================================================================

(deftest test-windmove-navigation
  (testing "windmove-right moves to right window"
    ;; windmove is a Lisp function
    (is false "windmove tested via unit tests")))

;; =============================================================================
;; Winner Mode - Placeholders for Unit Tests
;; =============================================================================

(deftest test-winner-mode-undo
  (testing "winner-undo restores window configuration"
    ;; winner-mode is a Lisp function
    (is false "winner-mode tested via unit tests")))

;; =============================================================================
;; Tab Bar - Placeholders for Unit Tests
;; =============================================================================

(deftest test-tab-bar-basics
  (testing "tab-bar-new-tab creates tab"
    ;; tab-bar is a Lisp function
    (is false "tab-bar tested via unit tests")))
