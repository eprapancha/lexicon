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

(deftest ^:skip test-user-splits-window
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

(deftest ^:skip test-user-switches-windows
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
;; Windmove - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-windmove-navigation
  (testing "windmove-right moves to right window"
    ;; windmove is a Lisp function
    (is true "PENDING: windmove - needs E2E implementation")))

;; =============================================================================
;; Winner Mode - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-winner-mode-undo
  (testing "winner-undo restores window configuration"
    ;; winner-mode is a Lisp function
    (is true "PENDING: winner-mode - needs E2E implementation")))

;; =============================================================================
;; Tab Bar - PENDING E2E Implementation
;; =============================================================================

(deftest ^:skip test-tab-bar-basics
  (testing "tab-bar-new-tab creates tab"
    ;; tab-bar is a Lisp function
    (is true "PENDING: tab-bar - needs E2E implementation")))
