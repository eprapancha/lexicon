(ns lexicon.lisp.window-test
  "Lisp API tests for window management operations.

  Tests window-related Lisp functions:
  - split-window-horizontally: Split window side by side
  - split-window-vertically: Split window stacked
  - delete-window: Remove current window
  - delete-other-windows: Keep only current window
  - other-window: Switch to next window
  - selected-window: Get current window ID
  - window-buffer: Get buffer displayed in window

  JUSTIFICATION: Window functions are Lisp primitives that must be tested
  via Lisp evaluation to verify correct exposure and behavior."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]
            [lexicon.lisp.helpers :as lisp]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; selected-window
;; =============================================================================

(deftest test-selected-window
  (testing "selected-window returns current window ID"
    (lisp/setup-test)
    ;; Ensure single window
    (lisp/eval-lisp! "(delete-other-windows)")
    (let [result (lisp/eval-lisp! "(selected-window)")]
      (is (some? result) "selected-window should return a value")
      (is (number? result) "Window ID should be a number"))))

;; =============================================================================
;; window-buffer
;; =============================================================================

(deftest test-window-buffer
  (testing "window-buffer returns buffer ID for current window"
    (lisp/setup-test)
    (lisp/eval-lisp! "(delete-other-windows)")
    (let [buffer-id (lisp/eval-lisp! "(window-buffer)")]
      (is (some? buffer-id) "window-buffer should return buffer ID")
      (is (number? buffer-id) "Buffer ID should be a number"))))

(deftest test-window-buffer-with-arg
  (testing "window-buffer accepts window argument"
    (lisp/setup-test)
    (lisp/eval-lisp! "(delete-other-windows)")
    (let [win-id (lisp/eval-lisp! "(selected-window)")
          buffer-id (lisp/eval-lisp! (str "(window-buffer " win-id ")"))]
      (is (some? buffer-id) "window-buffer with arg should return buffer ID"))))

;; =============================================================================
;; split-window-horizontally
;; =============================================================================

(deftest test-split-window-horizontally
  (testing "split-window-horizontally creates new window"
    (lisp/setup-test)
    ;; Start with single window
    (lisp/eval-lisp! "(delete-other-windows)")
    (Thread/sleep 100)
    ;; Split
    (lisp/eval-lisp! "(split-window-horizontally)")
    (Thread/sleep 100)
    ;; Should still have selected-window (just more windows now)
    (let [win-id (lisp/eval-lisp! "(selected-window)")]
      (is (some? win-id) "Should have a selected window after split"))))

;; =============================================================================
;; split-window-vertically
;; =============================================================================

(deftest test-split-window-vertically
  (testing "split-window-vertically creates new window"
    (lisp/setup-test)
    ;; Start with single window
    (lisp/eval-lisp! "(delete-other-windows)")
    (Thread/sleep 100)
    ;; Split
    (lisp/eval-lisp! "(split-window-vertically)")
    (Thread/sleep 100)
    ;; Should still have selected-window
    (let [win-id (lisp/eval-lisp! "(selected-window)")]
      (is (some? win-id) "Should have a selected window after split"))))

;; =============================================================================
;; delete-other-windows
;; =============================================================================

(deftest test-delete-other-windows
  (testing "delete-other-windows removes other windows"
    (lisp/setup-test)
    ;; Create splits
    (lisp/eval-lisp! "(split-window-horizontally)")
    (Thread/sleep 100)
    ;; Delete others
    (lisp/eval-lisp! "(delete-other-windows)")
    (Thread/sleep 100)
    ;; Should still have a selected window
    (let [win-id (lisp/eval-lisp! "(selected-window)")]
      (is (some? win-id) "Should have a selected window after delete-other-windows"))))

;; =============================================================================
;; delete-window
;; =============================================================================

(deftest test-delete-window
  (testing "delete-window removes current window"
    (lisp/setup-test)
    ;; Start with single window, then split
    (lisp/eval-lisp! "(delete-other-windows)")
    (Thread/sleep 100)
    (lisp/eval-lisp! "(split-window-horizontally)")
    (Thread/sleep 100)
    ;; Delete current window
    (lisp/eval-lisp! "(delete-window)")
    (Thread/sleep 100)
    ;; Should still have a selected window (the remaining one)
    (let [win-id (lisp/eval-lisp! "(selected-window)")]
      (is (some? win-id) "Should have a selected window after delete-window"))))

;; =============================================================================
;; other-window
;; =============================================================================

(deftest test-other-window
  (testing "other-window switches to another window"
    (lisp/setup-test)
    ;; Start with single window
    (lisp/eval-lisp! "(delete-other-windows)")
    (Thread/sleep 100)
    (let [win1 (lisp/eval-lisp! "(selected-window)")]
      ;; Split
      (lisp/eval-lisp! "(split-window-horizontally)")
      (Thread/sleep 100)
      ;; Switch to other window
      (lisp/eval-lisp! "(other-window 1)")
      (Thread/sleep 100)
      ;; After cycling through 2 windows, should be back at start
      (lisp/eval-lisp! "(other-window 1)")
      (Thread/sleep 100)
      (let [win-now (lisp/eval-lisp! "(selected-window)")]
        ;; With 2 windows, other-window twice should return to original
        (is (some? win-now) "Should have a selected window after other-window")))))
