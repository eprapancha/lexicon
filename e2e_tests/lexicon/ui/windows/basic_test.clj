(ns lexicon.ui.windows.basic-test
  "E2E tests for window management - tests USER window operations.

  Tests window splitting, deletion, and navigation via keyboard:
  - C-x 2 = split-window-vertically
  - C-x 3 = split-window-horizontally
  - C-x 0 = delete-window
  - C-x 1 = delete-other-windows
  - C-x o = other-window

  Uses keyboard simulation for all window operations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Window Splitting via Keyboard
;; =============================================================================

(deftest test-split-window-horizontally-keyboard
  (testing "C-x 3 creates horizontal split"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)
    ;; User types text first
    (h/type-text "Hello World")
    (Thread/sleep 50)

    (let [count-before (h/get-window-count*)]
      ;; C-x 3 splits horizontally
      (h/press-ctrl-x "3")
      (Thread/sleep 100)

      (let [count-after (h/get-window-count*)]
        (is (> count-after count-before)
            "C-x 3 should create new window")))))

(deftest test-split-window-vertically-keyboard
  (testing "C-x 2 creates vertical split"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)

    (let [count-before (h/get-window-count*)]
      ;; C-x 2 splits vertically
      (h/press-ctrl-x "2")
      (Thread/sleep 100)

      (let [count-after (h/get-window-count*)]
        (is (> count-after count-before)
            "C-x 2 should create new window")))))

;; =============================================================================
;; Window Deletion via Keyboard
;; =============================================================================

(deftest test-delete-current-window-keyboard
  (testing "C-x 0 deletes current window"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)
    ;; Create a split first
    (h/press-ctrl-x "2")
    (Thread/sleep 100)

    (let [count-before (h/get-window-count*)]
      (is (> count-before 1) "Should have multiple windows")

      ;; C-x 0 deletes current window
      (h/press-ctrl-x "0")
      (Thread/sleep 100)

      (let [count-after (h/get-window-count*)]
        (is (= 1 count-after)
            "C-x 0 should return to single window")))))

(deftest test-delete-other-windows-keyboard
  (testing "C-x 1 deletes all other windows"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)
    ;; Create multiple splits
    (h/press-ctrl-x "2")
    (Thread/sleep 50)
    (h/press-ctrl-x "3")
    (Thread/sleep 100)

    (let [count-before (h/get-window-count*)]
      (is (> count-before 1) "Should have multiple windows")

      ;; C-x 1 deletes other windows
      (h/press-ctrl-x "1")
      (Thread/sleep 100)

      (let [count-after (h/get-window-count*)]
        (is (= 1 count-after)
            "C-x 1 should leave only one window")))))

;; =============================================================================
;; Window Navigation via Keyboard
;; =============================================================================

(deftest test-other-window-keyboard
  (testing "C-x o cycles through windows"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)

    (let [win1 (h/get-selected-window-id*)]
      ;; Create split
      (h/press-ctrl-x "2")
      (Thread/sleep 100)

      ;; C-x o should switch to other window
      (h/press-ctrl-x "o")
      (Thread/sleep 100)

      ;; Should be back at original window (only 2 windows, so cycle back)
      (is (= win1 (h/get-selected-window-id*))
          "C-x o should cycle through windows"))))

;; =============================================================================
;; Independent Point per Window
;; =============================================================================

(deftest test-windows-have-independent-point-keyboard
  (testing "Each window tracks point independently during typing"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)
    ;; User types text
    (h/type-text "Hello World")
    (Thread/sleep 50)

    ;; Go to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    (let [pt1 (h/get-point*)]
      ;; Split window
      (h/press-ctrl-x "2")
      (Thread/sleep 100)

      ;; Move point in new window
      (h/press-ctrl "e")  ; Go to end
      (Thread/sleep 50)

      ;; Switch back to original window
      (h/press-ctrl-x "o")
      (Thread/sleep 100)

      ;; Point should still be where we left it
      (is (= pt1 (h/get-point*))
          "Window point should be independent"))))

;; =============================================================================
;; Typing in Different Windows
;; =============================================================================

(deftest test-typing-in-split-window
  (testing "User can type in split window"
    (h/setup-test*)
    (h/clear-buffer)
    ;; Ensure single window to start
    (h/press-ctrl-x "1")
    (Thread/sleep 100)
    ;; Type in first window
    (h/type-text "Window 1")
    (Thread/sleep 50)

    ;; Split
    (h/press-ctrl-x "2")
    (Thread/sleep 100)

    ;; Type in second window (same buffer, different position)
    (h/press-ctrl "e")  ; Go to end
    (Thread/sleep 30)
    (h/type-text " - Window 2")
    (Thread/sleep 100)

    (is (= "Window 1 - Window 2" (h/get-buffer-text*))
        "Typing in split window should work")))
