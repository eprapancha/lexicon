(ns lexicon.semantic.window-test
  "Semantic tests for window management.

  Emacs source: lisp/window.el (8,896 LOC), src/window.c (112 DEFUNs)
  Status: 60% implemented

  Key features:
  - Window splitting (C-x 2, C-x 3)
  - Window deletion (C-x 0, C-x 1)
  - Independent point per window
  - Window configuration

  Related: Issue #110 (Window Management), Issue #94 (TDD)
  Priority: HIGH"
  (:require [clojure.test :refer [deftest is testing]]
            [lexicon.test-helpers :as helpers])
  (:require-macros [lexicon.semantic.helpers :refer [with-test-buffer]]))

;;; =============================================================================
;;; Window Splitting
;;; =============================================================================

(deftest ^:critical split-window-horizontally
  "CRITICAL: C-x 3 splits window side by side.

  Emacs Semantics (window.c):
  - Creates new window to the right
  - Both windows show same buffer initially
  - Each has independent point

  Why this matters:
  - Multi-buffer workflows"
  (testing "split creates two windows"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (let [win1 (helpers/current-window)]
        (helpers/split-window-horizontally)
        (let [win2 (helpers/current-window)]
          (is (not= win1 win2)
              "New window created"))))))

(deftest ^:critical split-window-vertically
  "CRITICAL: C-x 2 splits window top/bottom.

  Emacs Semantics:
  - Creates new window below
  - Same buffer in both
  - Independent point

  Why this matters:
  - Compare file sections"
  (testing "vertical split creates two windows"
    (with-test-buffer "*test*"
      (helpers/split-window-vertically)
      (is (> (helpers/window-count) 1)
          "Should have multiple windows"))))

;;; =============================================================================
;;; Window Deletion
;;; =============================================================================

(deftest ^:critical delete-current-window
  "CRITICAL: C-x 0 deletes current window.

  Emacs Semantics:
  - Removes current window
  - Buffer not killed
  - Adjacent window expands

  Why this matters:
  - Clean up split views"
  (testing "delete-window removes current"
    (with-test-buffer "*test*"
      (helpers/split-window-horizontally)
      (is (> (helpers/window-count) 1))

      (helpers/delete-window)

      (is (= 1 (helpers/window-count))
          "Should return to single window"))))

(deftest ^:critical delete-other-windows
  "CRITICAL: C-x 1 deletes all but current window.

  Emacs Semantics:
  - All other windows deleted
  - Current window fills frame
  - Other buffers not killed

  Why this matters:
  - Focus on single buffer"
  (testing "delete-other-windows leaves one"
    (with-test-buffer "*test*"
      (helpers/split-window-horizontally)
      (helpers/split-window-horizontally)

      (helpers/delete-other-windows)

      (is (= 1 (helpers/window-count))
          "Should have only one window"))))

;;; =============================================================================
;;; Independent Point
;;; =============================================================================

(deftest ^:critical windows-have-independent-point
  "CRITICAL: Each window has its own point position.

  Emacs Semantics:
  - Same buffer in two windows
  - Different point in each
  - Switching windows preserves positions

  Why this matters:
  - Compare different file locations"
  (testing "windows track point independently"
    (with-test-buffer "*test*"
      (helpers/insert "Hello World")
      (helpers/goto-char 0)

      (let [win1 (helpers/current-window)]
        (helpers/split-window-horizontally)
        (let [win2 (helpers/current-window)]
          ;; Move point in window 2
          (helpers/goto-char 6)

          ;; Switch to window 1
          (helpers/select-window win1)

          ;; Point should still be at 0
          (is (= 0 (helpers/point))
              "Window 1 point unchanged"))))))

;;; =============================================================================
;;; Window Selection
;;; =============================================================================

(deftest ^:high other-window-cycles
  "HIGH: C-x o cycles through windows.

  Emacs Semantics:
  - Moves to next window
  - Wraps around at end

  Why this matters:
  - Navigate between splits"
  (testing "other-window moves to next"
    (with-test-buffer "*test*"
      (let [win1 (helpers/current-window)]
        (helpers/split-window-horizontally)

        (helpers/other-window)

        (is (= win1 (helpers/current-window))
            "Should cycle back to first window")))))

;;; =============================================================================
;;; Window Configuration
;;; =============================================================================

(deftest ^:medium window-configuration-saveable
  "MEDIUM: Window layout can be saved and restored.

  Emacs Semantics:
  - current-window-configuration saves layout
  - set-window-configuration restores it

  Why this matters:
  - Perspectives, workspace management"
  (testing "configuration can be captured"
    (with-test-buffer "*test*"
      (let [config (helpers/current-window-configuration)]
        (is (some? config)
            "Configuration should be returned")))))
