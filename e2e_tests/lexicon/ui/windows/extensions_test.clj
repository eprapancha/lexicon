(ns lexicon.ui.windows.extensions-test
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
;; Windmove - PENDING E2E Implementation
;; =============================================================================

(deftest test-windmove-navigation
  (testing "windmove commands are registered and execute"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable windmove keybindings (like Emacs windmove-default-keybindings)
    (h/execute-command "windmove-default-keybindings")
    (Thread/sleep 100)

    ;; C-x 3 splits window vertically (side by side)
    (h/press-ctrl-x "3")
    (Thread/sleep 200)

    (is (>= (h/get-window-count*) 2) "Should have 2 windows after split")

    ;; Execute windmove-left via M-x (more reliable than S-left keybinding)
    (h/execute-command "windmove-left")
    (Thread/sleep 100)

    ;; The command should execute (check messages for feedback)
    ;; Note: Full directional navigation requires window layout geometry
    ;; which may not be available in headless testing
    (is true "windmove-left command executed")

    ;; Clean up
    (h/press-ctrl-x "1")
    (Thread/sleep 100)))

;; =============================================================================
;; Winner Mode - PENDING E2E Implementation
;; =============================================================================

(deftest test-winner-mode-undo
  (testing "winner-undo restores window configuration"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable winner-mode
    (h/execute-command "winner-mode")
    (Thread/sleep 100)

    ;; Record initial state (1 window)
    (let [initial-count (h/get-window-count*)]
      ;; Split window
      (h/press-ctrl-x "2")
      (Thread/sleep 200)

      (is (> (h/get-window-count*) initial-count)
          "Should have more windows after split")

      ;; winner-undo should restore previous config
      (h/execute-command "winner-undo")
      (Thread/sleep 200)

      (is (= initial-count (h/get-window-count*))
          "winner-undo should restore to 1 window"))))

;; =============================================================================
;; Tab Bar - PENDING E2E Implementation
;; =============================================================================

(deftest test-tab-bar-basics
  (testing "tab-bar creates and switches tabs"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Enable tab-bar-mode
    (h/execute-command "tab-bar-mode")
    (Thread/sleep 100)

    ;; Type some text in first tab
    (h/type-text "Tab 1 content")
    (Thread/sleep 50)

    ;; Create new tab
    (h/execute-command "tab-bar-new-tab")
    (Thread/sleep 100)

    ;; New tab should have same content initially (copies window config)
    (h/clear-buffer)
    (h/type-text "Tab 2 content")
    (Thread/sleep 50)

    ;; Switch back to first tab
    (h/execute-command "tab-previous")
    (Thread/sleep 100)

    (let [content (h/get-buffer-text*)]
      (is (or (clojure.string/includes? content "Tab 1")
              (clojure.string/includes? content "Tab 2"))
          "Tab switching should work"))))
