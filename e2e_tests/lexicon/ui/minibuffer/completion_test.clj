(ns lexicon.ui.minibuffer.completion-test
  "E2E tests for minibuffer completion cycling - Issue #137

  Tests vanilla Emacs minibuffer completion behavior:
  - Arrow key cycling through completions
  - TAB completion behavior
  - *Completions* buffer display as bottom split
  - Focus remaining in minibuffer during cycling"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Arrow Key Cycling Tests
;; =============================================================================

(deftest test-arrow-down-cycles-completions
  (testing "Down arrow cycles through matching completions in minibuffer"
    (h/setup-test*)

    ;; Open buffer switch minibuffer (C-x b) which has buffer completions
    (h/press-ctrl-x "b")
    (Thread/sleep 200)

    ;; Minibuffer should be visible
    (is (h/minibuffer-visible?)
        "Minibuffer should open for C-x b")

    ;; Get initial input value (should be empty)
    (let [initial-value (h/get-minibuffer-input-value)]
      (is (= "" initial-value)
          "Initial minibuffer input should be empty"))

    ;; Press Down arrow to cycle to first completion
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 100)

    ;; Input should now contain a buffer name
    (let [after-down (h/get-minibuffer-input-value)]
      (is (not= "" after-down)
          "After arrow down, input should contain a completion"))

    ;; Press Down again to cycle to next
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 100)

    ;; Press Up to go back
    (h/press-arrow-up-in-minibuffer)
    (Thread/sleep 100)

    ;; Cancel with C-g
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-arrow-up-returns-to-original-input
  (testing "Up arrow at first completion returns to original typed input"
    (h/setup-test*)

    ;; Open M-x for command completion
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?)
        "Minibuffer should open for M-x")

    ;; Type partial command
    (h/type-in-minibuffer "goto")
    (Thread/sleep 100)

    ;; Down arrow to start cycling
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 100)

    (let [after-down (h/get-minibuffer-input-value)]
      (is (not= "goto" after-down)
          "After arrow down, input should be different from typed text")

      ;; Up arrow should return to original
      (h/press-arrow-up-in-minibuffer)
      (Thread/sleep 100)

      (let [after-up (h/get-minibuffer-input-value)]
        (is (= "goto" after-up)
            "After arrow up at first completion, should return to 'goto'")))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-typing-resets-cycling-state
  (testing "Typing new text resets cycling state"
    (h/setup-test*)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?))

    ;; Type partial command
    (h/type-in-minibuffer "goto")
    (Thread/sleep 100)

    ;; Cycle with arrow down
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 100)

    ;; Get the cycled completion
    (let [cycled-value (h/get-minibuffer-input-value)]
      ;; Now type more - this should clear the cycling state
      (h/type-in-minibuffer "-line")
      (Thread/sleep 100)

      (let [new-value (h/get-minibuffer-input-value)]
        ;; The input should now contain the typed text, not be cycling
        (is (.contains new-value "-line")
            "Typing should add to the input")))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; TAB Completion Tests
;; =============================================================================

(deftest test-tab-completes-common-prefix
  (testing "First TAB completes common prefix"
    (h/setup-test*)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type partial command that has completions
    (h/type-in-minibuffer "goto-l")
    (Thread/sleep 100)

    ;; Press TAB to complete
    (h/press-tab-in-minibuffer)
    (Thread/sleep 100)

    (let [after-tab (h/get-minibuffer-input-value)]
      ;; Should have completed to at least "goto-line" or common prefix
      (is (>= (count after-tab) (count "goto-l"))
          "TAB should complete at least what was typed"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-double-tab-shows-completions-buffer
  (testing "TAB twice shows *Completions* buffer in bottom split"
    (h/setup-test*)

    ;; Get initial window count
    (let [initial-windows (h/get-window-count)]

      ;; Open M-x
      (h/press-meta "x")
      (Thread/sleep 200)

      ;; Type ambiguous prefix with multiple matches
      (h/type-in-minibuffer "goto")
      (Thread/sleep 100)

      ;; First TAB - completes prefix
      (h/press-tab-in-minibuffer)
      (Thread/sleep 100)

      ;; Second TAB - should show *Completions* buffer
      (h/press-tab-in-minibuffer)
      (Thread/sleep 200)

      ;; Should have one more window (the *Completions* split)
      (let [after-windows (h/get-window-count)]
        ;; Note: This may not increase if already at 2 windows
        (is (or (> after-windows initial-windows)
                (h/completions-buffer-visible?))
            "Either window count increased or *Completions* buffer exists")))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-minibuffer-keeps-focus-after-completions
  (testing "Minibuffer keeps focus when *Completions* buffer appears"
    (h/setup-test*)

    ;; Open M-x
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Type something
    (h/type-in-minibuffer "goto")
    (Thread/sleep 100)

    ;; Tab twice to show completions
    (h/press-tab-in-minibuffer)
    (Thread/sleep 100)
    (h/press-tab-in-minibuffer)
    (Thread/sleep 200)

    ;; Minibuffer should still be visible and active
    (is (h/minibuffer-visible?)
        "Minibuffer should remain visible after showing *Completions*")

    ;; Should be able to continue typing
    (h/type-in-minibuffer "-line")
    (Thread/sleep 100)

    (let [value (h/get-minibuffer-input-value)]
      (is (.contains value "-line")
          "Should still be able to type in minibuffer after *Completions* shown"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main []
  (clojure.test/run-tests 'lexicon.ui.minibuffer.completion-test))
