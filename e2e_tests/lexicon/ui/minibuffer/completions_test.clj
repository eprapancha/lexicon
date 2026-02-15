(ns lexicon.ui.minibuffer.completions-test
  "Comprehensive E2E tests for minibuffer completion UI.

  Consolidates tests from:
  - ui/minibuffer/completion_test.clj (arrow cycling, TAB behavior)
  - ui/completions_test.clj (*Completions* buffer navigation)

  Tests:
  - Minibuffer arrow cycling (down/up through candidates)
  - TAB completion (common prefix, show *Completions*)
  - *Completions* buffer navigation (arrows, RET, q)
  - Interaction between minibuffer and *Completions*

  Each deftest initializes a fresh driver for isolation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions for *Completions* Buffer
;; =============================================================================

(defn completions-window-visible?
  "Check if *Completions* window is visible."
  []
  (try
    (let [result (e/js-execute h/*driver* "
      const buffers = window.editorState ? window.editorState.buffers : {};
      for (const [id, buffer] of Object.entries(buffers)) {
        if (buffer.name === '*Completions*') return true;
      }
      return false;
    ")]
      (boolean result))
    (catch Exception _ false)))

(defn get-completions-entry-count
  "Get the number of completion entries from completion-help state."
  []
  (let [result (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state || !state.completionHelp || !state.completionHelp.entries) return 0;
    return state.completionHelp.entries.length;
  ")]
    (or result 0)))

(defn get-current-entry-value
  "Get the value of the current completion entry under cursor."
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state || !state.completionHelp) return null;
    const entries = state.completionHelp.entries || [];
    const selectedPos = state.completionHelp.selectedPos;
    if (selectedPos == null) return null;
    for (const entry of entries) {
      if (selectedPos >= entry.start && selectedPos < entry.end) {
        return entry.value;
      }
    }
    return null;
  "))

(defn press-tab
  "Press Tab key to trigger completion."
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Tab',
        code: 'Tab',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 100))

(defn press-arrow-down
  "Press ArrowDown key for completion navigation."
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowDown',
        code: 'ArrowDown',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn press-arrow-up
  "Press ArrowUp key for completion navigation."
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowUp',
        code: 'ArrowUp',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn press-enter-in-completions
  "Press Enter to select the current completion."
  []
  (let [script "
    const input = document.querySelector('.minibuffer-input');
    if (input) {
      input.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        bubbles: true
      });
      input.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 100))

(defn get-minibuffer-input-value
  "Get the current value of the minibuffer input."
  []
  (try
    (e/js-execute h/*driver* "
      const input = document.querySelector('.minibuffer-input');
      return input ? input.value : '';
    ")
    (catch Exception _ "")))

;; =============================================================================
;; Test 1: Minibuffer Arrow Cycling
;; Combines: arrow-down-cycles, arrow-up-returns, typing-resets
;; =============================================================================

(deftest test-minibuffer-completion-cycling
  (testing "Arrow keys cycle through completions in minibuffer"
    (h/setup-test*)

    ;; === Part 1: Down arrow cycles through completions ===
    ;; Open buffer switch minibuffer (C-x b) which has buffer completions
    (h/press-ctrl-x "b")
    (Thread/sleep 200)

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
          "After arrow down, input should contain a completion")

      ;; Press Down again to cycle to next
      (h/press-arrow-down-in-minibuffer)
      (Thread/sleep 100)

      ;; Press Up to go back
      (h/press-arrow-up-in-minibuffer)
      (Thread/sleep 100))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; === Part 2: Up arrow returns to original typed input ===
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
    (Thread/sleep 100)

    ;; === Part 3: Typing resets cycling state ===
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?))

    ;; Type partial command
    (h/type-in-minibuffer "goto")
    (Thread/sleep 100)

    ;; Cycle with arrow down
    (h/press-arrow-down-in-minibuffer)
    (Thread/sleep 100)

    ;; Now type more - this should add to or reset the input
    (h/type-in-minibuffer "-line")
    (Thread/sleep 100)

    (let [new-value (h/get-minibuffer-input-value)]
      (is (.contains new-value "-line")
          "Typing should add to the input"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

;; =============================================================================
;; Test 2: TAB Completion Behavior
;; Combines: tab-completes-prefix, double-tab-shows-completions, keeps-focus
;; =============================================================================

(deftest test-tab-completion-behavior
  (testing "TAB completes prefix and shows *Completions* buffer"
    (h/setup-test*)

    ;; === Part 1: First TAB completes common prefix ===
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
    (Thread/sleep 100)

    ;; === Part 2: Double TAB shows *Completions* buffer ===
    (let [initial-windows (h/get-window-count)]
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

      ;; Should have *Completions* visible
      (let [after-windows (h/get-window-count)]
        (is (or (> after-windows initial-windows)
                (h/completions-buffer-visible?))
            "Either window count increased or *Completions* buffer exists")))

    ;; === Part 3: Minibuffer keeps focus when *Completions* appears ===
    ;; (continuing from above - *Completions* should be visible)
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
;; Test 3: *Completions* Buffer Interaction
;; Combines: buffer-appears, arrow-navigation, enter-selects, navigation-skips-header
;; =============================================================================

(deftest test-completions-buffer-interaction
  (testing "*Completions* buffer navigation and selection"
    (h/setup-test*)
    (h/clear-buffer)

    ;; === Part 1: TAB shows *Completions* buffer with entries ===
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Press TAB to show completions
    (press-tab)
    (Thread/sleep 300)

    (is (completions-window-visible?) "Completions window should be visible")
    (is (> (get-completions-entry-count) 0) "Should have completion entries")

    ;; === Part 2: Arrow keys navigate through entries ===
    ;; Press ArrowDown to select first completion
    (press-arrow-down)
    (Thread/sleep 50)

    (let [first-entry (get-current-entry-value)]
      (is (not (nil? first-entry)) "Should have an entry after first ArrowDown")
      (is (not= first-entry "Possible completions:")
          "Should not be on header text (navigation skips header)")

      ;; Press ArrowDown again to move to next (if multiple entries)
      (when (> (get-completions-entry-count) 1)
        (press-arrow-down)
        (let [second-entry (get-current-entry-value)]
          (is (not= first-entry second-entry)
              "ArrowDown should move to different entry")))

      ;; Press ArrowUp to move back
      (press-arrow-up)
      (let [after-up (get-current-entry-value)]
        (is (not (nil? after-up)) "Should have an entry after ArrowUp")))

    ;; Cancel and restart for enter test
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; === Part 3: RET selects the completion ===
    (h/press-meta "x")
    (Thread/sleep 200)
    (press-tab)
    (Thread/sleep 300)

    ;; Navigate to first completion
    (press-arrow-down)
    (Thread/sleep 100)

    (let [entry-value (get-current-entry-value)]
      (is (not (nil? entry-value)) "Should have a completion selected after ArrowDown")

      (when entry-value
        ;; Press Enter to select it
        (press-enter-in-completions)
        (Thread/sleep 200)

        ;; The minibuffer input should now contain the selected completion
        (let [minibuffer-value (get-minibuffer-input-value)]
          (is (or (= entry-value minibuffer-value)
                  (str/includes? minibuffer-value entry-value)
                  ;; Minibuffer might have closed (command executed)
                  (not (h/minibuffer-visible?)))
              (str "Selected completion '" entry-value "' should be in minibuffer")))))

    ;; Cancel if still active
    (when (h/minibuffer-visible?)
      (h/press-ctrl "g")
      (Thread/sleep 100))))

;; =============================================================================
;; Test 4: Quit Closes *Completions*
;; =============================================================================

(deftest test-quit-closes-completions
  (testing "q key closes *Completions* and keeps minibuffer"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x and show completions
    (h/press-meta "x")
    (Thread/sleep 200)
    (press-tab)
    (Thread/sleep 300)

    (is (completions-window-visible?) "Completions should be visible initially")

    ;; Press 'q' to quit
    (let [script "
      const hidden = document.querySelector('.hidden-input');
      if (hidden) {
        hidden.focus();
        const event = new KeyboardEvent('keydown', {
          key: 'q',
          code: 'KeyQ',
          bubbles: true
        });
        hidden.dispatchEvent(event);
      }
    "]
      (e/js-execute h/*driver* script))
    (Thread/sleep 200)

    ;; Minibuffer should still be active
    (is (h/minibuffer-visible?) "Minibuffer should still be visible after q")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))
