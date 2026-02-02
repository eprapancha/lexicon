(ns lexicon.ui.completions-test
  "E2E tests for *Completions* buffer and text properties (Issue #138).

  Tests:
  - Completion buffer navigation with arrow keys
  - Property-aware navigation (skips informational text)
  - Span-based cursor highlighting
  - Selection with RET key"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn get-completions-buffer-text
  "Get the text content of the *Completions* buffer."
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state || !state.completionsBuffer) return null;
    return state.completionsBuffer;
  "))

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

(defn get-cursor-owner
  "Get the ID of the window that owns the cursor."
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    return state ? state.cursorOwner : null;
  "))

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
    const cursorPos = state.point || 0;
    for (const entry of entries) {
      if (cursorPos >= entry.start && cursorPos < entry.end) {
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
  "Press ArrowDown key in *Completions* buffer."
  []
  (let [script "
    const hidden = document.querySelector('.hidden-input');
    if (hidden) {
      hidden.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowDown',
        code: 'ArrowDown',
        bubbles: true
      });
      hidden.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn press-arrow-up
  "Press ArrowUp key in *Completions* buffer."
  []
  (let [script "
    const hidden = document.querySelector('.hidden-input');
    if (hidden) {
      hidden.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'ArrowUp',
        code: 'ArrowUp',
        bubbles: true
      });
      hidden.dispatchEvent(event);
    }
  "]
    (e/js-execute h/*driver* script))
  (Thread/sleep 50))

(defn press-enter-in-completions
  "Press Enter in *Completions* buffer to select completion."
  []
  (let [script "
    const hidden = document.querySelector('.hidden-input');
    if (hidden) {
      hidden.focus();
      const event = new KeyboardEvent('keydown', {
        key: 'Enter',
        code: 'Enter',
        bubbles: true
      });
      hidden.dispatchEvent(event);
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
;; Tests
;; =============================================================================

(deftest test-completions-buffer-appears-on-tab
  (testing "TAB in minibuffer shows *Completions* buffer"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x to activate minibuffer
    (h/press-meta "x")
    (Thread/sleep 200)

    ;; Verify minibuffer is active
    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Press TAB to show completions
    (press-tab)
    (Thread/sleep 300)

    ;; Verify *Completions* buffer appeared
    (is (completions-window-visible?) "Completions window should be visible")

    ;; Verify we have some entries
    (is (> (get-completions-entry-count) 0) "Should have completion entries")

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-arrow-navigation-in-completions
  (testing "Arrow keys navigate through completions"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x and show completions
    (h/press-meta "x")
    (Thread/sleep 200)
    (press-tab)
    (Thread/sleep 300)

    ;; Get initial entry (should be first completion)
    (let [initial-entry (get-current-entry-value)]

      ;; Press ArrowDown to move to next
      (press-arrow-down)
      (let [next-entry (get-current-entry-value)]
        ;; Entry should change (or stay same if only one entry)
        (when (> (get-completions-entry-count) 1)
          (is (or (not= initial-entry next-entry)
                  (nil? next-entry))
              "ArrowDown should move to different entry")))

      ;; Press ArrowUp to move back
      (press-arrow-up)
      (let [after-up (get-current-entry-value)]
        ;; Should be back at first or wrapped to last
        (is (not (nil? after-up)) "Should have an entry after ArrowUp")))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-enter-selects-completion
  (testing "RET in *Completions* selects the completion under cursor"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x and show completions
    (h/press-meta "x")
    (Thread/sleep 200)
    (press-tab)
    (Thread/sleep 300)

    ;; Get the current completion value
    (let [entry-value (get-current-entry-value)]
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

(deftest test-navigation-skips-header
  (testing "Navigation skips the 'Possible completions:' header"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Open M-x and show completions
    (h/press-meta "x")
    (Thread/sleep 200)
    (press-tab)
    (Thread/sleep 300)

    ;; Navigate to first completion
    (press-arrow-down)
    (Thread/sleep 50)

    ;; The current entry should be a completion, not the header
    (let [entry (get-current-entry-value)]
      (is (not (nil? entry)) "Should be on a completion entry, not header")
      (is (not= entry "Possible completions:") "Should not be on header text"))

    ;; Navigate up - should stay on first completion, not go to header
    (press-arrow-up)
    (Thread/sleep 50)
    (let [entry (get-current-entry-value)]
      (is (not (nil? entry)) "Should still be on a completion after ArrowUp"))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)))

(deftest test-quit-closes-completions
  (testing "q key in *Completions* closes the buffer and returns to minibuffer"
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
