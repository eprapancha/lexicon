(ns lexicon.regression-test
  "Regression tests for critical workflows and past bugs.

  These tests ensure that bugs we've fixed stay fixed."
  (:require [cljs.test :refer [deftest is testing run-tests use-fixtures async]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]  ; MUST load first - initializes WASM
            [lexicon.events]    ; Register re-frame event handlers
            [lexicon.subs]      ; Register re-frame subscriptions
            [lexicon.test-events]))    ; Register test-specific event handlers

;; Wait for WASM before running tests
(use-fixtures :once
  {:before (fn []
             (async done
               (-> setup/wasm-load-promise
                   (.then (fn [_]
                            (.log js/console "✅ WASM ready for regression-test")
                            (done)))
                   (.catch (fn [e]
                             (.error js/console "❌ WASM failed in regression-test:" e)
                             (done))))))})

;; -- Test Helpers --

(defn reset-db!
  "Reset re-frame database to initial state for testing."
  []
  ;; Preserve WASM constructor when resetting, but NOT wasm-instance
  ;; Each buffer needs its own fresh WASM instance to avoid Rust borrow checker errors
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])]
    (reset! rfdb/app-db db/default-db)
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))))

(defn create-test-buffer
  "Create a test buffer with CONTENT and return buffer-id."
  [content]
  (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
        _ (when-not WasmGapBuffer
            (.error js/console "❌ No WASM constructor found in db"))
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer content))]
    (when wasm-instance
      (rf/dispatch-sync [:create-buffer "test-buffer" wasm-instance])
      ;; Get the actual buffer-id that was created (not a random one!)
      (first (keys (get-in @rfdb/app-db [:buffers]))))))

(defn get-buffer-text
  "Get text content of BUFFER-ID."
  [buffer-id]
  (let [db @rfdb/app-db
        buffer (get-in db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when wasm-instance
      (try
        (.getText ^js wasm-instance)
        (catch js/Error _ nil)))))

;; -- Phase 0: Basic Text Input Regression --

(deftest test-basic-typing-works
  (testing "REGRESSION: Basic typing should work (Phase 0 fix)"
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Type "Hello"
      (rf/dispatch-sync [:self-insert-command "H"])
      (rf/dispatch-sync [:self-insert-command "e"])
      (rf/dispatch-sync [:self-insert-command "l"])
      (rf/dispatch-sync [:self-insert-command "l"])
      (rf/dispatch-sync [:self-insert-command "o"])

      (is (= "Hello" (get-buffer-text buffer-id))
          "Typing characters should work"))))

(deftest test-newline-works
  (testing "REGRESSION: Enter key creates newline (Phase 0 fix)"
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])

      (rf/dispatch-sync [:newline])

      (is (= "Hello\n" (get-buffer-text buffer-id))
          "Newline should be inserted"))))

(deftest test-backspace-works
  (testing "REGRESSION: Backspace deletes character (Phase 0 fix)"
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])

      (rf/dispatch-sync [:delete-backward-char])

      (is (= "Hell" (get-buffer-text buffer-id))
          "Backspace should delete character before point"))))

;; -- Phase 3: Window Tree Cursor Bug --

(deftest test-window-tree-cursor-update
  (testing "REGRESSION: Cursor position updates correctly in window tree (Phase 3 fix)"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Line 1\nLine 2\nLine 3")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Split window
      (rf/dispatch-sync [:split-window-horizontally])

      ;; Move cursor in active window
      (rf/dispatch-sync [:buffer/goto-char buffer-id 7])  ; Start of Line 2

      ;; Cursor update should not crash or corrupt state
      (is (some? (get-in @rfdb/app-db [:editor :active-window-id]))
          "Active window should still exist")

      ;; Switch windows
      (rf/dispatch-sync [:other-window])

      ;; Should still work
      (is (some? (get-in @rfdb/app-db [:editor :active-window-id]))
          "Can switch windows without crash"))))

;; -- UTF-8 and Multi-byte Characters --

(deftest test-emoji-support
  (testing "REGRESSION: UTF-8 emoji characters work correctly"
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Insert emoji
      (rf/dispatch-sync [:self-insert-command "👍"])
      (rf/dispatch-sync [:self-insert-command "🎉"])

      (is (= "👍🎉" (get-buffer-text buffer-id))
          "Emoji characters should be handled correctly"))))

(deftest test-multibyte-cursor-movement
  (testing "REGRESSION: Cursor movement with multi-byte characters"
    (let [buffer-id (create-test-buffer "Hello 世界")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])

      ;; Move forward through multi-byte characters
      (dotimes [_ 8]
        (rf/dispatch-sync [:forward-char]))

      ;; Should not crash or corrupt state
      (is (some? (get-in @rfdb/app-db [:buffers buffer-id]))
          "Buffer should still exist after moving through multi-byte chars"))))

;; -- Empty Lines Rendering --

(deftest test-empty-lines-render
  (testing "REGRESSION: Empty lines should render correctly (Phase 0 fix)"
    (let [buffer-id (create-test-buffer "Line 1\n\nLine 3")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Verify content
      (is (= "Line 1\n\nLine 3" (get-buffer-text buffer-id))
          "Empty line should be preserved in buffer text")

      ;; Move to empty line
      (rf/dispatch-sync [:buffer/goto-char buffer-id 7])  ; Empty line

      ;; Should not crash
      (rf/dispatch-sync [:forward-char])
      (is (some? (get-in @rfdb/app-db [:buffers buffer-id]))
          "Can navigate empty lines"))))

;; -- Kill Ring and Region --

(deftest test-kill-yank-workflow
  (testing "REGRESSION: Complete kill-yank workflow works"
    (let [buffer-id (create-test-buffer "Hello World")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Mark at 0, point at 5
      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])
      (rf/dispatch-sync [:set-mark-command])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])

      ;; Kill region
      (rf/dispatch-sync [:kill-region])
      (is (= " World" (get-buffer-text buffer-id))
          "Text should be killed")

      ;; Move to end
      (rf/dispatch-sync [:buffer/goto-char buffer-id 6])

      ;; Yank
      (rf/dispatch-sync [:yank])
      (is (= " WorldHello" (get-buffer-text buffer-id))
          "Text should be yanked at new position"))))

;; -- Undo/Redo Chain --

(deftest test-undo-redo-chain
  (testing "REGRESSION: Complex undo/redo sequence works"
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Insert "Hello"
      (rf/dispatch-sync [:buffer/insert buffer-id 0 "Hello"])
      (is (= "Hello" (get-buffer-text buffer-id)))

      ;; Insert " World"
      (rf/dispatch-sync [:buffer/insert buffer-id 5 " World"])
      (is (= "Hello World" (get-buffer-text buffer-id)))

      ;; Undo last insert
      (rf/dispatch-sync [:undo])
      (is (= "Hello" (get-buffer-text buffer-id))
          "Last insert should be undone")

      ;; Undo first insert
      (rf/dispatch-sync [:undo])
      (is (= "" (get-buffer-text buffer-id))
          "First insert should be undone"))))

;; -- Minibuffer Completion --

(deftest test-minibuffer-completion
  (testing "REGRESSION: Minibuffer completion works"
    (reset-db!)
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Activate minibuffer with completions
      (rf/dispatch-sync [:minibuffer/activate
                        {:prompt "Test: "
                         :completions ["foo" "bar" "baz"]
                         :on-confirm [:test-command]}])

      (let [minibuffer (get-in @rfdb/app-db [:minibuffer])]
        (is (:active? minibuffer) "Minibuffer should be active")
        (is (= ["foo" "bar" "baz"] (:completions minibuffer))
            "Completions should be stored"))

      ;; Complete with TAB
      (rf/dispatch-sync [:minibuffer/complete])

      ;; Should not crash
      (is (some? (get-in @rfdb/app-db [:minibuffer]))
          "Minibuffer state should exist after completion"))))

;; -- Run all tests --

(defn ^:export run-all-tests []
  (run-tests 'lexicon.regression-test))
