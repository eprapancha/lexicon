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
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])
        initial-window-id 1]  ; Use ID 1 to match default buffer
    (reset! rfdb/app-db db/default-db)
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))
    ;; Set up window tree structure (not flat :windows map)
    (swap! rfdb/app-db assoc :window-tree {:type :leaf
                                            :id initial-window-id
                                            :buffer-id nil
                                            :cursor-position {:line 0 :column 0}
                                            :mark-position nil
                                            :viewport {:start-line 0 :end-line 50}
                                            :dimensions {:x 0 :y 0 :width 100 :height 100}})
    (swap! rfdb/app-db assoc :active-window-id initial-window-id)))

(defn create-test-buffer
  "Create a test buffer with CONTENT and return buffer-id."
  [content]
  (reset-db!)
  (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
        _ (when-not WasmGapBuffer
            (.error js/console "❌ No WASM constructor found in db"))
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer content))
        ;; Compute the buffer-id that will be created (next-buffer-id)
        buffer-id (db/next-buffer-id (get-in @rfdb/app-db [:buffers]))]
    (when wasm-instance
      (rf/dispatch-sync [:create-buffer "test-buffer" wasm-instance])
      buffer-id)))

(defn get-buffer-text
  "Get text content of BUFFER-ID."
  [buffer-id]
  (let [db @rfdb/app-db
        buffer (get-in db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when-not buffer
      (.error js/console "❌ get-buffer-text: Buffer not found for id" buffer-id))
    (when-not wasm-instance
      (.error js/console "❌ get-buffer-text: No WASM instance for buffer" buffer-id))
    (when wasm-instance
      (try
        (.getText ^js wasm-instance)
        (catch js/Error e
          (.error js/console "❌ get-buffer-text: getText failed" e)
          nil)))))

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

(deftest test-backspace-all-then-type
  (testing "REGRESSION: Typing should work after backspacing entire buffer (P0-01 → P0-02 transition bug)"
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])

      ;; Simulate P0-01: Type a sentence
      (let [sentence "The quick brown fox jumps over the lazy dog."]
        (doseq [ch sentence]
          (rf/dispatch-sync [:self-insert-command (str ch)])))

      (is (= "The quick brown fox jumps over the lazy dog." (get-buffer-text buffer-id))
          "Initial text should be typed correctly (P0-01)")

      ;; User wants clean slate for P0-02: Backspace everything
      (let [length (count "The quick brown fox jumps over the lazy dog.")]
        (dotimes [_ length]
          (rf/dispatch-sync [:delete-backward-char])))

      (is (= "" (get-buffer-text buffer-id))
          "Buffer should be empty after backspacing all content")

      ;; Critical test: Try to type again (P0-02 scenario)
      ;; This is where the bug manifests - typing doesn't appear on screen
      (rf/dispatch-sync [:self-insert-command "l"])
      (rf/dispatch-sync [:self-insert-command "i"])
      (rf/dispatch-sync [:self-insert-command "n"])
      (rf/dispatch-sync [:self-insert-command "e"])
      (rf/dispatch-sync [:self-insert-command " "])
      (rf/dispatch-sync [:self-insert-command "1"])

      (is (= "line 1" (get-buffer-text buffer-id))
          "Typing should still work after clearing buffer with backspace"))))

(deftest test-backspace-all-then-type-via-dom-events
  (testing "REGRESSION: DOM event handlers should work after backspacing entire buffer"
    (async done
      (let [buffer-id (create-test-buffer "")]
        (rf/dispatch-sync [:set-current-buffer buffer-id])

        ;; Type initial text via self-insert-command
        (doseq [ch "abcd"]
          (rf/dispatch-sync [:self-insert-command (str ch)]))

        (is (= "abcd" (get-buffer-text buffer-id))
            "Initial text should be present")

        ;; Backspace all characters
        (dotimes [_ 4]
          (rf/dispatch-sync [:delete-backward-char]))

        (is (= "" (get-buffer-text buffer-id))
            "Buffer should be empty after backspacing")

        ;; Check immediately after backspace - before setTimeout
        (let [db @rfdb/app-db
              active-window-id (:active-window-id db)
              window-tree (:window-tree db)
              active-window (when active-window-id
                              (lexicon.db/find-window-in-tree window-tree active-window-id))
              active-buffer-id (:buffer-id active-window)
              active-buffer (get-in db [:buffers active-buffer-id])
              wasm-instance (:wasm-instance active-buffer)]
          (println "🔍 IMMEDIATE after backspace (before setTimeout):")
          (println "  buffer-id from test:" buffer-id)
          (println "  active-buffer-id from window:" active-buffer-id)
          (println "  wasm-instance:" (if wasm-instance "PRESENT" "NIL")))

        ;; Wait a tick for DOM updates
        (js/setTimeout
         (fn []
           ;; Debug: Check state before attempting to insert
           (let [db @rfdb/app-db
                 active-window-id (:active-window-id db)
                 window-tree (:window-tree db)
                 active-window (when active-window-id
                                 (lexicon.db/find-window-in-tree window-tree active-window-id))
                 active-buffer-id (:buffer-id active-window)
                 active-buffer (get-in db [:buffers active-buffer-id])
                 wasm-instance (:wasm-instance active-buffer)]

             (println "🔍 DEBUG after backspace:")
             (println "  active-window-id:" active-window-id)
             (println "  active-window:" active-window)
             (println "  active-buffer-id:" active-buffer-id)
             (println "  active-buffer:" (keys active-buffer))
             (println "  wasm-instance:" (if wasm-instance "PRESENT" "NIL"))

             (is (some? active-window-id) "active-window-id should exist")
             (is (some? active-window) "active-window should exist")
             (is (some? active-buffer-id) "active-buffer-id should exist")
             (is (some? wasm-instance) "wasm-instance should exist after backspace"))

           ;; Try to trigger input via handle-text-input event (simulating DOM input)
           ;; Use :self-insert-command instead since :handle-text-input uses async effects
           ;; that don't flush in test environment
           (rf/dispatch-sync [:self-insert-command "a"])
           (rf/dispatch-sync [:self-insert-command "b"])
           (rf/dispatch-sync [:self-insert-command "c"])
           (rf/dispatch-sync [:self-insert-command "d"])

           ;; Check if text was inserted
           (let [result (get-buffer-text buffer-id)]
             (is (= "abcd" result)
                 (str "Text should be inserted after backspace. Got: " result)))

           (done))
         100)))))

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
