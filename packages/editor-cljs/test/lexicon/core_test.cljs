(ns lexicon.core-test
  "Core functionality tests for Lexicon editor.

  Tests basic text operations, cursor movement, and buffer state."
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
                            (.log js/console "✅ WASM ready for core-test")
                            (done)))
                   (.catch (fn [e]
                             (.error js/console "❌ WASM failed in core-test:" e)
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
  (reset-db!)
  ;; Don't call :initialize-db - it would wipe out the wasm-constructor that reset-db! preserved
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
    (when wasm-instance
      (try
        (.getText ^js wasm-instance)
        (catch js/Error _ nil)))))

(defn get-buffer-point
  "Get point (cursor position) for current buffer."
  [buffer-id]
  (let [db @rfdb/app-db]
    ;; Point is stored in :ui :cursor-position, not per-buffer
    (get-in db [:ui :cursor-position] 0)))

;; -- Text Operation Tests --

(deftest test-buffer-creation
  (testing "Creating a buffer with initial content"
    (let [buffer-id (create-test-buffer "Hello, World!")]
      (is (some? buffer-id) "Buffer ID should exist")
      (is (= "Hello, World!" (get-buffer-text buffer-id))
          "Buffer should contain initial content"))))

(deftest test-insert-text
  (testing "Inserting text at point"
    (let [buffer-id (create-test-buffer "Hello")]
      ;; Insert at end (point starts at 0 usually, let's set it to end)
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])
      (rf/dispatch-sync [:buffer/insert buffer-id 5 ", World!"])
      (is (= "Hello, World!" (get-buffer-text buffer-id))
          "Text should be inserted at point"))))

(deftest test-delete-text
  (testing "Deleting text from buffer"
    (let [buffer-id (create-test-buffer "Hello, World!")]
      ;; Delete ", World!" (from position 5 to end at position 13)
      (rf/dispatch-sync [:buffer/delete buffer-id 5 13])
      (is (= "Hello" (get-buffer-text buffer-id))
          "Text should be deleted from buffer"))))

(deftest test-self-insert-command
  (testing "Self-insert command adds character at point"
    (let [buffer-id (create-test-buffer "")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:self-insert-command "a"])
      (is (= "a" (get-buffer-text buffer-id))
          "Character should be inserted")
      (rf/dispatch-sync [:self-insert-command "b"])
      (is (= "ab" (get-buffer-text buffer-id))
          "Second character should be appended"))))

;; -- Cursor Movement Tests --

(deftest test-forward-char
  (testing "Moving cursor forward one character"
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])
      (is (= 0 (get-buffer-point buffer-id)) "Point should start at 0")

      (rf/dispatch-sync [:forward-char])
      (is (= 1 (get-buffer-point buffer-id)) "Point should move to 1")

      (rf/dispatch-sync [:forward-char])
      (is (= 2 (get-buffer-point buffer-id)) "Point should move to 2"))))

(deftest test-backward-char
  (testing "Moving cursor backward one character"
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])
      (is (= 5 (get-buffer-point buffer-id)) "Point should start at 5")

      (rf/dispatch-sync [:backward-char])
      (is (= 4 (get-buffer-point buffer-id)) "Point should move to 4")

      (rf/dispatch-sync [:backward-char])
      (is (= 3 (get-buffer-point buffer-id)) "Point should move to 3"))))

(deftest test-beginning-of-line
  (testing "Moving to beginning of line"
    (let [buffer-id (create-test-buffer "Hello\nWorld")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 8])  ; Middle of "World"
      (rf/dispatch-sync [:beginning-of-line])
      (is (= 6 (get-buffer-point buffer-id)) "Point should move to start of current line"))))

(deftest test-end-of-line
  (testing "Moving to end of line"
    (let [buffer-id (create-test-buffer "Hello\nWorld")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])  ; Start of first line
      (rf/dispatch-sync [:end-of-line])
      (is (= 5 (get-buffer-point buffer-id)) "Point should move to end of current line"))))

;; -- Kill Ring Tests --

(deftest test-kill-region
  (testing "Killing region adds to kill ring"
    (let [buffer-id (create-test-buffer "Hello, World!")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      ;; Set mark at 0, point at 5
      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])
      (rf/dispatch-sync [:set-mark-command])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])

      (rf/dispatch-sync [:kill-region])
      (is (= ", World!" (get-buffer-text buffer-id))
          "Text should be killed from buffer")

      ;; Check kill ring
      (let [db @rfdb/app-db
            kill-ring (:kill-ring db)]
        (is (= "Hello" (first kill-ring))
            "Killed text should be in kill ring")))))

(deftest test-yank
  (testing "Yanking from kill ring"
    (let [buffer-id (create-test-buffer "World")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      ;; Add to kill ring manually
      (swap! rfdb/app-db update :kill-ring conj "Hello, ")

      (rf/dispatch-sync [:buffer/goto-char buffer-id 0])
      (rf/dispatch-sync [:yank])

      (is (= "Hello, World" (get-buffer-text buffer-id))
          "Yanked text should be inserted at point"))))

;; -- Buffer Operations Tests --

(deftest test-switch-buffer
  (testing "Switching between buffers"
    (let [buffer1 (create-test-buffer "Buffer 1")
          buffer2 (random-uuid)]
      (rf/dispatch-sync [:create-buffer "Buffer 2"
                        :buffer-id buffer2
                        :content "Buffer 2"])

      (rf/dispatch-sync [:set-current-buffer buffer1])
      (is (= buffer1 (get-in @rfdb/app-db [:editor :current-buffer-id]))
          "Current buffer should be buffer1")

      (rf/dispatch-sync [:set-current-buffer buffer2])
      (is (= buffer2 (get-in @rfdb/app-db [:editor :current-buffer-id]))
          "Current buffer should be buffer2"))))

;; -- Undo Tests --

(deftest test-undo-insert
  (testing "Undo after insert"
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/goto-char buffer-id 5])
      (rf/dispatch-sync [:buffer/insert buffer-id 5 " World"])
      (is (= "Hello World" (get-buffer-text buffer-id))
          "Text should be inserted")

      (rf/dispatch-sync [:undo])
      (is (= "Hello" (get-buffer-text buffer-id))
          "Insert should be undone"))))

(deftest test-undo-delete
  (testing "Undo after delete"
    (let [buffer-id (create-test-buffer "Hello World")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:buffer/delete buffer-id 5 6])
      (is (= "Hello" (get-buffer-text buffer-id))
          "Text should be deleted")

      (rf/dispatch-sync [:undo])
      (is (= "Hello World" (get-buffer-text buffer-id))
          "Delete should be undone"))))

;; -- Run all tests --

(defn ^:export run-all-tests []
  (run-tests 'lexicon.core-test))
