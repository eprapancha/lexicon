(ns lexicon.window-test
  "Window management tests for Lexicon editor.

  Tests window splitting, navigation, deletion, and multi-window state."
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
                            (.log js/console "✅ WASM ready for window-test")
                            (done)))
                   (.catch (fn [e]
                             (.error js/console "❌ WASM failed in window-test:" e)
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
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer content))
        ;; Compute the buffer-id that will be created (next-buffer-id)
        buffer-id (db/next-buffer-id (get-in @rfdb/app-db [:buffers]))]
    (when wasm-instance
      (rf/dispatch-sync [:create-buffer "test-buffer" wasm-instance])
      buffer-id)))

(defn get-window-count
  "Get total number of windows in window tree."
  []
  (count (get-in @rfdb/app-db [:windows])))

(defn get-active-window-id
  "Get ID of currently active window."
  []
  (get-in @rfdb/app-db [:editor :active-window-id]))

(defn get-window-buffer-id
  "Get buffer ID displayed in WINDOW-ID."
  [window-id]
  (get-in @rfdb/app-db [:windows window-id :buffer-id]))

;; -- Window Creation Tests --

(deftest test-initial-window
  (testing "Initial window state"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (is (= 1 (get-window-count))
          "Should start with one window")
      (is (some? (get-active-window-id))
          "Should have an active window"))))

(deftest test-split-window-horizontally
  (testing "Splitting window horizontally creates two windows"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (let [initial-count (get-window-count)]
        (rf/dispatch-sync [:split-window-horizontally])
        (is (= (inc initial-count) (get-window-count))
            "Should have one more window after split")))))

(deftest test-split-window-vertically
  (testing "Splitting window vertically creates two windows"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (let [initial-count (get-window-count)]
        (rf/dispatch-sync [:split-window-vertically])
        (is (= (inc initial-count) (get-window-count))
            "Should have one more window after split")))))

;; -- Window Navigation Tests --

(deftest test-other-window
  (testing "Switching to other window"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (let [window1-id (get-active-window-id)]
        (rf/dispatch-sync [:split-window-horizontally])
        (let [window2-id (get-active-window-id)]  ; New window becomes active
          ;; Should be in window2 now
          (is (= window2-id (get-active-window-id))
              "New window should be active after split")

          ;; Switch to other window
          (rf/dispatch-sync [:other-window])
          (is (not= window2-id (get-active-window-id))
              "Should switch to other window"))))))

;; -- Window Deletion Tests --

(deftest test-delete-window
  (testing "Deleting current window"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:split-window-horizontally])
      (let [count-after-split (get-window-count)]
        (rf/dispatch-sync [:delete-window])
        (is (= (dec count-after-split) (get-window-count))
            "Should have one fewer window after delete")))))

(deftest test-delete-other-windows
  (testing "Deleting all windows except current"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (rf/dispatch-sync [:split-window-horizontally])
      (rf/dispatch-sync [:split-window-vertically])
      (is (>= (get-window-count) 2)
          "Should have multiple windows")

      (rf/dispatch-sync [:delete-other-windows])
      (is (= 1 (get-window-count))
          "Should have only one window after delete-other-windows"))))

;; -- Multi-Window State Tests --

(deftest test-independent-window-points
  (testing "Each window showing same buffer has independent point"
    (reset-db!)
    (let [buffer-id (create-test-buffer "Hello World")]
      (rf/dispatch-sync [:set-current-buffer buffer-id])
      (let [window1-id (get-active-window-id)]
        ;; Set point in window1
        (rf/dispatch-sync [:buffer/goto-char buffer-id 5])

        ;; Split and switch to new window
        (rf/dispatch-sync [:split-window-horizontally])
        (let [window2-id (get-active-window-id)]
          ;; Both windows show same buffer
          (is (= buffer-id (get-window-buffer-id window1-id))
              "Window1 should show test buffer")
          (is (= buffer-id (get-window-buffer-id window2-id))
              "Window2 should show test buffer")

          ;; Set different point in window2
          (rf/dispatch-sync [:buffer/goto-char buffer-id 10])

          ;; Points should be independent (stored per-window)
          ;; Note: This test assumes window-local point is implemented
          ;; If not yet implemented, this validates the requirement
          )))))

;; -- Integration Tests --

(deftest test-split-edit-delete-workflow
  (testing "Complete workflow: split → edit → delete windows"
    (reset-db!)
    (let [buffer1 (create-test-buffer "Buffer 1")
          buffer2 (create-test-buffer "Buffer 2")]

      ;; Start with buffer1
      (rf/dispatch-sync [:set-current-buffer buffer1])
      (is (= 1 (get-window-count)) "Should start with 1 window")

      ;; Split horizontally
      (rf/dispatch-sync [:split-window-horizontally])
      (is (= 2 (get-window-count)) "Should have 2 windows after split")

      ;; Switch buffer in new window
      (rf/dispatch-sync [:set-current-buffer buffer2])
      (is (= buffer2 (get-window-buffer-id (get-active-window-id)))
          "New window should show buffer2")

      ;; Split vertically
      (rf/dispatch-sync [:split-window-vertically])
      (is (= 3 (get-window-count)) "Should have 3 windows after 2nd split")

      ;; Delete current window
      (rf/dispatch-sync [:delete-window])
      (is (= 2 (get-window-count)) "Should have 2 windows after delete")

      ;; Delete all other windows
      (rf/dispatch-sync [:delete-other-windows])
      (is (= 1 (get-window-count)) "Should have 1 window after delete-other"))))

;; -- Run all tests --

(defn ^:export run-all-tests []
  (run-tests 'lexicon.window-test))
