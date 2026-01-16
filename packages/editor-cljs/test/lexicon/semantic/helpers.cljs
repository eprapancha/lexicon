(ns lexicon.semantic.helpers
  "Helper functions for semantic compatibility tests.

  Based on lexicon.core-test pattern - uses WASM in browser test environment."
  (:require [cljs.test :refer [use-fixtures async]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]
            [lexicon.api.message :as api-msg]
            [lexicon.effects.log]))  ; Load log effect handlers

;; =============================================================================
;; Test Context - Matches existing test pattern
;; =============================================================================

(defn reset-editor-db!
  "Reset editor to clean state while preserving WASM and system buffers.

  Clears user-created buffers (ID >= 3) but keeps:
  - Buffer 1: *scratch*
  - Buffer 2: *Messages*
  - WASM constructor"
  []
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])
        buffer-1 (get-in @rfdb/app-db [:buffers 1])
        buffer-2 (get-in @rfdb/app-db [:buffers 2])]
    ;; Start with default-db
    (reset! rfdb/app-db db/default-db)
    ;; Restore WASM and system buffers
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))
    (when buffer-1
      (swap! rfdb/app-db assoc-in [:buffers 1] buffer-1))
    (when buffer-2
      (swap! rfdb/app-db assoc-in [:buffers 2] buffer-2))))

(defn current-db
  "Get current re-frame database."
  []
  @rfdb/app-db)

;; =============================================================================
;; Buffer Operations
;; =============================================================================

(defn create-buffer
  "Create a new buffer with given name (and optional content).

  Returns buffer ID.

  Example:
    (create-buffer \"test\")           ; empty buffer
    (create-buffer \"test\" \"hello\")  ; with content"
  ([name]
   (create-buffer name ""))
  ([name content]
   (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (new WasmGapBuffer content))
         buffer-id (db/next-buffer-id (get-in @rfdb/app-db [:buffers]))]
     (when wasm-instance
       (rf/dispatch-sync [:create-buffer name wasm-instance])
       buffer-id))))

(defn buffer-text
  "Get text content of buffer by ID or name."
  [buffer-id-or-name]
  (let [buffer-id (if (string? buffer-id-or-name)
                    ;; Look up by name
                    (let [buffers (get-in @rfdb/app-db [:buffers])]
                      (->> buffers
                           vals
                           (filter #(= buffer-id-or-name (:name %)))
                           first
                           :id))
                    buffer-id-or-name)
        buffer (get-in @rfdb/app-db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)]
    (when wasm-instance
      (try
        (.getText wasm-instance)
        (catch js/Error _ "")))))

(defn insert-text
  "Insert text into buffer at end of buffer."
  [buffer-id text]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])
        ^js wasm-instance (:wasm-instance buffer)
        ;; Insert at end of buffer - get length from text
        point (if wasm-instance
                (count (.getText wasm-instance))
                0)]
    (rf/dispatch-sync [:buffer/insert buffer-id point text])))

(defn buffer-file
  "Get file path associated with buffer."
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :file-handle]))

(defn buffer-exists?
  "Check if buffer with name exists."
  [name]
  (let [buffers (get-in @rfdb/app-db [:buffers])]
    (some #(= name (:name %)) (vals buffers))))

(defn message
  "Display a message in the echo area and append to *Messages* buffer."
  [text]
  (api-msg/message text))

;; =============================================================================
;; WASM Fixture - Required for all semantic tests
;; =============================================================================

(defn with-wasm
  "Fixture to ensure WASM is loaded before tests run.

  Usage in test file:
    (use-fixtures :once with-wasm)"
  [f]
  (async done
    (-> setup/wasm-load-promise
        (.then (fn [_]
                 (.log js/console "✅ WASM ready for semantic tests")
                 (f)
                 (done)))
        (.catch (fn [e]
                  (.error js/console "❌ WASM failed:" e)
                  (done))))))
