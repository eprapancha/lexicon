(ns lexicon.semantic.helpers
  "Helper functions for semantic compatibility tests.

  Based on lexicon.core-test pattern - uses WASM in browser test environment."
  (:require [cljs.test :refer [use-fixtures async]]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]))

;; =============================================================================
;; Test Context - Matches existing test pattern
;; =============================================================================

(defn reset-editor-db!
  "Reset re-frame db to default while preserving WASM constructor.

  This is a function, not a macro, so it can be called from tests."
  []
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])]
    (reset! rfdb/app-db db/default-db)
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))))

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
  "Get text content of buffer."
  [buffer-id]
  (let [buffer (get-in @rfdb/app-db [:buffers buffer-id])
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
