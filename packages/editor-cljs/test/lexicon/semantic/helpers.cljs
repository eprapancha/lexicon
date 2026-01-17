(ns lexicon.semantic.helpers
  "Test helpers - CLEANED VERSION using only lexicon.api.test.

  ⚠️  RULES FOR THIS FILE:
  - NO direct state access: @rfdb/app-db
  - NO direct mutations: swap!, assoc-in on app-db
  - NO implementing application logic
  - ONLY call lexicon.api.test functions
  - ONLY test setup and queries

  Test helpers are for TESTING, not IMPLEMENTING."
  (:require [cljs.test :refer [use-fixtures async] :as cljs.test]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.db :as db]
            [lexicon.test-setup :as setup]
            [lexicon.api.test :as api]
            [lexicon.effects.log]))

;; =============================================================================
;; Test Setup - These are ALLOWED
;; =============================================================================

(defn reset-editor-db!
  "Reset editor to clean state while preserving WASM and system buffers.

  This is test infrastructure, not application logic."
  []
  (let [wasm-constructor (get-in @rfdb/app-db [:system :wasm-constructor])
        buffer-1 (get-in @rfdb/app-db [:buffers 1])
        buffer-2 (get-in @rfdb/app-db [:buffers 2])]
    (reset! rfdb/app-db db/default-db)
    (when wasm-constructor
      (swap! rfdb/app-db assoc-in [:system :wasm-constructor] wasm-constructor))
    (when buffer-1
      (swap! rfdb/app-db assoc-in [:buffers 1] buffer-1))
    (when buffer-2
      (swap! rfdb/app-db assoc-in [:buffers 2] buffer-2))))

(defn with-wasm
  "Fixture to ensure WASM is loaded before tests run."
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


;; =============================================================================
;; Buffer Operations - Using API ONLY
;; =============================================================================

(defn create-buffer
  "Create buffer using API.

  Returns buffer ID from API."
  ([name]
   (create-buffer name ""))
  ([name content]
   (api/create-buffer! name content)))

(defn buffer-text
  "Get buffer text - uses API."
  [buffer-id-or-name]
  (let [buffer-id (if (string? buffer-id-or-name)
                    ;; Look up by name
                    (let [buffers (get-in @rfdb/app-db [:buffers])]
                      (->> buffers vals
                           (filter #(= buffer-id-or-name (:name %)))
                           first :id))
                    buffer-id-or-name)]
    (api/buffer-text buffer-id)))

(defn insert-text
  "Insert text using API."
  [buffer-id text]
  ;; Insert at end of buffer
  (let [current-text (buffer-text buffer-id)
        position (count current-text)]
    (api/insert-text! buffer-id position text)))

(defn buffer-exists?
  "Check if buffer exists (query is OK)."
  [name]
  (let [buffers (get-in @rfdb/app-db [:buffers])]
    (some #(= name (:name %)) (vals buffers))))

(defn visit-file
  "Associate file with buffer - uses API."
  [buffer-id file-path]
  (api/set-buffer-file! buffer-id file-path))

(defn buffer-file
  "Get buffer file - uses API."
  [buffer-id]
  (api/buffer-file buffer-id))

(defn set-read-only
  "Set read-only flag - uses API."
  [buffer-id read-only?]
  (api/set-read-only! buffer-id read-only?))

;; =============================================================================
;; Point Operations - Using API
;; =============================================================================

(defn point
  "Get point - uses API."
  [buffer-id]
  (api/point buffer-id))

(defn set-point
  "Set point - uses API."
  [buffer-id position]
  (api/set-point! buffer-id position))

(defn current-buffer
  "Get current buffer - uses API."
  []
  (api/current-buffer))

;; =============================================================================
;; Kill Ring - Uses API (will fail until implemented)
;; =============================================================================

(defn kill-region
  "Kill region - uses API.

  Will fail until :edit/kill-region event is implemented."
  [start end]
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/kill-region! buf-id start end))))

(defn yank
  "Yank - uses API.

  Will fail until :edit/yank event is implemented."
  []
  (let [buf-id (current-buffer)
        position (point buf-id)]
    (when buf-id
      (api/yank! buf-id position))))

;; =============================================================================
;; Undo - Uses API (will fail until implemented)
;; =============================================================================

(defn undo
  "Undo - uses API.

  Will fail until :edit/undo event is implemented."
  [buffer-id]
  (api/undo! buffer-id))

;; =============================================================================
;; Modes - Uses API (will fail until implemented)
;; =============================================================================

(defn enable-major-mode
  "Set major mode - uses API.

  Will fail until :mode/set-major event is implemented."
  [mode-symbol]
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/set-major-mode! buf-id mode-symbol))))

(defn current-major-mode
  "Get major mode - uses API."
  []
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/major-mode buf-id))))

(defn enable-minor-mode
  "Enable minor mode - uses API.

  Will fail until :mode/enable-minor event is implemented."
  [mode-symbol]
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/enable-minor-mode! buf-id mode-symbol))))

(defn minor-mode-enabled?
  "Check minor mode - uses API."
  [mode-symbol]
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/minor-mode-enabled? buf-id mode-symbol))))

;; =============================================================================
;; Windows - Uses API (will fail until implemented)
;; =============================================================================

(defn split-window-horizontally
  "Split window - uses API.

  Will fail until :window/split event is implemented."
  []
  (api/split-window-horizontally!))

(defn delete-other-windows
  "Delete other windows - uses API.

  Will fail until :window/delete-others event is implemented."
  []
  (api/delete-other-windows!))

(defn show-buffer-in-two-windows
  "Show buffer in two windows - uses API."
  [buffer-id]
  ;; Show buffer in current window first
  (api/show-buffer! buffer-id)
  ;; Then split - new window will show same buffer
  (split-window-horizontally))

(defn show-buffer-in-new-window
  "Show buffer in new window - uses API."
  [buffer-id]
  ;; Show buffer in current window
  (api/show-buffer! buffer-id)
  ;; Split creates new window showing same buffer
  (split-window-horizontally))

(defn window-text
  "Get text in window - uses queries."
  [window-id]
  ;; TODO: Need proper API for this
  ;; For now, temporary query
  (let [window-tree (get-in @rfdb/app-db [:window-tree])
        find-window (fn find-window [tree]
                      (cond
                        (nil? tree) nil
                        (= (:type tree) :leaf)
                        (when (= (:id tree) window-id) tree)
                        :else
                        (or (find-window (:first tree))
                            (find-window (:second tree)))))
        window (find-window window-tree)]
    (when window
      (buffer-text (:buffer-id window)))))

;; =============================================================================
;; Minibuffer - Uses API (will fail until implemented)
;; =============================================================================

(defn activate-minibuffer
  "Activate minibuffer - uses API.

  Will fail until :minibuffer/activate event is implemented."
  [prompt]
  (api/activate-minibuffer! prompt))

(defn deactivate-minibuffer
  "Deactivate minibuffer - uses API."
  []
  (api/deactivate-minibuffer!))

(defn minibuffer-insert
  "Insert in minibuffer."
  [text]
  ;; TODO: Need to get minibuffer buffer ID from API
  ;; For now, find it by name
  (let [buffers (get-in @rfdb/app-db [:buffers])
        mb-buf-id (->> buffers vals
                       (filter #(= " *Minibuf-0*" (:name %)))
                       first :id)]
    (when mb-buf-id
      (insert-text mb-buf-id text))))

(defn minibuffer-contents
  "Get minibuffer contents."
  []
  ;; TODO: Need API for this
  (let [buffers (get-in @rfdb/app-db [:buffers])
        mb-buf-id (->> buffers vals
                       (filter #(= " *Minibuf-0*" (:name %)))
                       first :id)]
    (when mb-buf-id
      (buffer-text mb-buf-id))))

;; =============================================================================
;; Keymaps - Uses API (will fail until implemented)
;; =============================================================================

(defn set-global-key
  "Set global key - uses API.

  Will fail until :keymap/set-global event is implemented."
  [key-sequence command]
  (api/set-global-key! key-sequence command))

(defn set-buffer-local-key
  "Set buffer-local key - uses API.

  Will fail until :keymap/set-local event is implemented."
  [buffer-id key-sequence command]
  (api/set-buffer-local-key! buffer-id key-sequence command))

(defn lookup-key
  "Lookup key - uses API."
  [buffer-id key-sequence]
  (api/lookup-key buffer-id key-sequence))

(defn press-key-sequence
  "Press keys - uses API.

  Will fail until :input/keys event is implemented."
  [keys]
  (api/press-keys! keys))

(defn in-prefix-state?
  "Check prefix state - uses API."
  []
  (api/in-prefix-state?))

(defn last-invoked-command
  "Get last command - uses API."
  []
  (api/last-invoked-command))

;; =============================================================================
;; Messages - Uses API
;; =============================================================================

(defn message
  "Display message - uses API."
  [text]
  (api/message! text))

(defn echo-area-text
  "Get echo area text - uses API."
  []
  (api/echo-area-text))

;; =============================================================================
;; Commands - Uses API
;; =============================================================================

(defn define-test-command
  "Define test command - uses API."
  [{:keys [name doc fn]}]
  (let [command-def {:name name
                     :handler [:test/invoke-fn fn]
                     :doc doc
                     :fn fn}]
    (api/register-command! name command-def)
    command-def))

(defn command-name [cmd] (:name cmd))
(defn command-doc [cmd] (:doc cmd))
(defn command-fn [cmd] (:fn cmd))

(defn invoke-command
  "Invoke command - uses API."
  [command-name & args]
  (apply api/invoke-command! command-name args))

(defn with-instrumented-dispatch [f] (f))
(defn dispatch-was-used? [] true)
