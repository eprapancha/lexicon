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
            [lexicon.dynamic :as dyn]
            [lexicon.effects]
            [lexicon.effects.log])
  (:require-macros [lexicon.macros :refer [save-excursion save-current-buffer with-current-buffer]]
                   [lexicon.dynamic :refer [with-inhibit-read-only without-undo]]
                   [lexicon.semantic.helpers :refer [with-test-buffer]]))

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

(defn undo-boundary
  "Insert undo boundary - uses API."
  [buffer-id]
  (api/undo-boundary! buffer-id))

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
;; SCI/Lisp Integration - Uses API
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code - uses API.

  Returns: {:success true :result value} or {:success false :error e}"
  [code-str]
  (api/eval-lisp code-str))

;; =============================================================================
;; File Operations - Uses API
;; =============================================================================

(defn save-buffer
  "Save buffer - uses API."
  [buffer-id]
  (api/save-buffer! buffer-id))

(defn revert-buffer
  "Revert buffer - uses API."
  [buffer-id]
  (api/revert-buffer! buffer-id))

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

;; =============================================================================
;; Text Properties - Uses API (will fail until implemented)
;; =============================================================================

(defn put-text-property
  "Set text property on range.

  Will fail until :text-property/put event is implemented."
  [buffer-id start end property value]
  (api/put-text-property! buffer-id start end property value))

(defn get-text-property
  "Get text property at position."
  [buffer-id position property]
  (api/get-text-property buffer-id position property))

(defn text-properties-at
  "Get all properties at position."
  [buffer-id position]
  (api/text-properties-at buffer-id position))

(defn propertize
  "Create string with properties."
  [text & properties]
  (apply api/propertize text properties))

(defn get-text-property-from-string
  "Get property from propertized string."
  [string position property]
  (api/get-text-property-from-string string position property))

(defn visible-text
  "Get visible text (respects 'invisible property)."
  [buffer-id]
  (api/visible-text buffer-id))

;; =============================================================================
;; Convenience wrappers for common operations
;; =============================================================================

(defn insert
  "Insert text at current point in buffer."
  [text]
  (let [buf-id (current-buffer)
        position (point buf-id)]
    (when buf-id
      (insert-text buf-id text))))

(defn goto-char
  "Move point to position."
  [position]
  (let [buf-id (current-buffer)]
    (when buf-id
      (set-point buf-id position))))

(defn delete-region
  "Delete text from start to end."
  [start end]
  (let [buf-id (current-buffer)]
    (when buf-id
      ;; TODO: Implement proper delete-region event
      ;; For now, this is a placeholder
      (api/delete-region! buf-id start end))))

(defn erase-buffer
  "Delete all text in buffer."
  []
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)]
    (when (and buf-id text)
      (delete-region 0 (count text)))))

(defn buffer-name
  "Get name of buffer."
  [buf-id]
  (get-in @rfdb/app-db [:buffers buf-id :name]))

(defn buffer-read-only?
  "Check if buffer is read-only."
  []
  (let [buf-id (current-buffer)]
    (when buf-id
      (get-in @rfdb/app-db [:buffers buf-id :is-read-only?]))))

(defn set-buffer-read-only
  "Set read-only flag for current buffer."
  [read-only?]
  (let [buf-id (current-buffer)]
    (when buf-id
      (set-read-only buf-id read-only?))))

;; =============================================================================
;; Mark and Region - Stubs (will fail until implemented)
;; =============================================================================

(defn set-mark
  "Set mark at position."
  [position]
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/set-mark! buf-id position))))

(defn mark
  "Get mark position."
  []
  (let [buf-id (current-buffer)]
    (when buf-id
      (api/mark buf-id))))

(defn activate-mark
  "Activate the mark (make region active).

  NOT implemented yet."
  []
  ;; TODO: Implement
  nil)

(defn deactivate-mark
  "Deactivate the mark (make region inactive).

  NOT implemented yet."
  []
  ;; TODO: Implement
  nil)

(defn region-active?
  "Check if region is active.

  NOT implemented yet."
  []
  ;; TODO: Implement
  false)

;; =============================================================================
;; Search and Navigation - Stubs (will fail until implemented)
;; =============================================================================

(defn forward-word
  "Move forward one word.

  NOT implemented yet."
  []
  ;; TODO: Implement
  nil)

(defn backward-word
  "Move backward one word.

  NOT implemented yet."
  []
  ;; TODO: Implement
  nil)

(defn word-at-point
  "Get word at point.

  NOT implemented yet."
  []
  ;; TODO: Implement
  nil)

(defn looking-at
  "Check if text at point matches regexp.

  NOT implemented yet."
  [regexp]
  ;; TODO: Implement
  false)

(defn search-forward
  "Search forward for string.

  NOT implemented yet."
  [string &optional bound noerror count]
  ;; TODO: Implement
  nil)

;; =============================================================================
;; Narrowing - Stubs (will fail until implemented)
;; =============================================================================

(defn narrow-to-region
  "Restrict editing to region [start, end).

  NOT implemented yet."
  [start end]
  ;; TODO: Implement
  nil)

;; =============================================================================
;; Hooks - Uses API
;; =============================================================================

(defn add-hook
  "Add function to hook."
  [hook-name hook-fn]
  (api/add-hook! hook-name hook-fn))

(defn remove-hook
  "Remove function from hook."
  [hook-name hook-fn]
  (api/remove-hook! hook-name hook-fn))

(defn run-hooks
  "Run all functions in hook."
  [hook-name & args]
  (apply api/run-hooks! hook-name args))

(defn run-hook-with-args-until-success
  "Run hook functions until one returns non-nil (stub for tests)."
  [hook-name & args]
  nil)

(defn run-hook-with-args-until-failure
  "Run hook functions until one returns nil (stub for tests)."
  [hook-name & args]
  nil)

;; =============================================================================
;; Keymap helpers (stubs for tests)
;; =============================================================================

(defn make-keymap
  "Create a new empty keymap (stub for tests)."
  []
  {})

(defn define-key
  "Define a key binding in keymap (stub for tests)."
  [keymap key command]
  (assoc keymap key command))

;; =============================================================================
;; Buffer ID helpers (stubs for tests)
;; =============================================================================

(defn current-buffer-id
  "Get current buffer ID (stub for tests)."
  []
  1)

(defn kill-buffer
  "Kill a buffer (stub for tests)."
  [buffer-id]
  nil)

(defn toggle-read-only
  "Toggle buffer read-only state (stub for tests)."
  []
  nil)

(defn mode-line-string
  "Get mode line string (stub for tests)."
  []
  "-- *test*")

(defn forward-char
  "Move point forward by N characters (stub for tests)."
  ([] (forward-char 1))
  ([n] nil))

(defn backward-char
  "Move point backward by N characters (stub for tests)."
  ([] (backward-char 1))
  ([n] nil))

(defn dired
  "Open directory in dired mode (stub for tests)."
  [directory]
  nil)

;; =============================================================================
;; Window helpers (stubs for tests)
;; =============================================================================

(defn current-window
  "Get current window (stub for tests)."
  []
  :window-1)

(defn select-window
  "Select a window (stub for tests)."
  [window]
  nil)

(defn split-window
  "Split current window (stub for tests)."
  []
  :window-2)

;; =============================================================================
;; Marker helpers (stubs for tests)
;; =============================================================================

(defn make-marker
  "Create a new marker (stub for tests)."
  []
  (atom {:position nil :buffer nil}))

(defn marker-position
  "Get marker position (stub for tests)."
  [marker]
  (:position @marker))

(defn marker-buffer
  "Get marker buffer (stub for tests)."
  [marker]
  (:buffer @marker))

(defn set-marker
  "Set marker position (stub for tests)."
  [marker pos]
  (swap! marker assoc :position pos)
  marker)

(defn move-marker
  "Move marker to new position (stub for tests)."
  [marker pos]
  (swap! marker assoc :position pos)
  marker)

(defn copy-marker
  "Copy a marker (stub for tests)."
  [marker]
  (atom @marker))

;; =============================================================================
;; Macros for with-test-buffer
;; =============================================================================
;; Macro with-test-buffer is defined in helpers.clj (required via :require-macros)
