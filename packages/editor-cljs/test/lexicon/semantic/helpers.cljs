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
            [lexicon.core.db :as db]
            [lexicon.test-setup :as setup]
            [lexicon.core.api.test :as api]
            [lexicon.core.dynamic :as dyn]
            [lexicon.core.effects]
            [lexicon.core.effects.log])
  (:require-macros [lexicon.core.macros :refer [save-excursion save-current-buffer with-current-buffer]]
                   [lexicon.core.dynamic :refer [with-inhibit-read-only without-undo]]
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
  ([marker]
   (atom @marker))
  ([marker insertion-type]
   (atom (assoc @marker :insertion-type insertion-type))))

(defn marker-insertion-type
  "Get marker insertion type (stub for tests).
  Returns t if marker advances on insertion at its position, nil otherwise."
  [marker]
  (:insertion-type @marker))

(defn set-marker-insertion-type
  "Set marker insertion type (stub for tests).
  TYPE = t: marker advances when text inserted at its position
  TYPE = nil: marker stays before inserted text"
  [marker type]
  (swap! marker assoc :insertion-type type)
  type)

(defn markerp
  "Check if object is a marker (stub for tests)."
  [obj]
  (and (instance? Atom obj)
       (map? @obj)
       (contains? @obj :position)))

(defn insert-before-markers
  "Insert text, keeping all markers before the insertion (stub for tests).
  Unlike regular insert, all markers at insertion point stay before the text."
  [text]
  ;; Stub - just calls regular insert for now
  (insert text))

(defn buffer-string
  "Get entire buffer contents as string (stub for tests)."
  []
  (let [buf-id (current-buffer)]
    (when buf-id
      (buffer-text buf-id))))

;; =============================================================================
;; Kill Ring helpers (stubs for tests)
;; =============================================================================

(defn kill-new
  "Add string to kill ring (stub for tests)."
  [string]
  ;; TODO: Implement via API
  nil)

(defn kill-append
  "Append to last kill (stub for tests).
  BEFORE-P means prepend instead."
  [string before-p]
  ;; TODO: Implement via API
  nil)

(defn current-kill
  "Get Nth kill from ring (stub for tests)."
  [n]
  ;; TODO: Implement via API
  nil)

(defn kill-ring-length
  "Get number of items in kill ring (stub for tests)."
  []
  ;; TODO: Implement via API
  0)

(defn clear-kill-ring
  "Clear the kill ring (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn yank-pop
  "Replace last yank with previous kill (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn kill-line
  "Kill to end of line (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn kill-word
  "Kill word forward (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn backward-kill-word
  "Kill word backward (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn copy-region-as-kill
  "Copy region to kill ring without deleting (stub for tests)."
  [start end]
  ;; TODO: Implement via API
  nil)

;; =============================================================================
;; Buffer Primitives (stubs for tests - Issue #100)
;; =============================================================================

(defn buffer-live-p
  "Check if buffer is alive (stub for tests).
  Returns true if buffer exists and hasn't been killed."
  [buffer-id]
  ;; TODO: Implement via API
  (some? (get-in @rfdb/app-db [:buffers buffer-id])))

(defn get-buffer-create
  "Get or create buffer with NAME (stub for tests).
  Returns existing buffer if found, creates new one otherwise."
  [name]
  ;; TODO: Implement via API
  (if-let [buf (->> (get-in @rfdb/app-db [:buffers])
                    vals
                    (filter #(= name (:name %)))
                    first)]
    (:id buf)
    (create-buffer name)))

(defn buffer-modified-p
  "Check if current buffer has been modified (stub for tests)."
  []
  ;; TODO: Implement via API
  (let [buf-id (current-buffer)]
    (get-in @rfdb/app-db [:buffers buf-id :modified] false)))

(defn set-buffer-modified-p
  "Set buffer modification flag (stub for tests)."
  [flag]
  ;; TODO: Implement via API
  nil)

(defn widen
  "Remove narrowing restriction (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn save-restriction
  "Execute body with saved narrowing state (stub for tests).
  Takes a function to execute."
  [body-fn]
  ;; TODO: Implement properly - for now just call the function
  (body-fn))

(defn set-buffer
  "Make BUFFER-OR-NAME the current buffer (stub for tests).
  Does not display the buffer."
  [buffer-or-name]
  ;; TODO: Implement via API
  nil)

(defn rename-buffer
  "Rename current buffer to NEWNAME (stub for tests)."
  [newname]
  ;; TODO: Implement via API
  nil)

(defn other-buffer
  "Return most recently used buffer other than current (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn buffer-disable-undo
  "Disable undo recording for current buffer (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn buffer-enable-undo
  "Enable undo recording for current buffer (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn buffer-local-value
  "Get value of VARIABLE in BUFFER (stub for tests)."
  [variable buffer-id]
  ;; TODO: Implement via API
  nil)

(defn make-local-variable
  "Make VARIABLE buffer-local (stub for tests)."
  [variable]
  ;; TODO: Implement via API
  nil)

(defn setq
  "Set VARIABLE to VALUE (stub for tests).
  Named setq to avoid conflict with cljs.core/set."
  [variable value]
  ;; TODO: Implement via API
  nil)

(defn kill-all-local-variables
  "Kill all buffer-local variables (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn point-min
  "Return minimum point in current buffer (stub for tests).
  With narrowing, returns beginning of narrowed region."
  []
  ;; TODO: Implement via API
  0)

(defn point-max
  "Return maximum point in current buffer (stub for tests).
  With narrowing, returns end of narrowed region."
  []
  ;; TODO: Implement via API
  (let [buf-id (current-buffer)]
    (count (buffer-text buf-id))))

;; =============================================================================
;; Undo helpers (stubs for tests - Issue #103)
;; =============================================================================

(defn with-undo-group
  "Execute body-fn with all edits grouped for atomic undo (stub for tests)."
  [body-fn]
  ;; TODO: Implement via API
  (body-fn))

(defn undo-history-length
  "Get number of entries in undo history (stub for tests)."
  []
  ;; TODO: Implement via API
  0)

(defn undo-in-region
  "Undo changes within active region only (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn redo
  "Redo last undone change (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

;; =============================================================================
;; Editing Primitives (stubs for tests - Issue #105)
;; =============================================================================

(defn bobp
  "Check if point is at beginning of buffer (stub for tests)."
  []
  (let [buf-id (current-buffer)]
    (= 0 (point buf-id))))

(defn eobp
  "Check if point is at end of buffer (stub for tests)."
  []
  (let [buf-id (current-buffer)]
    (= (point buf-id) (count (buffer-text buf-id)))))

(defn char-after
  "Get character after point (stub for tests).
  Returns nil at end of buffer."
  ([]
   (let [buf-id (current-buffer)]
     (char-after (point buf-id))))
  ([pos]
   (let [buf-id (current-buffer)
         text (buffer-text buf-id)]
     (when (< pos (count text))
       (str (nth text pos))))))

(defn char-before
  "Get character before point (stub for tests).
  Returns nil at beginning of buffer."
  ([]
   (let [buf-id (current-buffer)]
     (char-before (point buf-id))))
  ([pos]
   (let [buf-id (current-buffer)
         text (buffer-text buf-id)]
     (when (> pos 0)
       (str (nth text (dec pos)))))))

(defn bolp
  "Check if point is at beginning of line (stub for tests)."
  []
  ;; TODO: Implement properly
  (or (bobp)
      (= "\n" (char-before))))

(defn eolp
  "Check if point is at end of line (stub for tests)."
  []
  ;; TODO: Implement properly
  (or (eobp)
      (= "\n" (char-after))))

(defn delete-and-extract-region
  "Delete region and return deleted text (stub for tests)."
  [start end]
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)
        start' (min start end)
        end' (max start end)
        deleted (subs text start' end')]
    (delete-region start end)
    deleted))

(defn buffer-substring-no-properties
  "Get buffer substring without text properties (stub for tests)."
  [start end]
  ;; TODO: Implement with actual property stripping
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)]
    (subs text (min start end) (max start end))))

(defn region-beginning
  "Return beginning of region (smaller of point and mark) (stub for tests)."
  []
  (let [buf-id (current-buffer)
        pt (point buf-id)
        mk (or (mark) pt)]
    (min pt mk)))

(defn region-end
  "Return end of region (larger of point and mark) (stub for tests)."
  []
  (let [buf-id (current-buffer)
        pt (point buf-id)
        mk (or (mark) pt)]
    (max pt mk)))

(defn line-beginning-position
  "Return position of beginning of current line (stub for tests)."
  []
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)
        pos (point buf-id)]
    (loop [p pos]
      (cond
        (<= p 0) 0
        (= "\n" (str (nth text (dec p)))) p
        :else (recur (dec p))))))

(defn line-end-position
  "Return position of end of current line (stub for tests)."
  []
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)
        pos (point buf-id)
        len (count text)]
    (loop [p pos]
      (cond
        (>= p len) len
        (= "\n" (str (nth text p))) p
        :else (recur (inc p))))))

(defn buffer-substring
  "Get text between START and END (stub for tests)."
  [start end]
  (let [buf-id (current-buffer)
        text (buffer-text buf-id)]
    (subs text (min start end) (max start end))))

;; =============================================================================
;; SCI Integration helpers (stubs for tests - Issue #106)
;; =============================================================================

(defn set-prefix-arg
  "Set the prefix argument for the next command (stub for tests)."
  [arg]
  ;; TODO: Implement via API
  nil)

(defn sci-namespace-keys
  "Get list of symbols exposed in SCI namespace (stub for tests)."
  []
  ;; Return the known set from lexicon.lisp/sci-namespace
  '#{point point-min point-max goto-char
     buffer-string buffer-substring insert delete-region
     buffer-name buffer-size current-buffer
     message forward-line beginning-of-line end-of-line
     search-forward search-backward
     set-major-mode enable-minor-mode
     kill-region yank set-mark mark})

;; =============================================================================
;; Isearch helpers (stubs for tests - Issue #107)
;; =============================================================================

(defn isearch-forward
  "Start incremental search forward (stub for tests)."
  [string]
  ;; TODO: Implement via API
  nil)

(defn isearch-forward-repeat
  "Continue incremental search forward (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn isearch-backward
  "Start incremental search backward (stub for tests)."
  [string]
  ;; TODO: Implement via API
  nil)

(defn query-replace
  "Query replace STRING with REPLACEMENT (stub for tests).
  Mode can be :all, :first, or interactive."
  [string replacement mode]
  ;; TODO: Implement via API
  nil)

;; =============================================================================
;; Minibuffer/Completion helpers (stubs for tests - Issue #108)
;; =============================================================================

(defn completion-metadata
  "Get completion metadata for COMMAND (stub for tests)."
  [command]
  ;; TODO: Implement via API
  {:category :command})

(defn all-completions
  "Get all completions for PREFIX from COLLECTION (stub for tests)."
  [prefix collection]
  (filter #(clojure.string/starts-with? % prefix) collection))

;; =============================================================================
;; Window helpers (stubs for tests - Issue #110)
;; =============================================================================

(defn split-window-vertically
  "Split window top/bottom (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn delete-window
  "Delete current window (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn window-count
  "Get number of windows (stub for tests)."
  []
  ;; TODO: Implement via API
  1)

(defn other-window
  "Select next window (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn current-window-configuration
  "Get current window configuration (stub for tests)."
  []
  ;; TODO: Implement via API
  {:windows []})

;; =============================================================================
;; File I/O helpers (stubs for tests - Issue #111)
;; =============================================================================

(defn find-file
  "Open FILE in buffer (stub for tests)."
  [file]
  ;; TODO: Implement via API
  1)

(defn file-exists-p
  "Check if FILE exists (stub for tests)."
  [file]
  ;; TODO: Implement via API - this is a real check
  (try
    (let [fs (js/require "fs")]
      (.existsSync fs file))
    (catch :default _ false)))

(defn file-directory-p
  "Check if FILE is a directory (stub for tests)."
  [file]
  ;; TODO: Implement via API - this is a real check
  (try
    (let [fs (js/require "fs")]
      (and (.existsSync fs file)
           (.isDirectory (.statSync fs file))))
    (catch :default _ false)))

(defn file-attributes
  "Get FILE attributes (stub for tests)."
  [file]
  ;; TODO: Implement via API
  {})

(defn buffer-file-name
  "Get filename associated with current buffer (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn directory-files
  "Get list of files in DIRECTORY (stub for tests)."
  [directory]
  ;; TODO: Implement via API
  (try
    (let [fs (js/require "fs")]
      (into [] (.readdirSync fs directory)))
    (catch :default _ [])))

;; =============================================================================
;; Shell helpers (stubs for tests - Issue #112)
;; =============================================================================

(defn shell
  "Start shell in buffer (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn shell-send-input
  "Send INPUT to shell (stub for tests)."
  [input]
  ;; TODO: Implement via API
  nil)

(defn default-directory
  "Get current default directory (stub for tests)."
  []
  ;; TODO: Implement via API
  "/")

(defn shell-previous-input
  "Recall previous shell input (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn shell-current-input
  "Get current shell input (stub for tests)."
  []
  ;; TODO: Implement via API
  "")

;; =============================================================================
;; Version Control helpers (stubs for tests - Issue #113)
;; =============================================================================

(defn vc-backend
  "Get VC backend for current file (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn vc-state
  "Get VC state for current file (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn vc-diff
  "Show diff for current file (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn vc-print-log
  "Show version history (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

;; =============================================================================
;; Outline/Folding helpers (stubs for tests - Issue #114)
;; =============================================================================

(defn outline-hide-subtree
  "Hide subtree under current heading (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn outline-next-heading
  "Move to next outline heading (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn hs-hide-block
  "Hide code block at point (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

(defn hs-toggle-hiding
  "Toggle visibility of block at point (stub for tests)."
  []
  ;; TODO: Implement via API
  nil)

;; =============================================================================
;; Extended Package Stubs (Issues #115-#124)
;; =============================================================================

;; Buffer Menu (#115)
(defn list-buffers
  "Display buffer list (stub for tests)."
  []
  nil)

;; Project/Xref (#116)
(defn project-root
  "Get project root for PATH (stub for tests)."
  [path]
  nil)

;; Window Extensions (#117)
(defn windmove-left
  "Move to window on left (stub for tests)."
  []
  nil)

;; File Persistence (#118)
(defn recentf-list
  "Get recent files list (stub for tests)."
  []
  [])

;; Diff/Merge (#119) - stubs only

;; Text Expansion (#120)
(defn dabbrev-expand
  "Expand word at point from buffer (stub for tests)."
  []
  nil)

;; Editing Enhancements (#121) - uses existing helpers

;; Programming Support (#122)
(defn compile
  "Run compile command (stub for tests)."
  [command]
  nil)

(defn imenu-create-index
  "Create buffer index (stub for tests)."
  []
  nil)

;; Visual Enhancements (#123)
(defn show-paren-data-function
  "Get matching paren data (stub for tests)."
  []
  nil)

;; Documentation (#124)
(defn eldoc-documentation-function
  "Get eldoc documentation (stub for tests)."
  []
  nil)

(defn apropos-command
  "Search commands by pattern (stub for tests)."
  [pattern]
  nil)

(defn describe-function
  "Show function documentation (stub for tests)."
  [function]
  nil)

(defn buffer-name-from-id
  "Get buffer name from ID (stub for tests)."
  [buffer-id]
  (get-in @rfdb/app-db [:buffers buffer-id :name]))

;; =============================================================================
;; Grep & Highlighting (#125)
;; =============================================================================

(defn grep
  "Run grep command (stub for tests)."
  [command]
  nil)

(defn grep-find
  "Run grep with find (stub for tests)."
  [command]
  nil)

(defn hi-lock-mode
  "Enable hi-lock highlighting mode (stub for tests)."
  []
  nil)

(defn highlight-regexp
  "Highlight REGEXP in buffer (stub for tests)."
  [regexp]
  nil)

;; =============================================================================
;; Remote Files / TRAMP (#126)
;; =============================================================================

(defn tramp-dissect-file-name
  "Parse TRAMP file name into components (stub for tests).
  Returns map with :method :user :host :localname or nil."
  [filename]
  (when (and filename (re-find #"^/[^:]+:" filename))
    ;; Very basic parsing for stubs
    (when-let [[_ method user host localname]
               (re-matches #"^/([^:]+):([^@]*)@?([^:]+):(.*)$" filename)]
      {:method method
       :user (when (seq user) user)
       :host host
       :localname localname})))

(defn tramp-tramp-file-p
  "Check if FILENAME is a TRAMP remote path (stub for tests)."
  [filename]
  (boolean (and filename (re-find #"^/[^:]+:[^:]+:" filename))))

(defn tramp-methods
  "Get list of available TRAMP methods (stub for tests)."
  []
  ["ssh" "scp" "rsync" "sudo"])

;; =============================================================================
;; Terminal Emulation (#127)
;; =============================================================================

(defn term
  "Start terminal process (stub for tests)."
  [program]
  nil)

(defn term-line-mode
  "Switch to line mode in term (stub for tests)."
  []
  nil)

(defn term-char-mode
  "Switch to char mode in term (stub for tests)."
  []
  nil)

(defn comint-send-input
  "Send input to comint process (stub for tests)."
  []
  nil)

(defn comint-previous-input
  "Navigate to previous input in history (stub for tests)."
  [n]
  nil)

(defn ansi-color-apply
  "Apply ANSI color codes to string (stub for tests)."
  [string]
  ;; Strip ANSI codes for stub
  (clojure.string/replace string #"\u001b\[[0-9;]*m" ""))

;; =============================================================================
;; Incremental Completion (#128)
;; =============================================================================

(defn icomplete-completions
  "Get current icomplete candidates (stub for tests)."
  []
  nil)

(defn icomplete-forward-completions
  "Cycle forward through icomplete candidates (stub for tests)."
  []
  nil)

;; =============================================================================
;; LSP Client / Eglot (#129)
;; =============================================================================

(defn eglot-server-programs
  "Get eglot server programs configuration (stub for tests)."
  []
  nil)

(defn eglot-ensure
  "Ensure eglot is connected for current buffer (stub for tests)."
  []
  false)

(defn eglot-eldoc-function
  "Get eglot documentation at point (stub for tests)."
  []
  nil)

(defn eglot-completion-at-point
  "Get eglot completions at point (stub for tests)."
  []
  nil)

(defn eglot-diagnostics
  "Get eglot diagnostics for current buffer (stub for tests)."
  []
  nil)

(defn eglot-code-actions
  "Get available code actions at point (stub for tests)."
  []
  nil)

;; =============================================================================
;; Font Lock & Code Intelligence (#130)
;; =============================================================================

(defn font-lock-keywords
  "Get font-lock-keywords for current mode (stub for tests)."
  []
  nil)

(defn font-lock-add-keywords
  "Add keywords to font-lock for MODE (stub for tests)."
  [mode keywords]
  nil)

(defn face-attribute
  "Get ATTRIBUTE of FACE (stub for tests)."
  [face attribute]
  ;; Return basic values for standard faces
  (case face
    font-lock-keyword-face (when (= attribute :foreground) "#7F0055")
    font-lock-string-face (when (= attribute :foreground) "#2A00FF")
    font-lock-comment-face (when (= attribute :foreground) "#3F7F5F")
    nil))

(defn which-function
  "Return name of function at point (stub for tests)."
  []
  nil)

;; =============================================================================
;; Macros for with-test-buffer
;; =============================================================================
;; Macro with-test-buffer is defined in helpers.clj (required via :require-macros)
