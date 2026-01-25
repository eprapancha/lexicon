(ns lexicon.core.api.test
  "Public test API - The ONLY interface tests may use.

  This API enforces a boundary between tests and implementation.
  All operations dispatch through re-frame events, preventing tests
  from bypassing application logic.

  ⚠️  Tests MUST use only these functions.
  ⚠️  Direct access to @rfdb/app-db is FORBIDDEN in tests."
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.core.api.message :as api-msg]
            [lexicon.core.eval :as eval]))

;; =============================================================================
;; Buffer Operations
;; =============================================================================

(defn insert-text!
  "Insert text at position in buffer."
  [buffer-id position text]
  (rf/dispatch-sync [:buffer/insert buffer-id position text]))

(defn delete-region!
  "Delete text from start to end in buffer."
  [buffer-id start end]
  (rf/dispatch-sync [:buffer/delete buffer-id start end]))

(defn create-buffer!
  "Create a new buffer with name and optional content.

  Returns buffer ID."
  ([name]
   (create-buffer! name ""))
  ([name content]
   ;; Create WASM instance
   (let [WasmGapBuffer (get-in @re-frame.db/app-db [:system :wasm-constructor])
         wasm-instance (when WasmGapBuffer (new WasmGapBuffer (or content "")))]
     (when wasm-instance
       ;; Dispatch buffer creation with WASM instance
       (rf/dispatch-sync [:create-buffer name wasm-instance])
       ;; Find the buffer by name and return its ID
       (let [buffers (get-in @re-frame.db/app-db [:buffers])]
         (->> buffers
              vals
              (filter #(= name (:name %)))
              first
              :id))))))

(defn buffer-text
  "Get text content of buffer."
  [buffer-id]
  ;; TODO: Use subscription when implemented
  ;; For now, direct query (temporary)
  (let [buffer (get-in @re-frame.db/app-db [:buffers buffer-id])
        wasm-instance (:wasm-instance buffer)]
    (when wasm-instance
      (try (.getText ^js wasm-instance)
           (catch js/Error _ "")))))

(defn set-buffer-file!
  "Associate file path with buffer."
  [buffer-id file-path]
  ;; TODO: Implement :buffer/set-file event
  (rf/dispatch-sync [:buffer/set-file buffer-id file-path]))

(defn buffer-file
  "Get file path associated with buffer."
  [buffer-id]
  ;; TODO: Use subscription when implemented
  (get-in @re-frame.db/app-db [:buffers buffer-id :file-handle]))

(defn set-read-only!
  "Set read-only flag for buffer."
  [buffer-id read-only?]
  ;; TODO: Implement :buffer/set-read-only event
  (rf/dispatch-sync [:buffer/set-read-only buffer-id read-only?]))

;; =============================================================================
;; Point/Mark Operations
;; =============================================================================

(defn point
  "Get point (cursor position) for buffer."
  [buffer-id]
  ;; TODO: Use subscription when implemented
  (get-in @re-frame.db/app-db [:buffers buffer-id :point] 0))

(defn set-point!
  "Set point for buffer."
  [buffer-id position]
  (rf/dispatch-sync [:buffer/set-point buffer-id position]))

(defn mark
  "Get mark position for buffer.

  Returns nil if mark not set."
  [buffer-id]
  ;; Mark is stored in the active window, not the buffer
  (let [active-window-id (get-in @re-frame.db/app-db [:active-window-id])
        window-tree (get-in @re-frame.db/app-db [:window-tree])
        find-window (fn find-window [tree id]
                      (cond
                        (nil? tree) nil
                        (= (:id tree) id) tree
                        (= (:type tree) :leaf) nil
                        :else (or (find-window (:first tree) id)
                                  (find-window (:second tree) id))))
        window (find-window window-tree active-window-id)]
    (when window
      (let [mark-pos (:mark-position window)]
        ;; Mark is stored as {:line N :column M}, convert to linear
        (when mark-pos
          (let [buffer (get-in @re-frame.db/app-db [:buffers buffer-id])
                ^js wasm-instance (:wasm-instance buffer)
                text (when wasm-instance (.getText wasm-instance))]
            (when text
              ;; Convert line/col to linear position
              ;; TODO: Use proper conversion function
              (+ (:line mark-pos) (:column mark-pos)))))))))

(defn set-mark!
  "Set mark at position in buffer."
  [buffer-id position]
  ;; TODO: Implement proper :set-mark event
  ;; For now, use existing :window/set-mark
  (let [active-window-id (get-in @re-frame.db/app-db [:active-window-id])]
    ;; Convert linear position to line/col
    ;; TODO: Use proper conversion function
    (rf/dispatch-sync [:window/set-mark active-window-id {:line position :column 0}])))

;; =============================================================================
;; Kill Ring Operations
;; =============================================================================

(defn kill-region!
  "Kill (cut) text from start to end in buffer.

  Adds to kill ring with consecutive kill appending."
  [buffer-id start end]
  ;; TODO: Implement :edit/kill-region event
  (rf/dispatch-sync [:edit/kill-region buffer-id start end]))

(defn yank!
  "Yank (paste) from kill ring at position in buffer."
  [buffer-id position]
  ;; TODO: Implement :edit/yank event
  (rf/dispatch-sync [:edit/yank buffer-id position]))

;; =============================================================================
;; Undo Operations
;; =============================================================================

(defn undo!
  "Undo last change in buffer."
  [buffer-id]
  ;; TODO: Implement :edit/undo event
  (rf/dispatch-sync [:edit/undo buffer-id]))

(defn undo-boundary!
  "Insert undo boundary in buffer.

  Groups operations between boundaries into single undo unit."
  [buffer-id]
  (rf/dispatch-sync [:undo/boundary buffer-id]))

;; =============================================================================
;; Mode Operations
;; =============================================================================

(defn set-major-mode!
  "Set major mode for buffer."
  [buffer-id mode-symbol]
  ;; TODO: Implement :mode/set-major event
  (rf/dispatch-sync [:mode/set-major buffer-id mode-symbol]))

(defn major-mode
  "Get major mode of buffer."
  [buffer-id]
  ;; TODO: Use subscription when implemented
  (get-in @re-frame.db/app-db [:buffers buffer-id :major-mode]))

(defn enable-minor-mode!
  "Enable minor mode for buffer."
  [buffer-id mode-symbol]
  ;; TODO: Implement :mode/enable-minor event
  (rf/dispatch-sync [:mode/enable-minor buffer-id mode-symbol]))

(defn minor-mode-enabled?
  "Check if minor mode is enabled in buffer."
  [buffer-id mode-symbol]
  ;; TODO: Use subscription when implemented
  (contains? (get-in @re-frame.db/app-db [:buffers buffer-id :minor-modes] #{}) mode-symbol))

;; =============================================================================
;; Window Operations
;; =============================================================================

(defn split-window-horizontally!
  "Split active window horizontally.

  Returns new window ID."
  []
  ;; TODO: Implement :window/split event
  (rf/dispatch-sync [:window/split :horizontal]))

(defn delete-other-windows!
  "Delete all windows except active one."
  []
  ;; TODO: Implement :window/delete-others event
  (rf/dispatch-sync [:window/delete-others]))

(defn current-buffer
  "Get ID of buffer in active window."
  []
  ;; TODO: Use subscription when implemented
  (let [active-window-id (get-in @re-frame.db/app-db [:active-window-id])
        window-tree (get-in @re-frame.db/app-db [:window-tree])
        find-window (fn find-window [tree id]
                      (cond
                        (nil? tree) nil
                        (= (:id tree) id) tree
                        (= (:type tree) :leaf) nil
                        :else (or (find-window (:first tree) id)
                                  (find-window (:second tree) id))))]
    (when-let [window (find-window window-tree active-window-id)]
      (:buffer-id window))))

(defn show-buffer!
  "Show buffer in active window."
  [buffer-id]
  (rf/dispatch-sync [:window/show-buffer buffer-id]))

;; =============================================================================
;; Minibuffer Operations
;; =============================================================================

(defn activate-minibuffer!
  "Activate minibuffer with prompt.

  Returns minibuffer buffer ID."
  [prompt]
  (rf/dispatch-sync [:minibuffer/activate {:prompt prompt
                                            :on-confirm [:minibuffer/deactivate]
                                            :on-cancel [:minibuffer/deactivate]}])
  ;; TODO: Return actual minibuffer buffer ID
  ;; For now, create the minibuffer buffer explicitly
  (let [WasmGapBuffer (get-in @re-frame.db/app-db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer ""))]
    (when wasm-instance
      (rf/dispatch-sync [:create-buffer " *Minibuf-0*" wasm-instance])
      ;; Find the buffer by name and return its ID
      (let [buffers (get-in @re-frame.db/app-db [:buffers])]
        (->> buffers
             vals
             (filter #(= " *Minibuf-0*" (:name %)))
             first
             :id)))))

(defn deactivate-minibuffer!
  "Deactivate minibuffer."
  []
  ;; TODO: Implement :minibuffer/deactivate event
  (rf/dispatch-sync [:minibuffer/deactivate]))

;; =============================================================================
;; Keymap Operations
;; =============================================================================

(defn set-global-key!
  "Bind key sequence to command in global keymap."
  [key-sequence command]
  ;; TODO: Implement :keymap/set-global event
  (rf/dispatch-sync [:keymap/set-global key-sequence command]))

(defn set-buffer-local-key!
  "Bind key sequence to command in buffer-local keymap."
  [buffer-id key-sequence command]
  ;; TODO: Implement :keymap/set-local event
  (rf/dispatch-sync [:keymap/set-local buffer-id key-sequence command]))

(defn lookup-key
  "Look up command for key sequence in buffer context."
  [buffer-id key-sequence]
  ;; TODO: Use subscription when implemented
  (or (get-in @re-frame.db/app-db [:buffers buffer-id :local-keymap key-sequence])
      (get-in @re-frame.db/app-db [:keymaps :global :bindings key-sequence])))

(defn press-keys!
  "Simulate key sequence input."
  [key-sequence]
  ;; TODO: Implement :input/keys event
  (rf/dispatch-sync [:input/keys key-sequence]))

(defn in-prefix-state?
  "Check if waiting for key sequence completion."
  []
  ;; TODO: Use subscription when implemented
  (boolean (seq (get-in @re-frame.db/app-db [:pending-keys] ""))))

(defn last-invoked-command
  "Get last command invoked via key sequence."
  []
  ;; TODO: Use subscription when implemented
  (get-in @re-frame.db/app-db [:last-invoked-command]))

;; =============================================================================
;; Message Operations
;; =============================================================================

(defn message!
  "Display message in echo area and *Messages* buffer."
  [text]
  (api-msg/message text))

(defn echo-area-text
  "Get current echo area text."
  []
  ;; TODO: Use subscription when implemented
  (get-in @re-frame.db/app-db [:minibuffer :message]))

;; =============================================================================
;; Command System
;; =============================================================================

(defn register-command!
  "Register a command for testing."
  [name command-def]
  (rf/dispatch-sync [:register-command name command-def]))

(defn invoke-command!
  "Invoke command by name (symbol or keyword).

  For test synchronicity, directly dispatches the command handler
  instead of going through :execute-command which uses async fx."
  [command-name & args]
  (let [cmd-keyword (if (symbol? command-name) (keyword command-name) command-name)
        command-def (get-in @re-frame.db/app-db [:commands cmd-keyword])
        handler (:handler command-def)]
    (if handler
      ;; Directly dispatch the handler synchronously for tests
      (rf/dispatch-sync (into handler args))
      (.error js/console "Command not found:" cmd-keyword))))

;; =============================================================================
;; SCI/Lisp Operations
;; =============================================================================

(defn eval-lisp
  "Evaluate Lisp code (string).

  Returns: {:success true :result value} or {:success false :error e}"
  [code-str]
  (eval/eval-string code-str))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn save-buffer!
  "Save buffer to its associated file.

  Buffer must have a file association via set-buffer-file!."
  [buffer-id]
  (rf/dispatch-sync [:save-buffer buffer-id]))

(defn revert-buffer!
  "Revert buffer to contents from disk.

  Discards all unsaved changes and reloads from file."
  [buffer-id]
  (rf/dispatch-sync [:revert-buffer buffer-id]))

;; =============================================================================
;; Hooks
;; =============================================================================

(defn add-hook!
  "Add function to hook.

  Args:
    hook-name - Keyword identifying the hook
    hook-fn   - Function to add

  Usage:
    (add-hook! :before-change-functions my-fn)"
  [hook-name hook-fn]
  (rf/dispatch-sync [:hooks/add hook-name hook-fn]))

(defn remove-hook!
  "Remove function from hook.

  Args:
    hook-name - Keyword identifying the hook
    hook-fn   - Function to remove (must be identical? to added fn)

  Usage:
    (remove-hook! :before-change-functions my-fn)"
  [hook-name hook-fn]
  (rf/dispatch-sync [:hooks/remove hook-name hook-fn]))

(defn run-hooks!
  "Run all functions in hook with arguments.

  Args:
    hook-name - Keyword identifying the hook
    args      - Arguments to pass to hook functions

  Usage:
    (run-hooks! :before-change-functions start end)"
  [hook-name & args]
  (apply rf/dispatch-sync :hooks/run hook-name args))

;; =============================================================================
;; Text Properties
;; =============================================================================

(defn put-text-property!
  "Set text property on range in buffer.

  Properties are metadata attached to text ranges:
  - 'invisible - hide text from display
  - 'face - styling/highlighting
  - 'keymap - region-specific key bindings
  - 'mouse-face - hover highlighting
  - 'help-echo - tooltip text"
  [buffer-id start end property value]
  (rf/dispatch-sync [:text-property/put buffer-id start end property value]))

(defn get-text-property
  "Get text property value at position in buffer."
  [buffer-id position property]
  (let [props (get-in @re-frame.db/app-db [:buffers buffer-id :text-properties])]
    ;; Find the property range containing position
    (some (fn [[range-start range-data]]
            (some (fn [[range-end prop-map]]
                    (when (and (>= position range-start)
                               (< position range-end))
                      (get prop-map property)))
                  range-data))
          props)))

(defn text-properties-at
  "Get all text properties at position in buffer.

  Returns property map {:face :bold :invisible t ...}"
  [buffer-id position]
  (let [props (get-in @re-frame.db/app-db [:buffers buffer-id :text-properties])]
    (reduce
     (fn [acc [range-start range-data]]
       (reduce
        (fn [acc2 [range-end prop-map]]
          (if (and (>= position range-start)
                   (< position range-end))
            (merge acc2 prop-map)
            acc2))
        acc
        range-data))
     {}
     props)))

(defn propertize
  "Create string with text properties.

  Usage: (propertize \"text\" 'face 'bold 'help-echo \"tooltip\")

  Returns string with properties metadata."
  [text & properties]
  (let [prop-map (apply hash-map properties)]
    ;; Return as ClojureScript with-meta for property storage
    (with-meta text {:text-properties {0 {(count text) prop-map}}})
    ))

(defn get-text-property-from-string
  "Get property from propertized string."
  [string position property]
  (let [props (-> string meta :text-properties)]
    (some (fn [[range-start range-data]]
            (some (fn [[range-end prop-map]]
                    (when (and (>= position range-start)
                               (< position range-end))
                      (get prop-map property)))
                  range-data))
          props)))

(defn visible-text
  "Get visible text (respects 'invisible property).

  Returns text with invisible regions removed."
  [buffer-id]
  (let [full-text (buffer-text buffer-id)
        props (get-in @re-frame.db/app-db [:buffers buffer-id :text-properties])]
    (if (empty? props)
      full-text
      ;; Build list of invisible ranges
      (let [invisible-ranges
            (for [[start-pos range-map] props
                  [end-pos prop-map] range-map
                  :when (get prop-map 'invisible)]
              [start-pos end-pos])
            ;; Sort ranges by start position
            sorted-ranges (sort-by first invisible-ranges)]
        ;; Remove invisible text
        (loop [result []
               pos 0
               ranges sorted-ranges]
          (if (empty? ranges)
            ;; No more invisible ranges, add rest of text
            (apply str (conj result (subs full-text pos)))
            (let [[inv-start inv-end] (first ranges)]
              ;; Add visible text before this invisible range
              (recur
               (conj result (subs full-text pos inv-start))
               inv-end
               (rest ranges)))))))))
