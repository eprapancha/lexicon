(ns lexicon.api.test
  "Public test API - The ONLY interface tests may use.

  This API enforces a boundary between tests and implementation.
  All operations dispatch through re-frame events, preventing tests
  from bypassing application logic.

  ⚠️  Tests MUST use only these functions.
  ⚠️  Direct access to @rfdb/app-db is FORBIDDEN in tests."
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.api.message :as api-msg]))

;; =============================================================================
;; Buffer Operations
;; =============================================================================

(defn insert-text!
  "Insert text at position in buffer."
  [buffer-id position text]
  (rf/dispatch-sync [:buffer/insert buffer-id position text]))

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

;; =============================================================================
;; Minibuffer Operations
;; =============================================================================

(defn activate-minibuffer!
  "Activate minibuffer with prompt.

  Returns minibuffer buffer ID."
  [prompt]
  ;; TODO: Implement :minibuffer/activate event
  (rf/dispatch-sync [:minibuffer/activate prompt]))

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
      (get-in @re-frame.db/app-db [:keymaps :global key-sequence])))

(defn press-keys!
  "Simulate key sequence input."
  [key-sequence]
  ;; TODO: Implement :input/keys event
  (rf/dispatch-sync [:input/keys key-sequence]))

(defn in-prefix-state?
  "Check if waiting for key sequence completion."
  []
  ;; TODO: Use subscription when implemented
  (seq (get-in @re-frame.db/app-db [:pending-keys])))

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
  "Invoke command by name."
  [command-name & args]
  (apply rf/dispatch-sync (into [:execute-command command-name] args)))
