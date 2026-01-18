(ns lexicon.lisp
  "Impure Emacs-compatible Lisp API for SCI.

  These functions are the PUBLIC API that:
  - Users call from evaluated Lisp code
  - Packages call from elisp
  - Tests call via (eval-lisp ...)
  - REPL calls interactively

  Unlike lexicon.api.buffer (pure functions taking db), these:
  - Access live editor state via @re-frame.db/app-db
  - Dispatch events synchronously for mutations
  - Return values directly (no db parameter needed)

  This is the ONLY API tests should use. If tests need something not here,
  it means we need to add it to the public API, not create test-only functions."
  (:require [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [lexicon.api.buffer :as buf]
            [lexicon.api.message :as msg]
            [lexicon.db :as db]))

;; =============================================================================
;; Buffer Query Functions (Read-Only)
;; =============================================================================

(defn point
  "Return current cursor position as linear byte offset.

  Usage: (point)
  Returns: Integer position"
  []
  (or (buf/point @rfdb/app-db) 0))

(defn point-min
  "Return minimum valid buffer position (always 0).

  Usage: (point-min)
  Returns: 0"
  []
  (buf/point-min))

(defn point-max
  "Return maximum valid buffer position (buffer length).

  Usage: (point-max)
  Returns: Integer position"
  []
  (or (buf/point-max @rfdb/app-db) 0))

(defn buffer-string
  "Return entire buffer contents as string.

  Usage: (buffer-string)
  Returns: String"
  []
  (or (buf/buffer-string @rfdb/app-db) ""))

(defn buffer-substring
  "Return text between START and END positions.

  Usage: (buffer-substring start end)
  Returns: String"
  [start end]
  (buf/buffer-substring @rfdb/app-db start end))

(defn buffer-name
  "Return name of current buffer.

  Usage: (buffer-name)
  Returns: String"
  []
  (buf/buffer-name @rfdb/app-db))

(defn buffer-size
  "Return size of current buffer in characters.

  Usage: (buffer-size)
  Returns: Integer"
  []
  (buf/buffer-size @rfdb/app-db))

(defn line-beginning-position
  "Return position at beginning of current line.

  Usage: (line-beginning-position)
  Returns: Integer position"
  []
  (buf/line-beginning-position @rfdb/app-db))

(defn line-end-position
  "Return position at end of current line.

  Usage: (line-end-position)
  Returns: Integer position"
  []
  (buf/line-end-position @rfdb/app-db))

(defn get-buffer
  "Get buffer by name.

  Usage: (get-buffer \"*scratch*\")
  Returns: Buffer ID or nil"
  [buffer-name]
  (when-let [buffer (buf/get-buffer @rfdb/app-db buffer-name)]
    (:id buffer)))

;; =============================================================================
;; Buffer Mutation Functions
;; =============================================================================

(defn insert
  "Insert TEXT at point.

  Usage: (insert \"hello\")
  Returns: nil (side effect only)"
  [text]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        pos (buf/point db)]
    (rf/dispatch-sync [:buffer/insert buffer-id pos (str text)])
    nil))

(defn delete-region
  "Delete text between START and END.

  Usage: (delete-region start end)
  Returns: nil (side effect only)"
  [start end]
  ;; TODO: Implement :edit/delete-region event
  ;; For now, we don't have a delete operation - would need to add to WASM API
  (js/console.warn "delete-region not yet implemented")
  nil)

(defn goto-char
  "Move cursor to position POS.

  Usage: (goto-char 100)
  Returns: nil (side effect only)"
  [pos]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) buffer-id)
        line-col (buf/point-to-line-col active-buffer pos)]
    (rf/dispatch-sync [:cursor/set-position line-col])
    nil))

;; =============================================================================
;; Kill Ring Functions
;; =============================================================================

(defn kill-region
  "Kill (cut) text between START and END.

  Usage: (kill-region start end)
  Returns: nil (side effect only)"
  [start end]
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (rf/dispatch-sync [:edit/kill-region buffer-id start end])
    nil))

(defn yank
  "Yank (paste) from kill ring at point.

  Usage: (yank)
  Returns: nil (side effect only)"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        pos (buf/point db)]
    (rf/dispatch-sync [:edit/yank buffer-id pos])
    nil))

;; =============================================================================
;; Mark and Region
;; =============================================================================

(defn set-mark
  "Set mark at position POS (or point if nil).

  Usage: (set-mark 100)
  Returns: nil (side effect only)"
  [pos]
  (let [db @rfdb/app-db
        actual-pos (or pos (buf/point db))
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) buffer-id)
        line-col (buf/point-to-line-col active-buffer actual-pos)]
    (rf/dispatch-sync [:mark/set line-col])
    nil))

(defn mark
  "Return position of mark, or nil if not set.

  Usage: (mark)
  Returns: Integer position or nil"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        mark-pos (:mark-position active-window)]
    (when mark-pos
      (let [buffer-id (:buffer-id active-window)
            active-buffer (get (:buffers db) buffer-id)]
        (buf/line-col-to-point active-buffer (:line mark-pos) (:column mark-pos))))))

(defn exchange-point-and-mark
  "Exchange point and mark.

  Usage: (exchange-point-and-mark)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:mark/exchange-point-and-mark])
  nil)

;; =============================================================================
;; Buffer Management
;; =============================================================================

(defn create-buffer
  "Create a new buffer with NAME.

  Usage: (create-buffer \"*scratch*\")
  Returns: Buffer ID"
  [name]
  (let [WasmGapBuffer (get-in @rfdb/app-db [:system :wasm-constructor])
        wasm-instance (when WasmGapBuffer (new WasmGapBuffer ""))]
    (when wasm-instance
      (rf/dispatch-sync [:create-buffer name wasm-instance])
      ;; Find the buffer by name and return its ID
      (let [buffers (get-in @rfdb/app-db [:buffers])]
        (->> buffers vals
             (filter #(= name (:name %)))
             first :id)))))

(defn switch-to-buffer
  "Switch to buffer NAME (create if doesn't exist).

  Usage: (switch-to-buffer \"*scratch*\")
  Returns: nil (side effect only)"
  [name]
  (let [db @rfdb/app-db
        buffer (buf/get-buffer db name)
        buffer-id (or (:id buffer) (create-buffer name))]
    (rf/dispatch-sync [:window/show-buffer buffer-id])
    nil))

(defn current-buffer
  "Return ID of current buffer.

  Usage: (current-buffer)
  Returns: Buffer ID (integer)"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))]
    (:buffer-id active-window)))

(defn buffer-name-from-id
  "Return name of buffer with ID.

  Usage: (buffer-name-from-id 5)
  Returns: String (buffer name)"
  [buffer-id]
  (let [db @rfdb/app-db
        buffer (get-in db [:buffers buffer-id])]
    (:name buffer)))

(defn buffer-list
  "Return list of all buffer names.

  Usage: (buffer-list)
  Returns: Vector of strings"
  []
  (let [db @rfdb/app-db
        buffers (:buffers db)]
    (vec (map :name (vals buffers)))))

;; =============================================================================
;; Message/Echo Area
;; =============================================================================

(defn message
  "Display MESSAGE in echo area.

  Usage: (message \"Hello, world!\")
  Returns: nil (side effect only)"
  [& args]
  (let [text (apply str args)]
    (msg/message text)
    nil))

;; =============================================================================
;; Undo
;; =============================================================================

(defn undo
  "Undo last change in current buffer.

  Usage: (undo)
  Returns: nil (side effect only)"
  []
  (let [db @rfdb/app-db
        active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (rf/dispatch-sync [:edit/undo buffer-id])
    nil))

;; =============================================================================
;; Window Management
;; =============================================================================

(defn split-window-horizontally
  "Split active window horizontally.

  Usage: (split-window-horizontally)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/split :horizontal])
  nil)

(defn delete-other-windows
  "Delete all windows except active one.

  Usage: (delete-other-windows)
  Returns: nil (side effect only)"
  []
  (rf/dispatch-sync [:window/delete-others])
  nil)

;; =============================================================================
;; Buffer-File Association
;; =============================================================================

(defn set-visited-file-name
  "Associate file path with current buffer.

  Usage: (set-visited-file-name \"/path/to/file.txt\")
  Returns: nil (side effect only)"
  [file-path]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/set-file buffer-id file-path])
    nil))

(defn buffer-file-name
  "Return file path associated with current buffer.

  Usage: (buffer-file-name)
  Returns: String (file path) or nil"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (:file-handle buffer)))

;; =============================================================================
;; Buffer Modes
;; =============================================================================

(defn set-major-mode
  "Set major mode for current buffer.

  Usage: (set-major-mode 'clojure-mode)
  Returns: nil (side effect only)"
  [mode-symbol]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:mode/set-major buffer-id mode-symbol])
    nil))

(defn major-mode
  "Return major mode of current buffer.

  Usage: (major-mode)
  Returns: Symbol (mode name)"
  []
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (:major-mode buffer)))

(defn enable-minor-mode
  "Enable minor mode for current buffer.

  Usage: (enable-minor-mode 'line-numbers-mode)
  Returns: nil (side effect only)"
  [mode-symbol]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:mode/enable-minor buffer-id mode-symbol])
    nil))

(defn minor-mode-enabled?
  "Check if minor mode is enabled in current buffer.

  Usage: (minor-mode-enabled? 'line-numbers-mode)
  Returns: Boolean"
  [mode-symbol]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)
        buffer (get-in db [:buffers buffer-id])]
    (contains? (:minor-modes buffer #{}) mode-symbol)))

;; =============================================================================
;; Buffer Properties
;; =============================================================================

(defn set-buffer-read-only
  "Set read-only flag for current buffer.

  Usage: (set-buffer-read-only t)
  Returns: nil (side effect only)"
  [read-only?]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:buffer/set-read-only buffer-id read-only?])
    nil))

;; =============================================================================
;; File Operations
;; =============================================================================

(defn save-buffer
  "Save current buffer to its associated file.

  Usage: (save-buffer)
  Returns: nil (side effect only)"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:save-buffer buffer-id])
    nil))

(defn revert-buffer
  "Revert current buffer to contents from disk.

  Usage: (revert-buffer)
  Returns: nil (side effect only)"
  []
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:revert-buffer buffer-id])
    nil))

;; =============================================================================
;; Minibuffer
;; =============================================================================

(defn read-string
  "Read a string from minibuffer with PROMPT.

  Usage: (read-string \"Enter text: \")
  Returns: String (user input)"
  [prompt]
  ;; TODO: This needs to be async or return a promise
  ;; For now, synchronous version for tests
  (rf/dispatch-sync [:minibuffer/activate {:prompt prompt
                                            :on-confirm [:minibuffer/deactivate]
                                            :on-cancel [:minibuffer/deactivate]}])
  ;; Return empty string for now - real implementation needs completion
  "")

;; =============================================================================
;; Keymaps
;; =============================================================================

(defn global-set-key
  "Bind KEY-SEQUENCE to COMMAND in global keymap.

  Usage: (global-set-key \"C-c C-c\" 'my-command)
  Returns: nil (side effect only)"
  [key-sequence command]
  (rf/dispatch-sync [:keymap/set-global key-sequence command])
  nil)

(defn local-set-key
  "Bind KEY-SEQUENCE to COMMAND in current buffer's local keymap.

  Usage: (local-set-key \"C-c C-c\" 'my-command)
  Returns: nil (side effect only)"
  [key-sequence command]
  (let [buffer-id (current-buffer)]
    (rf/dispatch-sync [:keymap/set-local buffer-id key-sequence command])
    nil))

(defn key-binding
  "Look up KEY-SEQUENCE and return bound command.

  Usage: (key-binding \"C-c C-c\")
  Returns: Command symbol or nil"
  [key-sequence]
  (let [db @rfdb/app-db
        buffer-id (current-buffer)]
    (or (get-in db [:buffers buffer-id :local-keymap key-sequence])
        (get-in db [:keymaps :global :bindings key-sequence]))))

;; =============================================================================
;; Commands
;; =============================================================================

(defn call-interactively
  "Call COMMAND interactively.

  Usage: (call-interactively 'save-buffer)
  Returns: Result of command"
  [command-name]
  (let [db @rfdb/app-db
        cmd-keyword (if (symbol? command-name) (keyword command-name) command-name)
        command-def (get-in db [:commands cmd-keyword])
        handler (:handler command-def)]
    (when handler
      (rf/dispatch-sync handler))))

;; =============================================================================
;; Echo Area
;; =============================================================================

(defn current-message
  "Return current message in echo area.

  Usage: (current-message)
  Returns: String or nil"
  []
  (let [db @rfdb/app-db]
    (get-in db [:minibuffer :message])))

;; =============================================================================
;; Export for SCI
;; =============================================================================

(def sci-namespace
  "Namespace map for SCI context.

  This is the complete public Lisp API."
  {;; Point and position
   'point point
   'point-min point-min
   'point-max point-max
   'goto-char goto-char
   'line-beginning-position line-beginning-position
   'line-end-position line-end-position
   ;; Buffer content
   'buffer-string buffer-string
   'buffer-substring buffer-substring
   'insert insert
   'delete-region delete-region
   ;; Buffer info
   'buffer-name buffer-name
   'buffer-size buffer-size
   'current-buffer current-buffer
   'buffer-name-from-id buffer-name-from-id
   'buffer-list buffer-list
   'get-buffer get-buffer
   ;; Buffer management
   'create-buffer create-buffer
   'switch-to-buffer switch-to-buffer
   ;; Kill ring
   'kill-region kill-region
   'yank yank
   ;; Mark
   'set-mark set-mark
   'mark mark
   'exchange-point-and-mark exchange-point-and-mark
   ;; Undo
   'undo undo
   ;; Windows
   'split-window-horizontally split-window-horizontally
   'delete-other-windows delete-other-windows
   ;; Files
   'set-visited-file-name set-visited-file-name
   'buffer-file-name buffer-file-name
   'save-buffer save-buffer
   'revert-buffer revert-buffer
   ;; Modes
   'set-major-mode set-major-mode
   'major-mode major-mode
   'enable-minor-mode enable-minor-mode
   'minor-mode-enabled? minor-mode-enabled?
   ;; Buffer properties
   'set-buffer-read-only set-buffer-read-only
   ;; Minibuffer
   'read-string read-string
   ;; Keymaps
   'global-set-key global-set-key
   'local-set-key local-set-key
   'key-binding key-binding
   ;; Commands
   'call-interactively call-interactively
   ;; Messages
   'message message
   'current-message current-message})
