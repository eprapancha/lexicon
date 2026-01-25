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
            [clojure.string :as str]
            [lexicon.core.api.buffer :as buf]
            [lexicon.core.api.message :as msg]
            [lexicon.core.db :as db]))

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
;; Search Functions
;; =============================================================================

(defn- case-fold-search?
  "Return true if search should be case-insensitive.
   Emacs convention: case-insensitive if search string is all lowercase."
  [string]
  (= string (str/lower-case string)))

(defn search-forward
  "Search forward for STRING starting after point.
   If found, move point to end of match and return point.
   If not found, return nil and leave point unchanged.

   Optional BOUND limits search to that position.
   Optional NOERROR if non-nil suppresses error (default behavior).
   Optional COUNT searches for Nth occurrence.

  Usage: (search-forward \"pattern\")
         (search-forward \"pattern\" limit)
  Returns: Position after match or nil"
  ([string] (search-forward string nil true 1))
  ([string bound] (search-forward string bound true 1))
  ([string bound noerror] (search-forward string bound noerror 1))
  ([string bound _noerror _count]
   (when-not (str/blank? string)
     (let [text (buffer-string)
           current-pos (point)
           case-fold? (case-fold-search? string)
           search-text (if case-fold? (str/lower-case text) text)
           search-term (if case-fold? (str/lower-case string) string)
           text-from-pos (subs search-text current-pos)
           effective-bound (or bound (count text))
           match-index (.indexOf text-from-pos search-term)]
       (when (and (>= match-index 0)
                  (<= (+ current-pos match-index (count string)) effective-bound))
         (let [match-end (+ current-pos match-index (count string))]
           (rf/dispatch-sync [:cursor/set-position match-end])
           match-end))))))

(defn search-backward
  "Search backward for STRING starting before point.
   If found, move point to beginning of match and return point.
   If not found, return nil and leave point unchanged.

   Optional BOUND limits search to that position.
   Optional NOERROR if non-nil suppresses error (default behavior).
   Optional COUNT searches for Nth occurrence.

  Usage: (search-backward \"pattern\")
         (search-backward \"pattern\" limit)
  Returns: Position at start of match or nil"
  ([string] (search-backward string nil true 1))
  ([string bound] (search-backward string bound true 1))
  ([string bound noerror] (search-backward string bound noerror 1))
  ([string bound _noerror _count]
   (when-not (str/blank? string)
     (let [text (buffer-string)
           current-pos (point)
           case-fold? (case-fold-search? string)
           search-text (if case-fold? (str/lower-case text) text)
           search-term (if case-fold? (str/lower-case string) string)
           text-before-pos (subs search-text 0 current-pos)
           effective-bound (or bound 0)
           match-index (.lastIndexOf text-before-pos search-term)]
       (when (and (>= match-index 0)
                  (>= match-index effective-bound))
         (rf/dispatch-sync [:cursor/set-position match-index])
         match-index)))))

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

(def ^:private lisp-command-registry
  "Registry of Lisp command functions.
   Maps command keywords to their implementing functions."
  (atom {}))

(defn define-command
  "Define a command that can be invoked via M-x.

  This is how packages register commands with the editor.
  The function will be called when the command is executed.

  Usage: (define-command 'my-command my-fn \"Description\" {:interactive ...})
  Returns: nil

  Options:
  - :interactive - Interactive spec for prompting (optional)
    Example: [{:type :async :prompt \"Directory: \"}]"
  ([command-name command-fn docstring]
   (define-command command-name command-fn docstring {}))
  ([command-name command-fn docstring options]
   (let [cmd-keyword (if (symbol? command-name) (keyword command-name) command-name)]
     (rf/dispatch-sync [:register-command cmd-keyword
                        (merge {:docstring docstring
                                :handler [:run-lisp-command cmd-keyword]}
                               (when (:interactive options)
                                 {:interactive (:interactive options)}))])
     ;; Store the function in a registry for :run-lisp-command to find
     (swap! lisp-command-registry assoc cmd-keyword command-fn)
     nil)))

(defn run-lisp-command
  "Execute a Lisp command by keyword.
   Called by :run-lisp-command event handler.

  Usage: (run-lisp-command :dired \"/home\")
  Returns: Result of command function"
  [command-keyword & args]
  (when-let [command-fn (get @lisp-command-registry command-keyword)]
    (apply command-fn args)))

;; Register the event handler for running lisp commands
;; This is done here to avoid circular deps with events/command.cljs
(rf/reg-event-fx
 :run-lisp-command
 (fn [_ [_ command-keyword & args]]
   (apply run-lisp-command command-keyword args)
   {}))

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
;; Filesystem Primitives (for packages like Dired)
;; =============================================================================

;; Note: These use a mock filesystem for now. Real filesystem access
;; will be implemented via File System Access API when available.

(def ^:private mock-filesystem
  "Mock filesystem for testing Dired and other packages.
   In production, this will be replaced with real FS access."
  {"/" [{:name "home" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "usr" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "tmp" :type :directory :size 4096 :modified "2025-01-20"}
        {:name "etc" :type :directory :size 4096 :modified "2025-01-20"}]
   "/home" [{:name "user" :type :directory :size 4096 :modified "2025-01-20"}]
   "/home/user" [{:name "documents" :type :directory :size 4096 :modified "2025-01-20"}
                 {:name "file.txt" :type :file :size 1234 :modified "2025-01-20"}
                 {:name "notes.org" :type :file :size 5678 :modified "2025-01-20"}]})

(defn directory-files
  "Return list of files in DIRECTORY.

  Usage: (directory-files \"/home/user\")
  Returns: Vector of filename strings"
  [directory]
  (let [entries (get mock-filesystem directory [])]
    (mapv :name entries)))

(defn file-attributes
  "Return attributes of FILE as a map.

  Usage: (file-attributes \"/home/user/file.txt\")
  Returns: {:name \"file.txt\" :type :file :size 1234 :modified \"...\"}"
  [file-path]
  (let [parent-dir (or (re-find #".*/" file-path) "/")
        filename (last (str/split file-path #"/"))
        entries (get mock-filesystem (str/replace parent-dir #"/$" "") [])]
    (first (filter #(= (:name %) filename) entries))))

(defn file-directory-p
  "Return true if PATH is a directory.

  Usage: (file-directory-p \"/home\")
  Returns: Boolean"
  [path]
  (let [attrs (file-attributes path)]
    (= (:type attrs) :directory)))

(defn file-exists-p
  "Return true if FILE exists.

  Usage: (file-exists-p \"/home/user/file.txt\")
  Returns: Boolean"
  [file-path]
  (some? (file-attributes file-path)))

(defn insert-directory
  "Insert directory listing for DIR at point.

  This is the core primitive for Dired buffer generation.
  Formats entries in ls -l style.

  Usage: (insert-directory \"/home/user\")
  Returns: nil (side effect: inserts text)"
  [directory]
  (let [entries (get mock-filesystem directory [])
        header (str "  " directory ":\n")
        pad-left (fn [s width]
                   (let [s (str s)
                         pad (- width (count s))]
                     (if (pos? pad)
                       (str (apply str (repeat pad " ")) s)
                       s)))
        format-entry (fn [{:keys [name type size modified]}]
                       (let [type-char (if (= type :directory) "d" "-")
                             perms "rwxr-xr-x"]
                         (str "  " type-char perms "  "
                              (pad-left size 6) "  "
                              modified "  " name)))
        lines (map format-entry entries)
        content (str header (str/join "\n" lines) "\n")]
    (insert content)))

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
   ;; Search
   'search-forward search-forward
   'search-backward search-backward
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
   'current-message current-message
   ;; Filesystem (for packages like Dired)
   'directory-files directory-files
   'file-attributes file-attributes
   'file-directory-p file-directory-p
   'file-exists-p file-exists-p
   'insert-directory insert-directory})
