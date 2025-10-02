(ns lexicon.db
  (:require [lexicon.constants :as const]))

(def default-db
  "Default application state database for re-frame"
  {:buffers {1 {:id 1
                :wasm-instance nil                     ; Will be set when WASM loads
                :file-handle nil
                :name "*scratch*"
                :is-modified? false
                :mark-position nil                     ; For region selection
                :cursor-position {:line 0 :column 0}  ; Cursor position as line/column coordinates
                :selection-range nil                   ; Selection range for future use
                :major-mode :fundamental-mode          ; Active major mode
                :minor-modes #{}                       ; Set of active minor modes
                :buffer-local-vars {}                  ; Mode-specific configuration
                :ast nil                               ; Parsed AST from Tree-sitter
                :language :text                        ; Language for syntax highlighting
                :diagnostics []}}                      ; LSP diagnostics for this buffer
   :windows {1 {:id 1, :buffer-id 1, :viewport {:start-line 0, :end-line const/DEFAULT_VIEWPORT_LINES}}}
   :active-window-id 1
   :line-height const/DEFAULT_LINE_HEIGHT
   :kill-ring []                                       ; Clipboard history
   :initialized? false                                 ; Whether WASM module is loaded
   :commands {}                                        ; Central command registry
   :hooks {:before-save-hook []                      ; Commands to run before saving
           :after-save-hook []                       ; Commands to run after saving
           :buffer-list-update-hook []               ; Commands to run when buffer list changes
           :kill-buffer-hook []}                     ; Commands to run before killing a buffer
   :ui {:cursor-position 0                            ; Current cursor position
        :selection {:start 0 :end 0}                  ; Current selection range
        :ime-composing? false                          ; IME composition state
        :ime-composition-text ""                       ; Current IME composition text
        :view-needs-update? false                      ; Flag to trigger view reconciliation
        :text-cache {}                                 ; Text range cache for performance
        :viewport {:start 0 :end 1000}                ; Currently visible text range
        :scroll-position 0}                            ; Current scroll position
   :editor {:mode :normal                             ; Editor mode (normal, insert, etc.)
            :keymap :emacs                             ; Active keymap
            :commands {}}                              ; Available commands
   :fsm {:current-state :insert                        ; Active FSM state (normal, insert, visual, etc.)
         :previous-state nil                           ; Previous state for transitions
         :operator-pending nil                         ; Pending operator function (e.g., delete command d)
         :active-keymap :normal-keymap}                ; Current keymap for the active state
   :keymaps {:global {"C-x C-f" :find-file             ; Find file
                     "C-x C-s" :save-buffer            ; Save buffer
                     "C-x C-c" :save-buffers-kill-emacs ; Quit emacs
                     "C-g" :keyboard-quit              ; Keyboard quit
                     "M-x" :execute-extended-command   ; M-x command prompt
                     "C-x b" :switch-to-buffer         ; Switch buffer
                     "C-x k" :kill-buffer              ; Kill buffer
                     "C-/" :undo                       ; Undo
                     "C-_" :undo                       ; Alternative undo
                     "C-w" :kill-region                ; Kill region
                     "M-w" :copy-region-as-kill        ; Copy region
                     "C-y" :yank                       ; Yank
                     "DEL" :delete-backward-char        ; Backspace
                     "DELETE" :delete-forward-char}     ; Delete
            :major {}                                  ; Major mode keymaps
            :minor {}                                  ; Minor mode keymaps
            
            ;; Legacy keymaps for backward compatibility
            :normal-keymap {"d" :delete-operator      ; Delete operator
                            "w" :forward-word          ; Forward word motion
                            "b" :backward-word         ; Backward word motion
                            "h" :backward-char         ; Backward character
                            "j" :next-line             ; Next line
                            "k" :previous-line         ; Previous line
                            "l" :forward-char          ; Forward character
                            "i" :enter-insert-mode     ; Enter insert mode
                            "a" :append-after-cursor   ; Append after cursor
                            "A" :append-end-of-line    ; Append at end of line
                            "o" :open-line-below       ; Open line below
                            "O" :open-line-above       ; Open line above
                            "x" :delete-char           ; Delete character under cursor
                            "v" :enter-visual-mode     ; Enter visual mode
                            "y" :yank-operator         ; Yank (copy) operator
                            "p" :paste-after           ; Paste after cursor
                            "P" :paste-before          ; Paste before cursor
                            "u" :undo                  ; Undo
                            "Ctrl+r" :redo             ; Redo
                            "gg" :goto-first-line      ; Go to first line
                            "G" :goto-last-line       ; Go to last line
                            "0" :beginning-of-line     ; Beginning of line
                            "$" :end-of-line           ; End of line
                            "Escape" :normal-mode}     ; Ensure normal mode
            :insert-keymap {"Escape" :exit-insert-mode  ; Exit insert mode
                           "Ctrl+c" :exit-insert-mode  ; Alternative exit
                           "Backspace" :delete-backward-char
                           "Delete" :delete-forward-char
                           "Enter" :newline
                           "Tab" :insert-tab}
            :visual-keymap {"d" :delete-region          ; Delete selected region
                           "y" :yank-region            ; Yank selected region
                           "x" :delete-region          ; Delete selected region (same as d)
                           "c" :change-region          ; Change selected region
                           "h" :extend-backward-char   ; Extend selection backward
                           "j" :extend-next-line       ; Extend selection down
                           "k" :extend-previous-line   ; Extend selection up
                           "l" :extend-forward-char    ; Extend selection forward
                           "w" :extend-forward-word    ; Extend selection by word
                           "b" :extend-backward-word   ; Extend selection backward by word
                           "0" :extend-beginning-of-line ; Extend to beginning of line
                           "$" :extend-end-of-line     ; Extend to end of line
                           "Escape" :exit-visual-mode  ; Exit visual mode
                           "Ctrl+c" :exit-visual-mode} ; Alternative exit
            :operator-pending-keymap {"w" :forward-word  ; Forward word motion
                                     "b" :backward-word ; Backward word motion
                                     "h" :backward-char ; Backward character
                                     "j" :next-line     ; Next line
                                     "k" :previous-line ; Previous line
                                     "l" :forward-char  ; Forward character
                                     "0" :beginning-of-line ; Beginning of line
                                     "$" :end-of-line   ; End of line
                                     "gg" :goto-first-line ; Go to first line
                                     "G" :goto-last-line ; Go to last line
                                     "Escape" :cancel-operator}} ; Cancel pending operator
   :system {:last-transaction-id 0                    ; For transaction ordering
            :mutation-observer nil                     ; MutationObserver instance
            :reconciliation-active? false              ; Prevent recursive reconciliation
            :parser-worker nil                         ; Web Worker instance for Tree-sitter
            :parser-worker-ready? false}               ; Whether parser worker is initialized
   :bridge {:ws nil                                   ; WebSocket connection to lexicon-bridge
            :status :disconnected                      ; Connection status (:disconnected, :connecting, :connected)
            :url "ws://localhost:30303"                ; Bridge server URL
            :retry-count 0                             ; Number of connection retry attempts
            :max-retries 5}                            ; Maximum retry attempts
   :transaction-queue []                              ; Queue for pending transactions
   :transaction-in-flight? false                     ; Flag to prevent concurrent transactions
   :minibuffer {:active? false                       ; Whether minibuffer is currently active
                :prompt ""                           ; Prompt text (e.g., "M-x ")
                :input ""                            ; Current user input in minibuffer
                :on-confirm nil                      ; Event vector to dispatch on Enter
                :on-cancel [:minibuffer/deactivate]} ; Event vector to dispatch on Escape/C-g
   })

(defn create-buffer
  "Create a new buffer data structure"
  [buffer-id name wasm-instance]
  {:id buffer-id
   :wasm-instance wasm-instance
   :file-handle nil
   :name name
   :is-modified? false
   :mark-position nil
   :cursor-position {:line 0 :column 0}  ; Cursor position as line/column coordinates
   :selection-range nil                   ; Selection range for future use
   :major-mode :fundamental-mode
   :minor-modes #{}
   :buffer-local-vars {}
   :ast nil                               ; Parsed AST from Tree-sitter
   :language :text                        ; Language for syntax highlighting
   :diagnostics []})

(defn create-buffer-with-content
  "Create a new buffer with initial content"
  [buffer-id name content]
  (let [wasm-instance (when content
                        ;; Will need to create WasmEditorCore instance
                        ;; For now, return the structure and set instance later
                        nil)]
    (create-buffer buffer-id name wasm-instance)))

(defn next-buffer-id
  "Generate next available buffer ID"
  [buffers]
  (inc (apply max 0 (keys buffers))))
