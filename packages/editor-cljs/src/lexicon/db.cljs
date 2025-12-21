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
                :diagnostics []                        ; LSP diagnostics for this buffer
                :undo-stack []                         ; Stack of undo entries
                :undo-in-progress? false               ; Prevent recording during undo
                :editor-version 0                      ; Increments on each edit (cache key)
                :cache {:text ""                       ; Cached full text
                        :line-count 1}}}               ; Cached line count
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
        :scroll-position 0                             ; Current scroll position
        :prefix-argument nil                           ; Universal argument (C-u)
        :prefix-argument-active? false}                ; Whether prefix argument is active
   :editor {:mode :emacs                              ; Editor mode - simple Emacs mode for now
            :keymap :emacs                             ; Active keymap
            :commands {}}                              ; Available commands
   ;; COMMENTED OUT: Evil mode / FSM - not ready yet, focus on basic Emacs first
   ;; :fsm {:current-state :insert
   ;;       :previous-state nil
   ;;       :state-context {}
   ;;       :operator-pending nil
   ;;       :motion-pending nil
   ;;       :count-register nil
   ;;       :register-name nil
   ;;       :active-keymap :insert-keymap
   ;;       :selection-mode :normal
   ;;       :selection-anchor nil
   ;;       :last-search {:pattern nil :direction :forward :case-sensitive false}
   ;;       :repeat-last-command nil
   ;;       :macro-recording nil
   ;;       :macro-registry {}
   ;;       :command-history []
   ;;       :transition-hooks {:enter {} :exit {}}}
   :keymaps {:global {"C-x C-f" :find-file             ; Find file
                     "C-x C-s" :save-buffer            ; Save buffer
                     "C-x C-w" :write-file             ; Write file (save as)
                     "C-x C-b" :list-buffers           ; List buffers
                     "C-x C-c" :save-buffers-kill-emacs ; Quit emacs
                     "C-g" :keyboard-quit              ; Keyboard quit
                     "M-x" :execute-extended-command   ; M-x command prompt
                     "C-x b" :switch-to-buffer         ; Switch buffer
                     "C-x k" :kill-buffer              ; Kill buffer
                     "C-h b" :describe-bindings        ; Describe bindings
                     "C-u" :universal-argument         ; Universal argument
                     "C-/" :undo                       ; Undo
                     "C-_" :undo                       ; Alternative undo
                     "C-w" :kill-region                ; Kill region
                     "M-w" :copy-region-as-kill        ; Copy region
                     "C-y" :yank                       ; Yank
                     "M-y" :yank-pop                   ; Yank pop (cycle kill ring)
                     "C-k" :kill-line                  ; Kill line
                     "C-o" :open-line                  ; Open line
                     "C-SPC" :set-mark-command         ; Set mark
                     "DEL" :delete-backward-char       ; Backspace
                     "DELETE" :delete-forward-char     ; Delete
                     ;; Cursor movement
                     "C-f" :forward-char               ; Forward char
                     "C-b" :backward-char              ; Backward char
                     "C-n" :next-line                  ; Next line
                     "C-p" :previous-line              ; Previous line
                     "C-a" :beginning-of-line          ; Beginning of line
                     "C-e" :end-of-line                ; End of line
                     "M-<" :beginning-of-buffer        ; Beginning of buffer
                     "M->" :end-of-buffer              ; End of buffer
                     "M-f" :forward-word               ; Forward word
                     "M-b" :backward-word              ; Backward word
                     "ArrowRight" :forward-char        ; Right arrow
                     "ArrowLeft" :backward-char        ; Left arrow
                     "ArrowDown" :next-line            ; Down arrow
                     "ArrowUp" :previous-line}         ; Up arrow
            :major {:buffer-menu-mode {"RET" :buffer-menu/select-buffer}}  ; Major mode keymaps
            :minor {}}                                 ; Minor mode keymaps

            ;; COMMENTED OUT: Evil mode keymaps - not ready yet, focus on basic Emacs first
            ;; :normal-keymap {"d" :delete-operator "w" :forward-word ...}
            ;; :insert-keymap {"Escape" :exit-insert-mode ...}
            ;; :visual-keymap {"d" :delete-region ...}
            ;; :operator-pending-keymap {"w" :forward-word ...}

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
                :on-cancel [:minibuffer/deactivate]  ; Event vector to dispatch on Escape/C-g
                :completions []                      ; List of possible completions
                :completion-index 0}                 ; Current completion selection index
   :echo-area {:message ""                           ; Current message to display
               :timeout-id nil}                      ; Timeout ID for auto-clearing message
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
   :diagnostics []
   :undo-stack []                         ; Stack of undo entries
   :undo-in-progress? false
   :editor-version 0                      ; Increments on each edit
   :cache {:text ""                       ; Cached full text
           :line-count 1}})

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
