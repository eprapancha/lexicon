(ns lexicon.db
  (:require [lexicon.constants :as const]))

(def default-db
  "Default application state database for re-frame

  STATE OWNERSHIP RULES:
  Each state path is OWNED by exactly ONE module. Other modules MUST dispatch
  events to the owner instead of directly manipulating state. Violating this
  creates tight coupling, debugging nightmares, and regression risk.

  OWNERSHIP MAP:
  - :buffers            → buffer.cljs    (use :create-buffer, :buffer/set-mode, :buffer/increment-version)
  - :window-tree        → ui.cljs        (use :window/set-mark, :window/set-cursor, :split-window-*, :delete-window)
  - :ui :cursor-position → edit.cljs    (use :update-cursor-position, :set-cursor-position)
  - :minibuffer-stack   → ui.cljs        (use :minibuffer/activate, :minibuffer/deactivate, :minibuffer/set-input - Phase 6.5 Week 3-4)
  - :minibuffer         → ui.cljs        (DEPRECATED - use :minibuffer-stack - kept for backward compat during migration)
  - :echo-area          → ui.cljs        (use :echo/message, :echo/clear)
  - :kill-ring          → edit.cljs      (only kill/yank commands should update)
  - :prefix-arg         → command.cljs   (use :set-prefix-arg, :clear-prefix-arg - Phase 6.5)
  - :current-prefix-arg → command.cljs   (read-only during command execution - Phase 6.5)
  - :keymaps            → keymap.cljs    (use :define-key, :set-keymap-parent)
  - :commands           → command.cljs   (use :register-command)
  - :hooks              → hooks.cljs     (use :register-hook, :enable-hook, :disable-hook)
  - :packages           → packages/loader.cljs (use :load-package, :unload-package)

  BUFFER METADATA (within :buffers buffer-id):
  - :editor-version     → buffer.cljs    (use :buffer/increment-version)
  - :undo-in-progress?  → buffer.cljs    (use :buffer/set-undo-in-progress)
  - :major-mode         → mode.cljs      (use :set-major-mode)

  WINDOW-SPECIFIC STATE (within :window-tree leaf nodes):
  - :mark-position      → ui.cljs        (use :window/set-mark - dispatched from edit.cljs mark commands)
  - :cursor-position    → ui.cljs        (use :window/set-cursor - window cursor stored as line/col, synced with :ui :cursor-position)

  PROGRESS (Issue #60 - Package Isolation + State Ownership):
  ✅ Phase 1: Minibuffer/echo-area violations fixed (3 violations)
  ✅ Phase 2: Mark management API created + violations fixed (7 violations)
  ✅ Phase 3: Buffer metadata violations fixed (1 violation)
  ✅ Phase 4: Documentation updated in db.cljs

  See /tmp/state-management-audit.md for original violation analysis (62 violations as of 2026-01-11 AM)
  See .CLAUDE.md Architecture Principles section for detailed ownership rules"
  {:buffers {1 {:id 1
                :wasm-instance nil                     ; Will be set when WASM loads
                :file-handle nil
                :name "*scratch*"
                :is-modified? false
                :is-read-only? false                   ; Phase 6B: Read-only buffer flag
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
                        :line-count 1}
                ;; Phase 6B Week 2: Text Properties & Overlays
                :text-properties {}                    ; Map of position ranges to property maps
                                                       ; {start-pos {end-pos {:face :highlight :read-only true}}}
                :overlays {}                           ; Map of overlay-id to overlay data
                                                       ; {1 {:id 1 :start 10 :end 20 :face :highlight :priority 0}}
                :next-overlay-id 1}}               ; Counter for generating overlay IDs
   ;; Window tree structure (binary tree of splits)
   :window-tree {:type :leaf
                 :id 1
                 :buffer-id 1
                 :cursor-position {:line 0 :column 0}  ; Each window has its own cursor
                 :mark-position nil                     ; Each window has its own mark
                 :viewport {:start-line 0 :end-line const/DEFAULT_VIEWPORT_LINES}
                 :dimensions {:x 0 :y 0 :width 100 :height 100}}
   :active-window-id 1
   :cursor-owner 1    ; Issue #62: Cursor singleton - which window/component owns cursor (window-id or :minibuffer)
   :next-window-id 2  ; Counter for generating new window IDs
   :line-height const/DEFAULT_LINE_HEIGHT
   :kill-ring []                                       ; Clipboard history
   :initialized? false                                 ; Whether WASM module is loaded
   :commands {}                                        ; Central command registry
   ;; Phase 6.5 Week 1-2: Prefix Arguments (Issue #76)
   ;; Raw prefix argument during accumulation (nil, number, '-, or list like (4), (16))
   :prefix-arg nil
   ;; Saved prefix-arg during command execution (for commands to access)
   :current-prefix-arg nil
   ;; Active transient keymap (for C-u accumulation)
   :transient-keymap nil
   ;; Phase 7.3: Hook System - Priority-based hook registry
   ;; Each hook is a vector of hook entries sorted by priority (lowest first)
   ;; Hook entry: {:id keyword :priority int :fn ifn :enabled? bool :package-id keyword :doc string}
   :hooks {:before-command-hook []                   ; Run before command execution
           :after-command-hook []                    ; Run after command execution
           :before-change-hook []                    ; Run before buffer text changes
           :after-change-hook []                     ; Run after buffer text changes
           :buffer-switch-hook []                    ; Run when switching buffers
           :mode-hook []                             ; Run when modes are enabled/disabled
           ;; Legacy hooks (to be migrated to new system)
           :before-save-hook []                      ; Run before saving
           :after-save-hook []                       ; Run after saving
           :buffer-list-update-hook []               ; Run when buffer list changes
           :kill-buffer-hook []}                     ; Run before killing a buffer
   :ui {:cursor-position 0                            ; Current cursor position
        :selection {:start 0 :end 0}                  ; Current selection range
        :ime-composing? false                          ; IME composition state
        :ime-composition-text ""                       ; Current IME composition text
        :view-needs-update? false                      ; Flag to trigger view reconciliation
        :text-cache {}                                 ; Text range cache for performance
        :viewport {:start 0 :end 1000}                ; Currently visible text range
        :scroll-position 0                             ; Current scroll position
        :show-line-numbers? true                       ; Show line numbers in status bar (Phase 5)
        :show-column-number? true}                     ; Show column number in status bar (Phase 5)
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
   ;; Keymaps with parent chain support
   ;; Structure: {:global {:parent nil :bindings {...}}
   ;;             :major {:text-mode {:parent [:global] :bindings {...}}}
   ;;             :minor {:linum-mode {:parent nil :bindings {...}}}}
   :keymaps {:global {:parent nil
                      :bindings {"C-x C-f" :find-file             ; Find file
                                 "C-x C-s" :save-buffer            ; Save buffer
                                 "C-x C-w" :write-file             ; Write file (save as)
                                 "C-x C-v" :find-alternate-file    ; Visit alternate file
                                 "C-x i" :insert-file              ; Insert file at point
                                 "C-x s" :save-some-buffers        ; Save some buffers
                                 "C-x C-b" :list-buffers           ; List buffers
                                 "C-x C-c" :save-buffers-kill-emacs ; Quit emacs
                                 "C-g" :keyboard-quit              ; Keyboard quit
                                 "M-x" :execute-extended-command   ; M-x command prompt
                                 "C-x b" :switch-to-buffer         ; Switch buffer
                                 "C-x k" :kill-buffer              ; Kill buffer
                                 "C-x 2" :split-window-below       ; Split horizontally
                                 "C-x 3" :split-window-right       ; Split vertically
                                 "C-x 0" :delete-window            ; Delete current window
                                 "C-x 1" :delete-other-windows     ; Delete all other windows
                                 "C-x o" :other-window             ; Switch to next window
                                 "C-h b" :describe-bindings        ; Describe bindings
                                 "C-h k" :describe-key             ; Describe key
                                 "C-h f" :describe-function        ; Describe function
                                 "C-h v" :describe-variable        ; Describe variable
                                 "C-h a" :apropos-command          ; Search commands
                                 "C-h ?" :help-for-help            ; Help menu
                                 "C-u" :universal-argument         ; Universal argument
                                 "C-/" :undo                       ; Undo
                                 "C-_" :undo                       ; Alternative undo
                                 "C-w" :kill-region                ; Kill region
                                 "M-w" :copy-region-as-kill        ; Copy region
                                 "C-Insert" :copy-region-as-kill   ; Alternative (M-w blocked by some browsers)
                                 "C-y" :yank                       ; Yank
                                 "S-Insert" :yank                  ; Alternative yank
                                 "M-y" :yank-pop                   ; Yank pop (cycle kill ring)
                                 "C-k" :kill-line                  ; Kill line
                                 "M-d" :kill-word                  ; Kill word forward
                                 "M-DEL" :backward-kill-word       ; Kill word backward
                                 "M-%" :query-replace              ; Query replace
                                 "C-s" :isearch-forward            ; Incremental search forward
                                 "C-r" :isearch-backward           ; Incremental search backward
                                 "C-o" :open-line                  ; Open line
                                 "C-SPC" :set-mark-command         ; Set mark
                                 "C-x C-x" :exchange-point-and-mark ; Exchange point and mark
                                 "DEL" :delete-backward-char       ; Backspace
                                 "DELETE" :delete-forward-char     ; Delete
                                 ;; Cursor movement
                                 "C-f" :forward-char               ; Forward char
                                 "C-b" :backward-char              ; Backward char
                                 "C-n" :next-line                  ; Next line
                                 "C-p" :previous-line              ; Previous line
                                 "C-a" :beginning-of-line          ; Beginning of line
                                 "C-e" :end-of-line                ; End of line
                                 "C-v" :scroll-up-command          ; Scroll forward one screen
                                 "M-v" :scroll-down-command        ; Scroll backward one screen
                                 "PageDown" :scroll-up-command     ; Alternative (M-v blocked by some browsers)
                                 "PageUp" :scroll-down-command     ; Alternative (M-v blocked by some browsers)
                                 "M-<" :beginning-of-buffer        ; Beginning of buffer
                                 "M->" :end-of-buffer              ; End of buffer
                                 "M-f" :forward-word               ; Forward word
                                 "M-b" :backward-word              ; Backward word
                                 "ArrowRight" :forward-char        ; Right arrow
                                 "ArrowLeft" :backward-char        ; Left arrow
                                 "ArrowDown" :next-line            ; Down arrow
                                 "ArrowUp" :previous-line}}        ; Up arrow
             :major {:buffer-menu-mode {:parent [:global]
                                        :bindings {"RET" :buffer-menu/select-buffer}}}  ; Major mode keymaps
             :minor {}                                  ; Minor mode keymaps
             :transient {:universal-argument-map {:parent nil
                                                  :bindings {"C-u" :universal-argument-more
                                                            "0" [:digit-argument 0]
                                                            "1" [:digit-argument 1]
                                                            "2" [:digit-argument 2]
                                                            "3" [:digit-argument 3]
                                                            "4" [:digit-argument 4]
                                                            "5" [:digit-argument 5]
                                                            "6" [:digit-argument 6]
                                                            "7" [:digit-argument 7]
                                                            "8" [:digit-argument 8]
                                                            "9" [:digit-argument 9]
                                                            "-" :negative-argument}}}}

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

   ;; Phase 6.5 Week 3-4: Minibuffer Stack (Issue #77 - fixes query-replace-regexp timeout)
   ;; Stack of minibuffer frames, where each frame represents one minibuffer activation
   ;; Empty stack [] means minibuffer is inactive
   ;; Stack depth = (count minibuffer-stack) - shows nesting level
   :minibuffer-stack []
   ;; Frame structure: {:prompt "" :input "" :on-confirm [...] :on-cancel [...]
   ;;                   :completions [] :completion-index 0 :metadata nil :persist? false
   ;;                   :show-completions? false :filtered-completions [] :height-lines 1}

   ;; Configuration for recursive minibuffer support (Phase 6.5 Week 3-4)
   :enable-recursive-minibuffers false              ; Allow minibuffer activation while already active

   ;; DEPRECATED: Old single-state minibuffer (Phase 6.5 Week 3-4)
   ;; Kept for backward compatibility during migration - will be removed after all consumers updated
   :minibuffer {:active? false                       ; Whether minibuffer is currently active
                :prompt ""                           ; Prompt text (e.g., "M-x ")
                :input ""                            ; Current user input in minibuffer
                :message ""                          ; Transient message (unified echo area)
                :message-timeout-id nil              ; Timeout ID for auto-clearing message
                :on-confirm nil                      ; Event vector to dispatch on Enter
                :on-cancel [:minibuffer/deactivate]  ; Event vector to dispatch on Escape/C-g
                :completions []                      ; Full list of possible completions (never filtered)
                :filtered-completions []             ; Filtered completions based on input
                :completion-index 0                  ; Current completion selection index
                :height-lines 1                      ; Dynamic height (1-20+ lines)
                :show-completions? false}            ; Whether to show completion candidates
   :echo-area {:message ""                           ; DEPRECATED - use :minibuffer :message
               :timeout-id nil}                      ; DEPRECATED - use :minibuffer :message-timeout-id
   :help {:awaiting-key? false                      ; Waiting for key press for C-h k
          :callback nil}                            ; Callback event vector for key press
   :packages {}                                     ; Loaded packages (Phase 6)

   ;; Phase 6B Week 2: Child Frames (Popups)
   :child-frames {}                                 ; Map of frame-id to frame data
                                                    ; {:corfu-popup {:id :corfu-popup
                                                    ;                :visible? false
                                                    ;                :x 0 :y 0
                                                    ;                :width 300 :height 200
                                                    ;                :content ["item1" "item2"]
                                                    ;                :face :default}}

   ;; Phase 6B: Face System - Visual styling with CSS variables
   :faces {;; Default faces - every face maps attributes to CSS variable names
           :default {:foreground "--lexicon-fg-default"
                     :background "--lexicon-bg-default"
                     :font-family "--lexicon-font-family-mono"
                     :font-size "--lexicon-font-size"}
           :region {:background "--lexicon-bg-region"
                    :extend true}  ; Extend to window edge
           :highlight {:background "--lexicon-bg-highlight"}
           :mode-line {:foreground "--lexicon-fg-mode-line"
                       :background "--lexicon-bg-mode-line"
                       :box {:line-width 1 :color "--lexicon-border-mode-line"}}
           :mode-line-inactive {:foreground "--lexicon-fg-mode-line-inactive"
                                :background "--lexicon-bg-mode-line-inactive"}
           :minibuffer-prompt {:foreground "--lexicon-fg-prompt"
                               :font-weight "bold"}
           :isearch {:foreground "--lexicon-fg-isearch"
                     :background "--lexicon-bg-isearch"
                     :font-weight "bold"}
           :lazy-highlight {:background "--lexicon-bg-lazy-highlight"}
           :error {:foreground "--lexicon-fg-error"
                   :font-weight "bold"}
           :warning {:foreground "--lexicon-fg-warning"}
           :success {:foreground "--lexicon-fg-success"}
           :shadow {:foreground "--lexicon-fg-shadow"}
           ;; Font-lock faces for syntax highlighting
           :font-lock-comment-face {:foreground "--lexicon-fg-comment"
                                    :font-slant "italic"}
           :font-lock-string-face {:foreground "--lexicon-fg-string"}
           :font-lock-keyword-face {:foreground "--lexicon-fg-keyword"
                                    :font-weight "bold"}
           :font-lock-builtin-face {:foreground "--lexicon-fg-builtin"}
           :font-lock-function-name-face {:foreground "--lexicon-fg-function"}
           :font-lock-variable-name-face {:foreground "--lexicon-fg-variable"}
           :font-lock-type-face {:foreground "--lexicon-fg-type"}
           :font-lock-constant-face {:foreground "--lexicon-fg-constant"}
           ;; Completion faces
           :completions-common-part {:foreground "--lexicon-fg-completion-match"
                                     :font-weight "bold"}
           :completions-first-difference {:foreground "--lexicon-fg-completion-diff"}
           :completions-annotations {:foreground "--lexicon-fg-completion-annotation"}}

   ;; Phase 7.8.1: Theme system - Modus themes support
   :current-theme :modus-vivendi  ; Active theme (:modus-operandi or :modus-vivendi)
   })

(defn create-buffer
  "Create a new buffer data structure"
  [buffer-id name wasm-instance]
  {:id buffer-id
   :wasm-instance wasm-instance
   :file-handle nil
   :name name
   :is-modified? false
   :is-read-only? false                   ; Phase 6B: Read-only buffer flag
   :cursor-position {:line 0 :column 0}   ; Per-buffer cursor position
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
           :line-count 1}
   ;; Phase 6B Week 2: Text Properties & Overlays
   :text-properties {}                    ; Map of position ranges to property maps
   :overlays {}                           ; Map of overlay-id to overlay data
   :next-overlay-id 1})

(defn create-buffer-with-content
  "Create a new buffer with initial content"
  [buffer-id name content]
  (let [wasm-instance (when content
                        ;; Will need to create WasmGapBuffer instance
                        ;; For now, return the structure and set instance later
                        nil)]
    (create-buffer buffer-id name wasm-instance)))

(defn next-buffer-id
  "Generate next available buffer ID"
  [buffers]
  (inc (apply max 0 (keys buffers))))

;; -- Window Tree Functions --

(defn next-window-id
  "Generate next available window ID from window tree"
  [db]
  (:next-window-id db 2))  ; Start from 2 since 1 is initial window

(defn create-leaf-window
  "Create a leaf window node"
  [window-id buffer-id]
  {:type :leaf
   :id window-id
   :buffer-id buffer-id
   :cursor-position {:line 0 :column 0}  ; Each window has its own cursor
   :mark-position nil                     ; Each window has its own mark
   :viewport {:start-line 0 :end-line const/DEFAULT_VIEWPORT_LINES}
   :dimensions {:x 0 :y 0 :width 100 :height 100}})  ; Will be calculated during layout

(defn create-split-window
  "Create a split window node (internal node in window tree)"
  [split-type window-id first-window second-window]
  {:type split-type  ; :hsplit or :vsplit
   :id window-id
   :first first-window
   :second second-window})

(defn find-window-in-tree
  "Find a window by ID in the window tree"
  [tree window-id]
  (when tree
    (cond
      (= (:type tree) :leaf)
      (when (= (:id tree) window-id) tree)

      (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
      (or (find-window-in-tree (:first tree) window-id)
          (find-window-in-tree (:second tree) window-id))

      :else nil)))

(defn get-all-leaf-windows
  "Get all leaf windows from the tree (depth-first)"
  [tree]
  (when tree
    (cond
      (= (:type tree) :leaf)
      [tree]

      (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
      (concat (get-all-leaf-windows (:first tree))
              (get-all-leaf-windows (:second tree)))

      :else [])))

(defn update-window-in-tree
  "Update a window in the tree by ID"
  [tree window-id update-fn]
  (when tree
    (cond
      (= (:type tree) :leaf)
      (if (= (:id tree) window-id)
        (update-fn tree)
        tree)

      (or (= (:type tree) :hsplit) (= (:type tree) :vsplit))
      (assoc tree
             :first (update-window-in-tree (:first tree) window-id update-fn)
             :second (update-window-in-tree (:second tree) window-id update-fn))

      :else tree)))
