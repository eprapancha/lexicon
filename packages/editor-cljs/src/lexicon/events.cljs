(ns lexicon.events
  (:require [re-frame.core :as rf]
            [re-frame.db]
            [lexicon.db :as db]
            [lexicon.cache :as cache]
            [lexicon.constants :as const]
            [lexicon.packages :as packages]
            [lexicon.completion.styles :as completion-styles]
            [lexicon.completion.metadata :as completion-metadata]
            [re-frame.std-interceptors :refer [debug]]
            ;; Event modules (auto-register handlers)
            [lexicon.events.keymap]
            [lexicon.events.mode]
            [lexicon.events.wasm]
            [lexicon.events.buffer :as buffer-events]
            [lexicon.events.edit]
            [lexicon.events.ui]
            [lexicon.events.command]
            [lexicon.events.isearch]
            [lexicon.events.message]  ; Issue #47: Message system
            [lexicon.events.window]   ; Window management (splitting, deletion)
            [lexicon.events.dired]))  ; Directory editor (Issue #93)

;; -- Helper Functions --

;; Helper functions moved to lexicon.events.buffer:
;; - line-col-to-linear-pos
;; - detect-language-from-filename

;; Helper functions moved to lexicon.events.edit:
;; - word-char?
;; - find-forward-word-boundary
;; - find-backward-word-boundary
;; - linear-pos-to-line-col

;; -- Initialization Events --

(rf/reg-event-fx
 :initialize-db
 (fn [_ _]
   "Initialize the application database with default state"
   {:db db/default-db
    :fx [[:dispatch [:initialize-commands]]]}))

;; WASM and parser event handlers moved to lexicon.events.wasm
;; - :wasm-module-loaded
;; - :wasm-load-failed

;; =============================================================================
;; REMOVED: FSM State Management (Evil-mode)
;; =============================================================================
;; All FSM (Finite State Machine) code for modal editing has been moved to:
;; packages/evil-mode/src/lexicon/evil/fsm/
;;
;; This includes:
;; - State lifecycle hooks (enter/exit)
;; - Interceptors for state transitions
;; - FSM transition events (:fsm/transition-to, etc.)
;; - Operator-pending state management
;;
;; Evil-mode will be re-implemented as an external package in Phase 6.
;; Core Emacs does not have modal editing - it uses standard Emacs keybindings.
;; =============================================================================

;; Command system event handlers moved to lexicon.events.command
;; - :register-command
;; - :execute-command
;; - :execute-extended-command
;; - :execute-command-by-name
;; - :initialize-commands
;; - :universal-argument
;; - :clear-prefix-argument
;; - :initialize-mode-commands
;; - command-registry (atom)
;; - register-command! (function)
;; - get-command (function)
;;
;; Help system event handlers moved to lexicon.events.command
;; - :describe-bindings
;; - :describe-key
;; - :describe-key/show
;; - :describe-function
;; - :describe-function/show
;; - :apropos-command
;; - :apropos-command/show
;; - :help-for-help

(rf/reg-event-db
 :show-error
 (fn [db [_ message]]
   "Show an error message to the user"
   (println "Error:" message)
   (assoc-in db [:system :last-error] message)))

(rf/reg-event-fx
 :keyboard-quit
 (fn [{:keys [db]} [_]]
   "Cancel the current operation (equivalent to C-g in Emacs)"
   (let [minibuffer-active? (get-in db [:minibuffer :active?])
         on-cancel (get-in db [:minibuffer :on-cancel] [:minibuffer/deactivate])]
     (if minibuffer-active?
       ;; If minibuffer is active, cancel it and dispatch on-cancel
       {:db (-> db
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch on-cancel]
             [:dispatch [:echo/message "Quit"]]]}
       ;; Otherwise just clear pending operations and show quit message
       {:db (-> db
                (assoc-in [:ui :selection] {:start 0 :end 0})
                (assoc-in [:fsm :operator-pending] nil))
        :fx [[:dispatch [:echo/message "Quit"]]]}))))

;; -- Key Sequence Parsing and Processing --

(defn key-event-to-string
  "Convert a KeyboardEvent to Emacs-style key string notation"
  [event]
  (let [ctrl? (.-ctrlKey event)
        meta? (.-metaKey event)
        alt? (.-altKey event)
        shift? (.-shiftKey event)
        key (.-key event)
        code (.-code event)]
    ;; DEBUG: Log key event details to *Messages* buffer
    (rf/dispatch [:message/add (str "🔍 KEY EVENT: key=" key " code=" code " ctrl=" ctrl? " meta=" meta? " alt=" alt? " shift=" shift?)])
    (cond
      ;; Special handling for certain keys
      (= key "Control") nil  ; Ignore standalone modifier keys
      (= key "Meta") nil
      (= key "Alt") nil
      (= key "Shift") nil

      ;; Handle special keys (without modifiers)
      (and (= key "Escape") (not ctrl?) (not meta?) (not alt?)) "ESC"
      (and (= key "Enter") (not ctrl?) (not meta?) (not alt?)) "RET"
      (and (= key "Tab") (not ctrl?) (not meta?) (not alt?)) "TAB"
      (and (= key "Backspace") (not ctrl?) (not meta?) (not alt?)) "DEL"
      (and (= key "Delete") (not ctrl?) (not meta?) (not alt?)) "DELETE"
      (and (= key " ") (not ctrl?) (not meta?) (not alt?)) "SPC"

      ;; Function keys
      (and (>= (.indexOf key "F") 0)
           (js/isNaN (js/parseInt (subs key 1))))
      key

      ;; Regular keys with modifiers (including special keys with modifiers)
      :else
      (let [;; Check if this is a special key that needs explicit S- prefix
            ;; (e.g., Insert, not shifted punctuation like < or >)
            needs-s-prefix? (and shift?
                                 (or (= key "Insert")
                                     (= key "PageUp")
                                     (= key "PageDown")))
            base-key (cond
                       (= key " ") "SPC"
                       (= key "Enter") "RET"
                       (= key "Tab") "TAB"
                       (= key "Backspace") "DEL"
                       (= key "Delete") "DELETE"
                       (= key "Escape") "ESC"
                       (= key "Insert") "Insert"
                       (= key "PageUp") "PageUp"
                       (= key "PageDown") "PageDown"
                       (and shift? (= (count key) 1) (re-matches #"[a-z]" key)) (.toUpperCase key)
                       :else key)]
        (str
         (when ctrl? "C-")
         (when (or meta? alt?) "M-")  ; Map both Meta and Alt keys to M- (Emacs convention)
         (when needs-s-prefix? "S-")
         base-key)))))

(defn parse-key-sequence
  "Parse a key sequence string like 'C-x C-f' into a vector of individual keys"
  [key-sequence-str]
  (when key-sequence-str
    (clojure.string/split (clojure.string/trim key-sequence-str) #"\s+")))

(defn normalize-key-sequence
  "Normalize a key sequence vector to handle common variations"
  [key-sequence]
  (mapv (fn [key]
          (case key
            "C-m" "RET"     ; Ctrl+M is Enter
            "C-i" "TAB"     ; Ctrl+I is Tab
            "C-[" "ESC"     ; Ctrl+[ is Escape
            key))
        key-sequence))

;; Keymap event handlers moved to lexicon.events.keymap
;; - :set-prefix-key-state
;; - :clear-prefix-key-state
;; - :handle-key-sequence

;; Mode event handlers moved to lexicon.events.mode
;; - :set-major-mode
;; - :toggle-minor-mode
;; - :enable-minor-mode
;; - :disable-minor-mode
;; - :line-number-mode
;; - :column-number-mode

;; -- Package Management Commands (Phase 6) --

(rf/reg-event-fx
 :load-package
 (fn [{:keys [db]} [_ package-name]]
   "Load a package by name"
   (packages/load-package! package-name)
   {:db db}))

(rf/reg-event-fx
 :unload-package
 (fn [{:keys [db]} [_ package-name]]
   "Unload a package by name"
   (packages/unload-package! package-name)
   {:db db}))

;; Text editing event handlers moved to lexicon.events.edit
;; - :delete-backward-char
;; - :delete-forward-char

;; Cursor movement event handlers moved to lexicon.events.edit
;; - :forward-char
;; - :backward-char
;; - :next-line
;; - :previous-line
;; - :click-to-position
;; - :beginning-of-line
;; - :end-of-line
;; - :beginning-of-buffer
;; - :end-of-buffer
;; - :forward-word
;; - :backward-word

;; Editing event handlers moved to lexicon.events.edit
;; - :kill-line
;; - :open-line

;; Mark and region event handlers moved to lexicon.events.edit
;; - :set-mark-command
;; - :set-mark
;; - :copy-region-as-kill
;; - :kill-region

;; Prefix argument event handlers moved to lexicon.events.command
;; - :universal-argument
;; - :clear-prefix-argument

;; Cursor management event handlers moved to lexicon.events.edit
;; - :initialize-buffer-cursor
;; - :update-cursor-position
;; - :set-cursor-position


;; -- Hook System Implementation --

;; Hook event handlers moved to lexicon.events.mode
;; - :add-hook
;; - :remove-hook
;; - :run-hook

;; Example hook usage - update save-buffer to use before-save-hook
(rf/reg-event-fx
 :save-buffer-with-hooks
 (fn [{:keys [db]} [_]]
   "Save buffer with before-save and after-save hooks"
   {:fx [[:dispatch [:run-hook :before-save-hook]]
         [:dispatch [:save-buffer-internal]]
         [:dispatch [:run-hook :after-save-hook]]]}))

;; Create internal save-buffer event (renamed from original)
(rf/reg-event-fx
 :save-buffer-internal
 (fn [{:keys [db]} [_]]
   "Internal save buffer implementation"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         file-handle (:file-handle active-buffer)]
     
     (if wasm-instance
       (let [content (.getText ^js wasm-instance)]
         (if file-handle
           ;; Save to existing file
           {:fx [[:save-to-file-handle {:file-handle file-handle
                                        :content content
                                        :buffer-id active-buffer-id}]]}
           ;; Prompt for save location
           {:fx [[:save-file-picker {:content content
                                     :buffer-id active-buffer-id}]]}))
       {:db db}))))

;; Update the original save-buffer command registration to use hooks
(rf/reg-event-fx
 :update-save-buffer-command
 (fn [{:keys [db]} [_]]
   "Update save-buffer command to use hook system"
   {:fx [[:dispatch [:register-command :save-buffer 
                    {:docstring "Save current buffer with hooks"
                     :handler [:save-buffer-with-hooks]}]]]}))

;; REMOVED: Evil-mode example commands
;; These have been moved to packages/evil-mode/
;; Evil-mode will be implemented as an external package in Phase 6

;; =============================================================================
;; REMOVED: Modal Command Dispatcher (Evil-mode)
;; =============================================================================
;; The modal/FSM-aware command dispatcher has been moved to:
;; packages/evil-mode/src/lexicon/evil/command/dispatcher.cljs
;;
;; This included:
;; - :modal/dispatch-key event (FSM-aware key dispatcher)
;; - Operator-motion composition logic
;; - State transition commands (:modal/enter-insert-mode, etc.)
;; - Mode-specific command registration
;;
;; Core Emacs uses a simple keymap lookup → command execution model.
;; No FSM, no modal states, no operator-pending logic.
;; =============================================================================

;; Buffer event handlers moved to lexicon.events.buffer
;; - :create-buffer
;; - :switch-buffer
;; - :switch-to-buffer
;; - :switch-to-buffer-by-name
;; - :kill-buffer
;; - :kill-buffer-by-name
;; - :list-buffers
;; - :buffer-menu/select-buffer
;; - :save-buffer
;; - :write-file
;; - :buffer-saved
;; - :find-file
;; - :file-read-success
;; - :file-read-failure
;; - :close-buffer
;; - :save-file-picker (effect)
;; - :save-to-file-handle (effect)
;; - :open-file-picker (effect)

;; Window management event handlers moved to lexicon.events.ui
;; - :split-window-below
;; - :split-window-right
;; - :other-window
;; - :set-active-window
;; - :delete-window
;; - :delete-other-windows

;; Transaction queue handlers moved to lexicon.events.wasm
;; - :editor/set-flight-status
;; - :editor/queue-transaction
;; - :handle-text-input
;; - :editor/process-queue (effect)
;; - :editor/transaction-success
;; - :editor/transaction-failure
;; - :dispatch-transaction
;; - :apply-transaction-result
;; - :transaction-failed
;; - :compound-transaction


;; UI state event handlers moved to lexicon.events.ui
;; - :set-selection
;; - :ime-composition-start
;; - :ime-composition-update
;; - :ime-composition-end
;; - :ime-commit-composition

;; View reconciliation event handlers moved to lexicon.events.ui
;; - :view-updated
;; - :reconcile-dom-state

;; System event handlers moved to lexicon.events.ui
;; - :set-mutation-observer
;; - :set-reconciliation-active

;; Minibuffer event handlers moved to lexicon.events.ui
;; - :minibuffer/activate
;; - :minibuffer/deactivate
;; - :minibuffer/set-input
;; - :minibuffer/complete
;; - :minibuffer/confirm

;; Echo area event handlers moved to lexicon.events.ui
;; - :echo/message
;; - :echo/clear

;; -- Parser Worker Events --


;; Parser event handlers moved to lexicon.events.wasm
;; - :parser/start-worker (effect)
;; - :parser/worker-created
;; - :parser/worker-ready
;; - :parser/worker-error
;; - :parser/ast-updated
;; - :parser/parse-error
;; - :parser/request-parse
;; - :parser/request-incremental-parse

;; WebSocket bridge event handlers moved to lexicon.events.ui
;; - :ws/connecting
;; - :ws/opened
;; - :ws/closed
;; - :ws/error
;; - :ws/connect
;; - :ws/message-received

;; WebSocket effects moved to lexicon.events.ui
;; - :ws/connect (effect)
;; - :ws/send (effect)

;; Focus effect moved to lexicon.events.ui
;; - :focus-editor (effect)

;; Kill/yank event handlers moved to lexicon.events.edit
;; - :yank
;; - :yank-pop

;; Undo event handlers moved to lexicon.events.edit
;; - :undo
;; - :undo-complete
