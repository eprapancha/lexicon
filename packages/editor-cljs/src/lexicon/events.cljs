(ns lexicon.events
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.cache :as cache]
            [lexicon.constants :as const]
            [re-frame.std-interceptors :refer [debug]])
  (:require-macros [lexicon.macros :refer [def-evil-motion def-evil-operator]]))

;; -- Initialization Events --

(rf/reg-event-fx
 :initialize-db
 (fn [_ _]
   "Initialize the application database with default state"
   {:db db/default-db
    :fx [[:dispatch [:initialize-commands]]]}))

(rf/reg-event-db
 :wasm-module-loaded
 (fn [db [_ {:keys [instance constructor]}]]
   "Store the loaded WASM module instance and constructor in the app state"
   (-> db
       (assoc :initialized? true)
       (assoc-in [:system :wasm-constructor] constructor)
       (assoc-in [:buffers 1 :wasm-instance] instance))))

;; -- FSM State Management --

;; Registry for state lifecycle hooks
(defonce state-enter-hooks (atom {}))
(defonce state-exit-hooks (atom {}))

(defn register-state-enter-hook!
  "Register a hook function to be called when entering a state"
  [state hook-fn]
  (swap! state-enter-hooks update state (fnil conj []) hook-fn))

(defn register-state-exit-hook!
  "Register a hook function to be called when exiting a state"
  [state hook-fn]
  (swap! state-exit-hooks update state (fnil conj []) hook-fn))

;; Interceptors for FSM lifecycle hooks
(def on-exit-interceptor
  (rf/->interceptor
    :id :on-exit-interceptor
    :before (fn [{:keys [coeffects] :as context}]
              (let [db (:db coeffects)
                    current-state (get-in db [:fsm :current-state])
                    exit-hooks (get @state-exit-hooks current-state [])]
                (doseq [hook exit-hooks]
                  (try
                    (hook current-state)
                    (catch js/Error e
                      (js/console.error "Error in exit hook for state" current-state ":" e))))
                context))))

(def on-enter-interceptor
  (rf/->interceptor
    :id :on-enter-interceptor
    :after (fn [{:keys [effects] :as context}]
             (let [db (:db effects)
                   new-state (get-in db [:fsm :current-state])
                   enter-hooks (get @state-enter-hooks new-state [])]
               (doseq [hook enter-hooks]
                 (try
                   (hook new-state)
                   (catch js/Error e
                     (js/console.error "Error in enter hook for state" new-state ":" e))))
               context))))

;; Core FSM transition event
(rf/reg-event-fx
 :fsm/transition-to
 [on-exit-interceptor on-enter-interceptor]
 (fn [{:keys [db]} [_ new-state]]
   "Transition the FSM to a new state with lifecycle hooks"
   (let [current-state (get-in db [:fsm :current-state])
         keymap-mapping {:normal :normal-keymap
                        :insert :insert-keymap
                        :visual :visual-keymap
                        :operator-pending :normal-keymap}
         new-keymap (get keymap-mapping new-state :normal-keymap)]
     {:db (-> db
              (assoc-in [:fsm :previous-state] current-state)
              (assoc-in [:fsm :current-state] new-state)
              (assoc-in [:fsm :active-keymap] new-keymap))})))

;; FSM utility events
(rf/reg-event-db
 :fsm/set-operator-pending
 (fn [db [_ operator-fn]]
   "Set a pending operator in the FSM state"
   (assoc-in db [:fsm :operator-pending] operator-fn)))

(rf/reg-event-db
 :fsm/clear-operator-pending
 (fn [db [_]]
   "Clear any pending operator from the FSM state"
   (assoc-in db [:fsm :operator-pending] nil)))

;; Register some default hooks for demonstration
(register-state-enter-hook! :insert 
  (fn [state] 
    (js/console.log "Entering INSERT mode")))

(register-state-enter-hook! :normal 
  (fn [state] 
    (js/console.log "Entering NORMAL mode")))

(register-state-exit-hook! :insert 
  (fn [state] 
    (js/console.log "Exiting INSERT mode")))

;; -- Emacs-style Command Registry and Execution --

(rf/reg-event-db
 :register-command
 (fn [db [_ command-name command-definition]]
   "Register a new command in the central command registry"
   (assoc-in db [:commands command-name] command-definition)))

(rf/reg-event-fx
 :execute-command
 (fn [{:keys [db]} [_ command-name & args]]
   "Execute a command by name from the central registry"
   (let [command-def (get-in db [:commands command-name])]
     (if command-def
       (let [handler (:handler command-def)]
         {:fx [[:dispatch (into handler args)]]})
       {:fx [[:dispatch [:show-error (str "Command not found: " command-name)]]]}))))

(rf/reg-event-db
 :show-error
 (fn [db [_ message]]
   "Show an error message to the user"
   (println "Error:" message)
   (assoc-in db [:system :last-error] message)))

(rf/reg-event-db
 :keyboard-quit
 (fn [db [_]]
   "Cancel the current operation (equivalent to C-g in Emacs)"
   (println "Keyboard quit")
   ;; Clear any pending operations, selection, etc.
   (-> db
       (assoc-in [:ui :selection] {:start 0 :end 0})
       (assoc-in [:fsm :operator-pending] nil))))

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
    (cond
      ;; Special handling for certain keys
      (= key "Control") nil  ; Ignore standalone modifier keys
      (= key "Meta") nil
      (= key "Alt") nil
      (= key "Shift") nil
      
      ;; Handle special keys
      (= key "Escape") "ESC"
      (= key "Enter") "RET"
      (= key "Tab") "TAB"
      (= key "Backspace") "DEL"
      (= key "Delete") "DELETE"
      (= key " ") "SPC"
      
      ;; Function keys
      (and (>= (.indexOf key "F") 0) 
           (js/isNaN (js/parseInt (subs key 1))))
      key
      
      ;; Regular keys with modifiers
      :else
      (let [base-key (if (and shift? (= (count key) 1))
                       ;; For shifted letters, use uppercase
                       (.toUpperCase key)
                       key)]
        (str 
         (when ctrl? "C-")
         (when meta? "M-")
         (when (and alt? (not meta?)) "A-")  ; Alt without Meta
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

(defn get-active-minor-modes
  "Get the set of active minor modes for the active buffer"
  [db]
  (let [active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (:minor-modes active-buffer #{})))

(defn get-active-major-mode
  "Get the active major mode for the active buffer"
  [db]
  (let [active-window (get (:windows db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (:major-mode active-buffer :fundamental-mode)))

(defn resolve-keybinding
  "Resolve a key sequence to a command using Emacs precedence order"
  [db key-sequence-str]
  (let [keymaps (:keymaps db)
        minor-modes (get-active-minor-modes db)
        major-mode (get-active-major-mode db)]
    
    ;; Emacs precedence order:
    ;; 1. Active minor mode keymaps (in reverse order of activation)
    ;; 2. Active major mode keymap  
    ;; 3. Global keymap
    
    (or 
     ;; 1. Check minor mode keymaps
     (some (fn [minor-mode]
             (get-in keymaps [:minor minor-mode key-sequence-str]))
           (reverse (seq minor-modes)))
     
     ;; 2. Check major mode keymap
     (get-in keymaps [:major major-mode key-sequence-str])
     
     ;; 3. Check global keymap
     (get-in keymaps [:global key-sequence-str])
     
     ;; Return nil if no binding found
     nil)))

;; State management for prefix keys
(rf/reg-event-db
 :set-prefix-key-state
 (fn [db [_ prefix-key]]
   "Set the current prefix key state for multi-key sequences"
   (assoc-in db [:ui :prefix-key-state] prefix-key)))

(rf/reg-event-db
 :clear-prefix-key-state
 (fn [db [_]]
   "Clear the prefix key state"
   (assoc-in db [:ui :prefix-key-state] nil)))

(rf/reg-event-fx
 :handle-key-sequence
 (fn [{:keys [db]} [_ key-str]]
   "Handle a key sequence and dispatch the appropriate command"
   (let [prefix-state (get-in db [:ui :prefix-key-state])
         full-sequence (if prefix-state
                        (str prefix-state " " key-str)
                        key-str)
         command-name (resolve-keybinding db full-sequence)]
     
     (cond
       ;; Found a complete command binding
       command-name
       {:fx [[:dispatch [:execute-command command-name]]
             [:dispatch [:clear-prefix-key-state]]]}
       
       ;; Check if this might be a prefix for a multi-key sequence
       ;; by seeing if any keybinding starts with this sequence
       (some (fn [keymap-entry]
               (and (string? (first keymap-entry))
                    (.startsWith (first keymap-entry) full-sequence)))
             (concat 
              (mapcat (fn [minor-mode] 
                        (get-in db [:keymaps :minor minor-mode]))
                      (get-active-minor-modes db))
              (get-in db [:keymaps :major (get-active-major-mode db)])
              (get-in db [:keymaps :global])))
       {:fx [[:dispatch [:set-prefix-key-state full-sequence]]]}
       
       ;; No binding found - check if it's a printable character for insertion
       (and (= (count key-str) 1)
            (not prefix-state)
            (not (re-matches #"[CM]-." key-str))  ; Not a modifier combo
            (>= (.charCodeAt key-str 0) 32)      ; Printable ASCII
            (= (get-in db [:fsm :current-state]) :insert))
       {:fx [[:dispatch [:dispatch-transaction {:type :insert 
                                               :pos (get-in db [:ui :cursor-position])
                                               :text key-str}]]
             [:dispatch [:clear-prefix-key-state]]]}
       
       ;; Unknown key sequence - clear state and ignore
       :else
       {:fx [[:dispatch [:clear-prefix-key-state]]]}))))

;; -- Major and Minor Mode Architecture --

(rf/reg-event-db
 :set-major-mode
 (fn [db [_ mode-keyword]]
   "Set the major mode for the active buffer"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)]
     ;; Update the buffer's major mode
     (assoc-in db [:buffers active-buffer-id :major-mode] mode-keyword))))

(rf/reg-event-db
 :toggle-minor-mode
 (fn [db [_ mode-keyword]]
   "Toggle a minor mode for the active buffer"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (if (contains? current-modes mode-keyword)
       ;; Remove the mode
       (assoc-in db [:buffers active-buffer-id :minor-modes] 
                 (disj current-modes mode-keyword))
       ;; Add the mode
       (assoc-in db [:buffers active-buffer-id :minor-modes] 
                 (conj current-modes mode-keyword))))))

(rf/reg-event-db
 :enable-minor-mode
 (fn [db [_ mode-keyword]]
   "Enable a minor mode for the active buffer"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (assoc-in db [:buffers active-buffer-id :minor-modes] 
               (conj current-modes mode-keyword)))))

(rf/reg-event-db
 :disable-minor-mode
 (fn [db [_ mode-keyword]]
   "Disable a minor mode for the active buffer"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (assoc-in db [:buffers active-buffer-id :minor-modes] 
               (disj current-modes mode-keyword)))))

;; Register mode-related commands
(rf/reg-event-fx
 :initialize-mode-commands
 (fn [{:keys [db]} [_]]
   "Initialize mode-related commands"
   {:fx [[:dispatch [:register-command :fundamental-mode 
                    {:docstring "Switch to fundamental mode"
                     :handler [:set-major-mode :fundamental-mode]}]]
         [:dispatch [:register-command :text-mode 
                    {:docstring "Switch to text mode"
                     :handler [:set-major-mode :text-mode]}]]
         [:dispatch [:register-command :clojure-mode 
                    {:docstring "Switch to Clojure mode"
                     :handler [:set-major-mode :clojure-mode]}]]
         [:dispatch [:register-command :line-number-mode 
                    {:docstring "Toggle line number display"
                     :handler [:toggle-minor-mode :line-number-mode]}]]]}))

;; Update the main initialization to include mode commands
(rf/reg-event-fx
 :initialize-commands
 (fn [{:keys [db]} [_]]
   "Initialize built-in commands in the command registry"
   {:fx [[:dispatch [:register-command :find-file 
                    {:docstring "Open a file"
                     :handler [:find-file]}]]
         [:dispatch [:register-command :save-buffer 
                    {:docstring "Save current buffer"
                     :handler [:save-buffer]}]]
         [:dispatch [:register-command :kill-buffer 
                    {:docstring "Close current buffer"
                     :handler [:close-buffer]}]]
         [:dispatch [:register-command :keyboard-quit 
                    {:docstring "Cancel current operation"
                     :handler [:keyboard-quit]}]]
         [:dispatch [:register-command :undo 
                    {:docstring "Undo last change"
                     :handler [:undo]}]]
         [:dispatch [:register-command :kill-region 
                    {:docstring "Kill (cut) the active region"
                     :handler [:kill-region]}]]
         [:dispatch [:register-command :yank 
                    {:docstring "Yank (paste) from kill ring"
                     :handler [:yank]}]]
         ;; Initialize mode commands
         [:dispatch [:initialize-mode-commands]]
         ;; Update save-buffer to use hooks
         [:dispatch [:update-save-buffer-command]]]}))

;; -- Hook System Implementation --

(rf/reg-event-db
 :add-hook
 (fn [db [_ hook-name command-keyword]]
   "Add a command to a hook"
   (update-in db [:hooks hook-name] (fnil conj []) command-keyword)))

(rf/reg-event-db
 :remove-hook
 (fn [db [_ hook-name command-keyword]]
   "Remove a command from a hook"
   (update-in db [:hooks hook-name] 
              (fn [commands]
                (vec (remove #(= % command-keyword) commands))))))

(rf/reg-event-fx
 :run-hook
 (fn [{:keys [db]} [_ hook-name]]
   "Run all commands registered for a hook"
   (let [hook-commands (get-in db [:hooks hook-name] [])]
     {:fx (mapv (fn [command-name]
                  [:dispatch [:execute-command command-name]])
                hook-commands)})))

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
   (let [active-window (get (:windows db) (:active-window-id db))
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

;; -- Legacy Command Registry (for backward compatibility) --

;; Registry for command functions
(defonce command-registry (atom {}))

(defn register-command!
  "Register a command function in the command registry"
  [command-key command-fn]
  (swap! command-registry assoc command-key command-fn))

(defn get-command
  "Get a command function from the registry"
  [command-key]
  (get @command-registry command-key))

;; Example motion commands defined with macros
(def-evil-motion my-forward-word [count]
  "Move forward by count words"
  {:type :exclusive :jump? false}
  (js/console.log "Forward word motion with count:" count)
  ;; TODO: Implement actual word movement logic
  )

(def-evil-motion my-backward-word [count]
  "Move backward by count words"
  {:type :exclusive :jump? false}
  (js/console.log "Backward word motion with count:" count)
  ;; TODO: Implement actual word movement logic
  )

;; Example operator commands defined with macros
(def-evil-operator my-delete-operator [motion-fn]
  "Delete text defined by motion"
  {:repeat? true}
  (js/console.log "Delete operator with motion:" motion-fn)
  ;; TODO: Implement actual delete logic
  )

;; Register the example commands
(register-command! :forward-word my-forward-word)
(register-command! :backward-word my-backward-word)
(register-command! :delete-operator my-delete-operator)

;; Modal command dispatcher - the core command loop
(rf/reg-event-fx
 :modal/dispatch-key
 (fn [{:keys [db]} [_ key-input]]
   "FSM-aware command dispatcher that handles operator-motion composition"
   (let [fsm-state (get-in db [:fsm :current-state])
         active-keymap (get-in db [:fsm :active-keymap])
         keymap (get-in db [:keymaps active-keymap])
         command-key (get keymap key-input)
         command-fn (get-command command-key)
         operator-pending (get-in db [:fsm :operator-pending])
         
         ;; Get metadata from command function if it exists
         command-metadata (when command-fn 
                           (or (meta command-fn) {}))]
     
     (cond
       ;; No command found for this key
       (not command-key)
       (do
         (js/console.log "No command bound to key:" key-input "in keymap:" active-keymap)
         {:db db})
       
       ;; Command found but no function registered
       (not command-fn)
       (do
         (js/console.log "Command" command-key "not implemented yet")
         {:db db})
       
       ;; We're in operator-pending state and received a motion
       (and operator-pending 
            (:motion? command-metadata))
       (let [operator-fn (get-command operator-pending)]
         (js/console.log "Executing operator-motion composition:" operator-pending "+" command-key)
         (when operator-fn
           ;; Execute the operator with the motion
           (operator-fn command-fn))
         {:db (-> db
                  (assoc-in [:fsm :operator-pending] nil)
                  ;; Return to normal state
                  (assoc-in [:fsm :current-state] :normal)
                  (assoc-in [:fsm :active-keymap] :normal-keymap))})
       
       ;; Received an operator command
       (:operator? command-metadata)
       (do
         (js/console.log "Setting operator pending:" command-key)
         {:db (-> db
                  (assoc-in [:fsm :operator-pending] command-key)
                  (assoc-in [:fsm :current-state] :operator-pending)
                  (assoc-in [:fsm :active-keymap] :operator-pending-keymap))})
       
       ;; Simple command execution (motion in normal mode, or any other command)
       :else
       (do
         (js/console.log "Executing command:" command-key)
         ;; For motions, execute with default count of 1
         (if (:motion? command-metadata)
           (command-fn 1)
           (command-fn))
         {:db db})))))

;; Special state transition commands
(rf/reg-event-fx
 :modal/enter-insert-mode
 (fn [{:keys [db]} [_]]
   "Enter insert mode"
   {:fx [[:dispatch [:fsm/transition-to :insert]]]}))

(rf/reg-event-fx
 :modal/exit-insert-mode
 (fn [{:keys [db]} [_]]
   "Exit insert mode and return to normal"
   {:fx [[:dispatch [:fsm/transition-to :normal]]]}))

(rf/reg-event-fx
 :modal/enter-visual-mode
 (fn [{:keys [db]} [_]]
   "Enter visual mode"
   {:fx [[:dispatch [:fsm/transition-to :visual]]]}))

(rf/reg-event-fx
 :modal/exit-visual-mode
 (fn [{:keys [db]} [_]]
   "Exit visual mode and return to normal"
   {:fx [[:dispatch [:fsm/transition-to :normal]]]}))

(rf/reg-event-fx
 :modal/cancel-operator
 (fn [{:keys [db]} [_]]
   "Cancel pending operator and return to normal"
   {:fx [[:dispatch [:fsm/clear-operator-pending]]
         [:dispatch [:fsm/transition-to :normal]]]}))

;; Register state transition commands
(register-command! :enter-insert-mode #(rf/dispatch [:modal/enter-insert-mode]))
(register-command! :exit-insert-mode #(rf/dispatch [:modal/exit-insert-mode]))
(register-command! :enter-visual-mode #(rf/dispatch [:modal/enter-visual-mode]))
(register-command! :exit-visual-mode #(rf/dispatch [:modal/exit-visual-mode]))
(register-command! :cancel-operator #(rf/dispatch [:modal/cancel-operator]))

;; -- Buffer Management Events --

(rf/reg-event-db
 :create-buffer
 (fn [db [_ name wasm-instance]]
   "Create a new buffer with the given name and WASM instance"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         new-buffer (db/create-buffer buffer-id name wasm-instance)]
     (assoc-in db [:buffers buffer-id] new-buffer))))

(rf/reg-event-db
 :switch-buffer
 (fn [db [_ buffer-id]]
   "Switch to the specified buffer by updating the active window"
   (if (get-in db [:buffers buffer-id])
     (assoc-in db [:windows (:active-window-id db) :buffer-id] buffer-id)
     db))) ; Ignore if buffer doesn't exist

;; -- Input Handling Events --

(rf/reg-event-fx
 :handle-text-input
 (fn [{:keys [db]} [_ {:keys [input-type data dom-cursor-pos]}]]
   "Handle text input events with proper cursor position from app state"
   (let [current-cursor (get-in db [:ui :cursor-position] 0)
         insert-pos current-cursor  ; Use app state cursor position
         
         
         transaction (case input-type
                       "insertText"
                       (when data
                         {:type :insert :pos insert-pos :text data})
                       
                       "insertCompositionText"
                       (when data
                         ;; Handle IME composition
                         (rf/dispatch [:ime-composition-update data])
                         nil)
                       
                       "deleteContentBackward"
                       (when (> insert-pos 0)
                         {:type :delete :pos (dec insert-pos) :length 1})
                       
                       "deleteContentForward"
                       {:type :delete :pos insert-pos :length 1}
                       
                       "insertFromPaste"
                       (when data
                         {:type :insert :pos insert-pos :text data})
                       
                       ;; Log unhandled input types
                       (do (println "Unhandled input type:" input-type) nil))]
     
     (if transaction
       {:fx [[:dispatch [:dispatch-transaction transaction]]]}
       {:db db}))))

;; -- Transaction Events --

(rf/reg-event-fx
 :dispatch-transaction
 (fn [{:keys [db]} [_ transaction]]
   "Apply a transaction to the WASM kernel - side effect only"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     
     (if (and wasm-instance active-buffer-id)
       (let [transaction-id (inc (get-in db [:system :last-transaction-id]))
             {:keys [type pos text length]} transaction
             
             ;; Convert to WASM transaction format using constants
             wasm-transaction (case type
                               :insert {:type const/TRANSACTION_INSERT :position pos :text text}
                               :delete {:type const/TRANSACTION_DELETE :position pos :length length}
                               :replace {:type const/TRANSACTION_REPLACE :position pos :length length :text text}
                               {:type const/TRANSACTION_INSERT :position 0 :text ""}) ; fallback
             
             ;; Convert to JSON string for WASM
             transaction-json (js/JSON.stringify (clj->js wasm-transaction))]
         
         ;; Apply transaction and handle result synchronously
         (try
           (let [patch-json (.applyTransaction ^js wasm-instance transaction-json)]
             (println "🔧 Transaction applied. Patch JSON:" patch-json)
             ;; Transaction successful - dispatch result event
             (rf/dispatch [:apply-transaction-result 
                          {:patch-json patch-json
                           :transaction transaction
                           :transaction-id transaction-id
                           :buffer-id active-buffer-id}]))
           (catch js/Error error
             ;; Transaction failed - dispatch error event
             (println "❌ Transaction failed:" error)
             (rf/dispatch [:transaction-failed {:error (str error)}])))
         
         ;; Return no immediate db changes - handlers will update state
         {:db db})
       
       ;; WASM not ready or no active buffer
       {:db db}))))

;; New event handlers for async transaction results

(rf/reg-event-db
 :apply-transaction-result
 (fn [db [_ {:keys [patch-json transaction transaction-id buffer-id]}]]
   "Apply the result of a successful transaction - pure state update"
   (println "📥 Applying transaction result. Patch JSON:" patch-json)
   (try
     (let [patch (js->clj (js/JSON.parse patch-json) :keywordize-keys true)
           {:keys [type pos text length]} transaction
           new-cursor (:cursor-position patch)
           new-length (:length patch)
           
           ;; Invalidate affected cache ranges
           cache-start (case type
                         :insert pos
                         :delete pos
                         :replace pos
                         0)
           cache-end (case type
                       :insert (+ pos (count text))
                       :delete (+ pos length)
                       :replace (+ pos (count text))
                       cache-start)
           
           updated-cache (cache/invalidate-cache-range 
                         (get-in db [:ui :text-cache])
                         cache-start cache-end)
           
           ;; Set cursor position - if not provided by WASM, calculate it
           final-cursor (or new-cursor 
                           (case type
                             :insert (+ pos (count text))
                             :delete pos
                             :replace (+ pos (count text))
                             pos))]
       
       (println "✅ Final cursor position:" final-cursor "Patch:" patch)
       (-> db
           (assoc-in [:system :last-transaction-id] transaction-id)
           (assoc-in [:system :last-patch] patch)
           (assoc-in [:ui :cursor-position] final-cursor)
           (assoc-in [:ui :view-needs-update?] true)
           (assoc-in [:ui :text-cache] updated-cache)
           (assoc-in [:buffers buffer-id :is-modified?] true)))
     (catch js/Error error
       (println "❌ Error processing transaction result:" error)
       db))))

(rf/reg-event-db
 :transaction-failed
 (fn [db [_ {:keys [error]}]]
   "Handle transaction failure"
   (println "Transaction failed:" error)
   (assoc-in db [:system :last-error] error)))

(rf/reg-event-fx
 :compound-transaction
 (fn [{:keys [db]} [_ operations]]
   "Apply multiple operations as a compound transaction"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (if wasm-instance
       (let [compound-transaction {:type const/TRANSACTION_COMPOUND :operations operations}
             transaction-json (js/JSON.stringify (clj->js compound-transaction))
             error-code (.applyTransaction ^js wasm-instance transaction-json)]
         
         (if (= error-code 0)
           ;; Success - update state similar to single transaction
           (rf/dispatch [:transaction-completed])
           ;; Error - handle appropriately
           (rf/dispatch [:transaction-failed (.getLastErrorMessage ^js wasm-instance)])))
       
       {:db db}))))

;; -- UI State Events --

(rf/reg-event-db
 :set-cursor-position
 (fn [db [_ position]]
   "Update cursor position"
   (assoc-in db [:ui :cursor-position] position)))

(rf/reg-event-db
 :set-selection
 (fn [db [_ start end]]
   "Update selection range"
   (assoc-in db [:ui :selection] {:start start :end end})))

(rf/reg-event-db
 :ime-composition-start
 (fn [db [_]]
   "Start IME composition"
   (assoc-in db [:ui :ime-composing?] true)))

(rf/reg-event-db
 :ime-composition-update
 (fn [db [_ text]]
   "Update IME composition text"
   (assoc-in db [:ui :ime-composition-text] text)))

(rf/reg-event-db
 :ime-composition-end
 (fn [db [_ final-text]]
   "End IME composition and commit text"
   (-> db
       (assoc-in [:ui :ime-composing?] false)
       (assoc-in [:ui :ime-composition-text] ""))))

(rf/reg-event-fx
 :ime-commit-composition
 (fn [coeffects [_ text position]]
   "Commit IME composition as a transaction"
   (rf/dispatch [:dispatch-transaction {:type :insert
                                        :pos position
                                        :text text}])
   coeffects))

;; -- View Reconciliation Events --

(rf/reg-event-db
 :view-updated
 (fn [db [_]]
   "Mark that view has been updated"
   (assoc-in db [:ui :view-needs-update?] false)))

(rf/reg-event-fx
 :reconcile-dom-state
 (fn [{:keys [db]} [_ dom-content expected-content]]
   "Reconcile DOM state with WASM state using diff algorithm"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     (when wasm-instance
       ;; TODO: Implement text diffing algorithm
       ;; For now, replace entire content
       (.deleteText ^js wasm-instance 0 (.getLength ^js wasm-instance))
       (.insertText ^js wasm-instance 0 dom-content))
     
     {:db (assoc-in db [:ui :view-needs-update?] true)})))

;; -- System Events --

(rf/reg-event-db
 :set-mutation-observer
 (fn [db [_ observer]]
   "Store MutationObserver instance"
   (assoc-in db [:system :mutation-observer] observer)))

(rf/reg-event-db
 :set-reconciliation-active
 (fn [db [_ active?]]
   "Set reconciliation active flag"
   (assoc-in db [:system :reconciliation-active?] active?)))

;; -- File System Access Events --

(rf/reg-event-fx
 :save-buffer
 (fn [{:keys [db]} [_]]
   "Save the active buffer to disk"
   (let [active-window (get (:windows db) (:active-window-id db))
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

(rf/reg-fx
 :save-file-picker
 (fn [{:keys [content buffer-id]}]
   (-> (js/window.showSaveFilePicker)
       (.then (fn [file-handle]
                (-> (.createWritable file-handle)
                    (.then (fn [writable]
                             (-> (.write writable content)
                                 (.then (fn []
                                          (.close writable)
                                          (rf/dispatch [:buffer-saved 
                                                       {:buffer-id buffer-id
                                                        :file-handle file-handle}])))
                                 (.catch (fn [error]
                                           (println "Write failed:" error))))))
                    (.catch (fn [error]
                              (println "Failed to create writable stream:" error))))))
       (.catch (fn [error]
                 (println "Save cancelled or failed:" error))))))

(rf/reg-fx
 :save-to-file-handle
 (fn [{:keys [file-handle content buffer-id]}]
   (-> (.createWritable file-handle)
       (.then (fn [writable]
                (-> (.write writable content)
                    (.then (fn []
                             (.close writable)
                             (rf/dispatch [:buffer-saved 
                                          {:buffer-id buffer-id
                                           :file-handle file-handle}])))
                    (.catch (fn [error]
                              (println "Write failed:" error))))))
       (.catch (fn [error]
                 (println "Failed to create writable stream:" error))))))

(rf/reg-event-db
 :buffer-saved
 (fn [db [_ {:keys [buffer-id file-handle]}]]
   "Mark buffer as saved and update file handle"
   (let [file-name (.-name file-handle)]
     (-> db
         (assoc-in [:buffers buffer-id :is-modified?] false)
         (assoc-in [:buffers buffer-id :file-handle] file-handle)
         (assoc-in [:buffers buffer-id :name] file-name)))))

(rf/reg-event-fx
 :find-file
 (fn [{:keys [db]} [_]]
   "Open a file from disk"
   {:fx [[:open-file-picker]]}))

(rf/reg-fx
 :open-file-picker
 (fn [_]
   "Handle file picker interaction and dispatch appropriate events"
   (-> (js/window.showOpenFilePicker)
       (.then (fn [file-handles]
                (let [file-handle (first file-handles)]
                  (-> (.getFile file-handle)
                      (.then (fn [file]
                               (-> (.text file)
                                   (.then (fn [content]
                                            (rf/dispatch [:file-read-success 
                                                         {:file-handle file-handle
                                                          :content content
                                                          :name (.-name file)}])))
                                   (.catch (fn [error]
                                             (rf/dispatch [:file-read-failure 
                                                          {:error error
                                                           :message "Failed to read file content"}]))))))
                      (.catch (fn [error]
                                (rf/dispatch [:file-read-failure 
                                             {:error error
                                              :message "Failed to access file"}])))))))
       (.catch (fn [error]
                 ;; Don't dispatch error for user cancellation
                 (when (not= (.-name error) "AbortError")
                   (rf/dispatch [:file-read-failure 
                                {:error error
                                 :message "File picker failed"}])))))))

(rf/reg-event-fx
 :file-read-success
 (fn [{:keys [db]} [_ {:keys [file-handle content name]}]]
   "Handle successful file read - create new buffer and switch to it"
   (let [buffer-id (db/next-buffer-id (:buffers db))
         WasmEditorCore (get-in db [:system :wasm-constructor])
         wasm-instance (WasmEditorCore.)]
     ;; Initialize WASM instance with file content
     (.init wasm-instance content)
     
     ;; Create new buffer and update app state
     (let [new-buffer {:id buffer-id
                       :wasm-instance wasm-instance
                       :file-handle file-handle
                       :name name
                       :is-modified? false
                       :mark-position nil}]
       {:db (-> db
                (assoc-in [:buffers buffer-id] new-buffer)
                (assoc-in [:windows (:active-window-id db) :buffer-id] buffer-id))}))))

(rf/reg-event-db
 :file-read-failure
 (fn [db [_ {:keys [error message]}]]
   "Handle file read failure"
   (println "File read failed:" message error)
   ;; Could add user notification here in the future
   db))

;; -- Buffer Lifecycle Events --

(rf/reg-event-fx
 :close-buffer
 (fn [{:keys [db]} [_ buffer-id]]
   "Close a buffer and free its WASM memory, with defensive logic"
   (let [buffers (:buffers db)
         buffer-to-close (get buffers buffer-id)
         active-window-id (:active-window-id db)
         active-window (get (:windows db) active-window-id)
         currently-active-buffer-id (:buffer-id active-window)]
     
     (if buffer-to-close
       (let [wasm-instance (:wasm-instance buffer-to-close)
             remaining-buffers (dissoc buffers buffer-id)
             remaining-buffer-ids (keys remaining-buffers)]
         
         ;; Free the WASM instance memory
         (when wasm-instance
           (.free ^js wasm-instance))
         
         (if (empty? remaining-buffer-ids)
           ;; No buffers left - create a new default *scratch* buffer
           (let [new-buffer-id (inc (apply max 0 (keys buffers)))
                 WasmEditorCore (get-in db [:system :wasm-constructor])
                 new-wasm-instance (WasmEditorCore.)]
             (.init new-wasm-instance "")
             {:db (-> db
                      (assoc :buffers {new-buffer-id {:id new-buffer-id
                                                      :wasm-instance new-wasm-instance
                                                      :file-handle nil
                                                      :name "*scratch*"
                                                      :is-modified? false
                                                      :mark-position nil}})
                      (assoc-in [:windows active-window-id :buffer-id] new-buffer-id))})
           
           ;; Other buffers exist - switch to another buffer if necessary
           (let [new-active-buffer-id (if (= buffer-id currently-active-buffer-id)
                                        ;; Need to switch to a different buffer
                                        (first remaining-buffer-ids)
                                        ;; Keep current active buffer
                                        currently-active-buffer-id)]
             {:db (-> db
                      (assoc :buffers remaining-buffers)
                      (assoc-in [:windows active-window-id :buffer-id] new-active-buffer-id))})))
       
       ;; Buffer not found - no action needed
       {:db db}))))

;; -- WASM Loading Error Events --

(rf/reg-event-db
 :wasm-load-failed
 (fn [db [_ error]]
   "Handle WASM loading failure"
   (println "WASM load failed, showing error to user:" error)
   (assoc-in db [:system :wasm-error] (str error))))

(rf/reg-event-fx
 :reconcile-dom-if-needed
 (fn [{:keys [db]} [_ dom-content]]
   "Check if DOM content differs from WASM content and reconcile if needed"
   (let [reconciliation-active? (get-in db [:system :reconciliation-active?])
         active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)]
     ;; Only process if reconciliation is not currently active
     (when (and (not reconciliation-active?) wasm-instance)
       (let [expected-content (.getText ^js wasm-instance)]
         (when (not= dom-content expected-content)
           (println "🔄 Reconciling DOM state")
           {:fx [[:dispatch [:reconcile-dom-state dom-content expected-content]]]})))
     {:db db})))

;; -- Viewport Events for Virtualized Rendering --

(rf/reg-event-db
 :update-viewport
 (fn [db [_ start-line end-line]]
   "Update the viewport for the active window"
   (let [active-window-id (:active-window-id db)]
     (-> db
         (assoc-in [:windows active-window-id :viewport :start-line] start-line)
         (assoc-in [:windows active-window-id :viewport :end-line] end-line)))))

;; -- Region Selection and Kill Ring Events --

(rf/reg-event-db
 :set-mark
 (fn [db [_]]
   "Set the mark at the current cursor position"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         cursor-pos (get-in db [:ui :cursor-position])]
     (assoc-in db [:buffers active-buffer-id :mark-position] cursor-pos))))

(rf/reg-event-fx
 :kill-region
 (fn [{:keys [db]} [_]]
   "Kill (cut) the region between point and mark"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         mark-position (:mark-position active-buffer)
         cursor-pos (get-in db [:ui :cursor-position])]
     
     (if (and wasm-instance mark-position)
       (let [start (min cursor-pos mark-position)
             end (max cursor-pos mark-position)
             length (- end start)]
         (if (> length 0)
           (let [killed-text (.getTextInRange ^js wasm-instance start end)
                 kill-ring (:kill-ring db)
                 updated-kill-ring (take const/KILL_RING_MAX_SIZE (cons killed-text kill-ring))]
             {:db (-> db
                      (assoc :kill-ring updated-kill-ring)
                      (assoc-in [:buffers active-buffer-id :mark-position] nil))
              :fx [[:dispatch [:dispatch-transaction 
                              {:type :delete
                               :pos start
                               :length length}]]]})
           {:db db}))
       (do
         (println "Mark not set")
         {:db db})))))

(rf/reg-event-fx
 :yank
 (fn [{:keys [db]} [_]]
   "Yank (paste) the most recent kill"
   (let [active-window (get (:windows db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         active-buffer (get (:buffers db) active-buffer-id)
         wasm-instance (:wasm-instance active-buffer)
         kill-ring (:kill-ring db)
         cursor-pos (get-in db [:ui :cursor-position])]
     
     (if (and wasm-instance (seq kill-ring))
       (let [text-to-yank (first kill-ring)]
         {:fx [[:dispatch [:dispatch-transaction 
                          {:type :insert
                           :pos cursor-pos
                           :text text-to-yank}]]]})
       {:db db}))))
