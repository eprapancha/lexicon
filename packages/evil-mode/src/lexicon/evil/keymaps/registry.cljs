(ns lexicon.evil.keymaps.registry
  "Centralized keymap registry with context partitioning and precedence handling"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- Keymap Context Specification --

(defn make-context
  "Create a keymap context with state, major mode, and minor modes"
  [& {:keys [state major-mode minor-modes buffer-id]
      :or {state :normal
           major-mode :fundamental-mode
           minor-modes #{}
           buffer-id :global}}]
  {:state state
   :major-mode major-mode
   :minor-modes minor-modes
   :buffer-id buffer-id})

(defn context-matches?
  "Check if a binding context matches the current editor context"
  [binding-context current-context]
  (and 
   ;; State must match exactly
   (= (:state binding-context) (:state current-context))
   
   ;; Major mode must match (or binding is mode-agnostic)
   (or (nil? (:major-mode binding-context))
       (= (:major-mode binding-context) (:major-mode current-context)))
   
   ;; Minor modes: binding minor modes must be subset of current minor modes
   (let [binding-minor (:minor-modes binding-context #{})]
     (or (empty? binding-minor)
         (every? #(contains? (:minor-modes current-context #{}) %) binding-minor)))
   
   ;; Buffer scope: either global or matches specific buffer
   (or (= (:buffer-id binding-context) :global)
       (= (:buffer-id binding-context) (:buffer-id current-context)))))

(defn context-specificity
  "Calculate specificity score for a context (higher = more specific)"
  [context]
  (+ (if (:state context) 10 0)
     (if (:major-mode context) 5 0)
     (* 2 (count (:minor-modes context #{})))
     (if (not= (:buffer-id context) :global) 1 0)))

;; -- Keymap Registry Data Structure --

;; Central registry of all keybindings with context information
(defonce keymap-registry (atom {}))

(defn register-binding!
  "Register a key binding with context information"
  [key-sequence command context]
  (let [context-with-defaults (merge (make-context) context)
        binding-id (str (hash [key-sequence context-with-defaults]))]
    
    (swap! keymap-registry assoc binding-id 
           {:key-sequence key-sequence
            :command command
            :context context-with-defaults
            :specificity (context-specificity context-with-defaults)
            :id binding-id})
    
    (js/console.log "🔑 Registered key binding:" key-sequence "->" command 
                    "in context:" context-with-defaults)
    binding-id))

(defn unregister-binding!
  "Remove a key binding by its ID"
  [binding-id]
  (swap! keymap-registry dissoc binding-id)
  (js/console.log "🗑️ Unregistered key binding:" binding-id))

(defn clear-bindings-for-context!
  "Remove all bindings that match a specific context"
  [context]
  (let [matching-ids (reduce-kv
                      (fn [acc id binding]
                        (if (context-matches? (:context binding) context)
                          (conj acc id)
                          acc))
                      []
                      @keymap-registry)]
    (doseq [id matching-ids]
      (swap! keymap-registry dissoc id))
    (js/console.log "🧹 Cleared" (count matching-ids) "bindings for context:" context)))

;; -- Key Sequence Resolution --

(defn resolve-key-binding
  "Resolve a key sequence to a command in the given context"
  [key-sequence current-context]
  (let [matching-bindings (->> @keymap-registry
                               vals
                               (filter #(= (:key-sequence %) key-sequence))
                               (filter #(context-matches? (:context %) current-context))
                               (sort-by :specificity >))] ; Most specific first
    
    (when-let [best-match (first matching-bindings)]
      (js/console.log "🎯 Resolved key:" key-sequence "->" (:command best-match)
                      "with specificity:" (:specificity best-match))
      (:command best-match))))

(defn list-bindings-for-context
  "List all key bindings available in a given context"
  [context]
  (->> @keymap-registry
       vals
       (filter #(context-matches? (:context %) context))
       (sort-by :specificity >)))

(defn find-bindings-for-command
  "Find all key sequences bound to a specific command"
  [command]
  (->> @keymap-registry
       vals
       (filter #(= (:command %) command))))

;; -- Re-frame Integration --

(rf/reg-event-db
 :keymap/register
 (fn [db [_ key-sequence command context]]
   "Register a key binding via re-frame event"
   (register-binding! key-sequence command context)
   db))

(rf/reg-event-db
 :keymap/unregister
 (fn [db [_ binding-id]]
   "Unregister a key binding via re-frame event"
   (unregister-binding! binding-id)
   db))

(rf/reg-event-db
 :keymap/clear-context
 (fn [db [_ context]]
   "Clear all bindings for a context via re-frame event"
   (clear-bindings-for-context! context)
   db))

(rf/reg-event-fx
 :keymap/resolve-and-dispatch
 (fn [{:keys [db]} [_ key-sequence]]
   "Resolve key sequence and dispatch command"
   (let [current-context (make-context
                          :state (get-in db [:fsm :current-state])
                          :major-mode (get-in db [:buffers (:active-buffer-id db) :major-mode])
                          :minor-modes (get-in db [:buffers (:active-buffer-id db) :minor-modes] #{})
                          :buffer-id (:active-buffer-id db))
         command (resolve-key-binding key-sequence current-context)]
     
     (if command
       {:fx [[:dispatch [:command/dispatch command]]]}
       (do
         (js/console.log "🚫 No command bound to key:" key-sequence "in context:" current-context)
         {:db db})))))

;; -- State-scoped Binding Functions (Evil-define-key equivalent) --

(defn evil-define-key
  "Define key bindings for a specific state (equivalent to Emacs evil-define-key)"
  ([state keymap]
   (evil-define-key state nil keymap))
  ([state major-mode keymap]
   (let [context (cond-> {:state state}
                   major-mode (assoc :major-mode major-mode))]
     (doseq [[key command] keymap]
       (register-binding! key command context)))))

(defn evil-define-minor-mode-key
  "Define key bindings for a minor mode"
  [minor-mode keymap & {:keys [state] :or {state :normal}}]
  (let [context {:state state :minor-modes #{minor-mode}}]
    (doseq [[key command] keymap]
      (register-binding! key command context))))

(defn evil-define-buffer-local-key
  "Define key bindings local to a specific buffer"
  [buffer-id state keymap]
  (let [context {:state state :buffer-id buffer-id}]
    (doseq [[key command] keymap]
      (register-binding! key command context))))

;; -- Keymap Introspection --

(rf/reg-sub
 :keymap/bindings-for-context
 (fn [db _]
   "Get all key bindings for the current context"
   (let [current-context (make-context
                          :state (get-in db [:fsm :current-state])
                          :major-mode (get-in db [:buffers (:active-buffer-id db) :major-mode])
                          :minor-modes (get-in db [:buffers (:active-buffer-id db) :minor-modes] #{})
                          :buffer-id (:active-buffer-id db))]
     (list-bindings-for-context current-context))))

(rf/reg-sub
 :keymap/command-bindings
 (fn [db [_ command]]
   "Get all key sequences bound to a specific command"
   (find-bindings-for-command command)))

;; -- Built-in Keymap Definitions --

(defn initialize-default-keymaps!
  "Initialize default keymaps for all states"
  []
  
  ;; Normal mode bindings
  (evil-define-key :normal 
    {"h" :backward-char
     "j" :next-line
     "k" :previous-line
     "l" :forward-char
     "w" :forward-word
     "b" :backward-word
     "e" :forward-word-end
     "0" :beginning-of-line
     "$" :end-of-line
     "gg" :goto-first-line
     "G" :goto-last-line
     "x" :delete-char
     "d" :delete-operator
     "y" :yank-operator
     "c" :change-operator
     "p" :paste-after
     "P" :paste-before
     "u" :undo
     "C-r" :redo
     "i" :enter-insert-mode
     "a" :append-after-cursor
     "A" :append-end-of-line
     "o" :open-line-below
     "O" :open-line-above
     "v" :enter-visual-mode
     "V" :enter-visual-line-mode
     "C-v" :enter-visual-block-mode
     "/" :search-forward
     "?" :search-backward
     "n" :search-next
     "N" :search-previous
     "." :repeat-last-command
     "ESC" :normal-mode})
  
  ;; Insert mode bindings
  (evil-define-key :insert
    {"ESC" :exit-insert-mode
     "C-c" :exit-insert-mode
     "C-[" :exit-insert-mode})
  
  ;; Visual mode bindings
  (evil-define-key :visual
    {"h" :extend-backward-char
     "j" :extend-next-line
     "k" :extend-previous-line
     "l" :extend-forward-char
     "w" :extend-forward-word
     "b" :extend-backward-word
     "0" :extend-beginning-of-line
     "$" :extend-end-of-line
     "d" :delete-region
     "y" :yank-region
     "c" :change-region
     "x" :delete-region
     "ESC" :exit-visual-mode
     "C-c" :exit-visual-mode})
  
  ;; Operator-pending mode bindings
  (evil-define-key :operator-pending
    {"h" :backward-char
     "j" :next-line
     "k" :previous-line
     "l" :forward-char
     "w" :forward-word
     "b" :backward-word
     "e" :forward-word-end
     "0" :beginning-of-line
     "$" :end-of-line
     "gg" :goto-first-line
     "G" :goto-last-line
     "ESC" :cancel-operator
     "C-c" :cancel-operator})
  
  (js/console.log "🗝️ Initialized default keymaps for all states"))

;; -- Keymap Export/Import for Configuration --

(defn export-keymaps
  "Export all keymaps to a serializable format"
  []
  (let [bindings @keymap-registry]
    (js/console.log "📤 Exported" (count bindings) "key bindings")
    bindings))

(defn import-keymaps
  "Import keymaps from a serialized format"
  [keymaps]
  (reset! keymap-registry keymaps)
  (js/console.log "📥 Imported" (count keymaps) "key bindings"))

;; -- Initialize on namespace load --
;; NOTE: Disabled for package system - initialize explicitly via package initialization
;; (defonce ^:private initialized? (atom false))
;;
;; (when (not @initialized?)
;;   (initialize-default-keymaps!)
;;   (reset! initialized? true))