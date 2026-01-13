(ns lexicon.events.keymap
  "Event handlers for key sequence handling and keymap resolution.

  Handles:
  - Multi-key sequence processing
  - Keymap lookup with Emacs precedence order
  - Prefix key state management
  - Special key insertion (SPC, RET, TAB)"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; -- Helper Functions --

(defn get-active-minor-modes
  "Get the set of active minor modes for the active buffer"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (:minor-modes active-buffer #{})))

(defn get-active-major-mode
  "Get the active major mode for the active buffer"
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        active-buffer-id (:buffer-id active-window)
        active-buffer (get (:buffers db) active-buffer-id)]
    (:major-mode active-buffer :fundamental-mode)))

(defn lookup-key-in-keymap
  "Look up a key in a single keymap, walking parent chain if needed.
   Returns command or nil if not found."
  [keymaps keymap-path key-sequence-str]
  (when keymap-path
    (let [keymap-data (get-in keymaps keymap-path)]
      (or
       ;; Check direct binding in this keymap
       (get-in keymap-data [:bindings key-sequence-str])
       ;; If not found, check parent keymap
       (when-let [parent-path (:parent keymap-data)]
         (lookup-key-in-keymap keymaps parent-path key-sequence-str))))))

(defn collect-all-bindings-from-keymap
  "Collect all key bindings from a keymap and its parent chain.
   Returns a flat sequence of [key-str command] pairs."
  [keymaps keymap-path]
  (when keymap-path
    (let [keymap-data (get-in keymaps keymap-path)
          bindings (:bindings keymap-data)
          parent-bindings (when-let [parent (:parent keymap-data)]
                           (collect-all-bindings-from-keymap keymaps parent))]
      (concat (seq bindings) parent-bindings))))

(defn resolve-keybinding
  "Resolve a key sequence to a command using Emacs precedence order.
   Walks keymap parent chains automatically."
  [db key-sequence-str]
  (let [keymaps (:keymaps db)
        transient-keymap (:transient-keymap db)
        minor-modes (get-active-minor-modes db)
        major-mode (get-active-major-mode db)]

    ;; Emacs precedence order (Phase 6.5):
    ;; 0. Transient keymap (for C-u accumulation)
    ;; 1. Active minor mode keymaps (in reverse order of activation)
    ;; 2. Active major mode keymap
    ;; 3. Global keymap
    ;; Each keymap can have a parent chain

    (or
     ;; 0. Check transient keymap (Phase 6.5 Week 1-2)
     (when transient-keymap
       (lookup-key-in-keymap keymaps [:transient transient-keymap] key-sequence-str))

     ;; 1. Check minor mode keymaps (with parent chains)
     (some (fn [minor-mode]
             (lookup-key-in-keymap keymaps [:minor minor-mode] key-sequence-str))
           (reverse (seq minor-modes)))

     ;; 2. Check major mode keymap (with parent chain)
     (lookup-key-in-keymap keymaps [:major major-mode] key-sequence-str)

     ;; 3. Check global keymap (with parent chain)
     (lookup-key-in-keymap keymaps [:global] key-sequence-str)

     ;; Return nil if no binding found
     nil)))

;; -- Event Handlers --

(rf/reg-event-db
 :set-keymap-parent
 (fn [db [_ keymap-path parent-path]]
   "Set the parent keymap for a given keymap.
    keymap-path: [:major :text-mode] or [:minor :linum-mode] or [:global]
    parent-path: [:global] or [:major :fundamental-mode] or nil"
   ;; Detect cycles
   (letfn [(has-cycle? [keymaps current-path target-path visited]
             (when (and current-path (not (visited current-path)))
               (if (= current-path target-path)
                 true
                 (when-let [parent (:parent (get-in keymaps current-path))]
                   (recur keymaps parent target-path (conj visited current-path))))))]
     (when (and parent-path (has-cycle? (:keymaps db) parent-path keymap-path #{}))
       (throw (ex-info "Cyclic keymap inheritance detected"
                       {:keymap keymap-path :parent parent-path}))))
   (assoc-in db (concat [:keymaps] keymap-path [:parent]) parent-path)))

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
   (let [awaiting-key? (get-in db [:help :awaiting-key?])
         help-callback (get-in db [:help :callback])
         query-replace-active? (get-in db [:ui :query-replace :active?])
         isearch-active? (get-in db [:ui :isearch :active?])]

     (cond
       ;; If we're in isearch mode, intercept keys
       isearch-active?
       {:fx [[:dispatch [:isearch/handle-key key-str]]]}

       ;; If we're in query-replace mode, intercept keys
       query-replace-active?
       {:fx [[:dispatch [:query-replace/handle-key key-str]]]}

       ;; If we're waiting for a key press for C-h k, handle it specially
       awaiting-key?
       {:fx [[:dispatch (conj help-callback key-str)]]}

       ;; Normal key handling
       :else
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
       (let [keymaps (:keymaps db)
             transient-keymap (:transient-keymap db)
             all-bindings (concat
                           ;; Include transient keymap if active
                           (when transient-keymap
                             (collect-all-bindings-from-keymap keymaps [:transient transient-keymap]))
                           (mapcat #(collect-all-bindings-from-keymap keymaps [:minor %])
                                   (get-active-minor-modes db))
                           (collect-all-bindings-from-keymap keymaps [:major (get-active-major-mode db)])
                           (collect-all-bindings-from-keymap keymaps [:global]))]
         (some (fn [[key-str _cmd]]
                 (and (string? key-str)
                      (.startsWith key-str full-sequence)))
               all-bindings))
       {:fx [[:dispatch [:set-prefix-key-state full-sequence]]]}

       ;; Handle special keys that should insert
       (and (not prefix-state)
            (or (= key-str "SPC")   ; Space
                (= key-str "RET")   ; Enter/Return
                (= key-str "TAB"))) ; Tab
       (let [text-to-insert (case key-str
                              "SPC" " "
                              "RET" "\n"
                              "TAB" "\t"
                              key-str)]
         ;; (println "✍️ Inserting special key:" key-str "as:" (pr-str text-to-insert))
         {:fx [[:dispatch [:editor/queue-transaction {:op :insert :text text-to-insert}]]
               [:dispatch [:clear-prefix-key-state]]]})

       ;; No binding found - check if it's a printable character for insertion (Emacs mode - always insert)
       (and (= (count key-str) 1)
            (not prefix-state)
            (not (re-matches #"[CM]-." key-str))  ; Not a modifier combo
            (>= (.charCodeAt key-str 0) 32))     ; Printable ASCII
       (let [prefix-arg (get-in db [:ui :prefix-argument])
             prefix-active? (get-in db [:ui :prefix-argument-active?])
             repeat-count (if (and prefix-active? prefix-arg) prefix-arg 1)
             text-to-insert (apply str (repeat repeat-count key-str))]
         (println "✍️ Inserting character:" key-str "×" repeat-count "=" text-to-insert)
         {:fx [[:dispatch [:editor/queue-transaction {:op :insert :text text-to-insert}]]
               [:dispatch [:clear-prefix-key-state]]
               (when prefix-active?
                 [:dispatch [:clear-prefix-argument]])]})

           ;; Unknown key sequence - clear state and ignore
           :else
           {:fx [[:dispatch [:clear-prefix-key-state]]]}))))))
