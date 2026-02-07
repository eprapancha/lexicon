(ns lexicon.core.events.keymap
  "Event handlers for key sequence handling and keymap resolution.

  Handles:
  - Multi-key sequence processing
  - Keymap lookup with Emacs precedence order
  - Prefix key state management
  - Special key insertion (SPC, RET, TAB)"
  (:require [re-frame.core :as rf]
            [lexicon.core.db :as db]
            [lexicon.core.modes.electric-pair :as electric-pair]
            [lexicon.core.modes.delete-selection :as delete-selection]
            [lexicon.core.kmacro :as kmacro]))

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
    ;; 2.5. Mode-specific bindings (Issue #139: from define-key-for-mode)
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

     ;; 2.5. Issue #139: Check mode-specific keymaps (from define-key-for-mode)
     ;; These are stored at [:keymaps :mode mode key-sequence]
     ;; Mode keymaps are stored with keyword keys, but major-mode may be symbol
     (get-in keymaps [:mode (keyword major-mode) key-sequence-str])

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

;; Atom to track the prefix echo timer (for cancellation)
(defonce prefix-echo-timer (atom nil))

(rf/reg-event-fx
 :set-prefix-key-state
 (fn [{:keys [db]} [_ prefix-key]]
   "Set the current prefix key state for multi-key sequences.
   Starts a timer to echo the prefix in the minibuffer after a delay (Emacs-style)."
   ;; Cancel any existing timer
   (when-let [old-timer @prefix-echo-timer]
     (js/clearTimeout old-timer))

   ;; Start a new timer to show prefix after delay (default 1 second like Emacs)
   (let [echo-delay 1000  ; 1 second delay like Emacs echo-keystrokes
         timer-id (js/setTimeout
                    #(rf/dispatch [:prefix-key-echo prefix-key])
                    echo-delay)]
     (reset! prefix-echo-timer timer-id))

   {:db (assoc-in db [:ui :prefix-key-state] prefix-key)}))

(rf/reg-event-fx
 :prefix-key-echo
 (fn [{:keys [db]} [_ prefix-key]]
   "Echo the prefix key in the minibuffer with a trailing dash (like Emacs)."
   ;; Only show if we're still waiting for the same prefix
   (if (= prefix-key (get-in db [:ui :prefix-key-state]))
     {:db (assoc-in db [:ui :prefix-key-echoed?] true)
      :fx [[:dispatch [:echo/message (str prefix-key "-") true]]]}  ; persist=true
     {:db db})))

(rf/reg-event-fx
 :clear-prefix-key-state
 (fn [{:keys [db]} [_]]
   "Clear the prefix key state and cancel any pending echo timer."
   ;; Cancel the echo timer
   (when-let [timer @prefix-echo-timer]
     (js/clearTimeout timer)
     (reset! prefix-echo-timer nil))

   ;; Clear the echo if we had echoed the prefix
   (let [was-echoed? (get-in db [:ui :prefix-key-echoed?])]
     (cond-> {:db (-> db
                      (assoc-in [:ui :prefix-key-state] nil)
                      (assoc-in [:ui :prefix-key-echoed?] false))}
       ;; Only add :fx when we need to clear the echo
       was-echoed? (assoc :fx [[:dispatch [:echo/clear]]])))))

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
       (do
         ;; Record key for keyboard macros if recording
         (when (kmacro/recording?-fn)
           (kmacro/record-key! key-str))

         (let [prefix-state (get-in db [:ui :prefix-key-state])
               full-sequence (if prefix-state
                              (str prefix-state " " key-str)
                              key-str)
               command-name (resolve-keybinding db full-sequence)]

         (cond
       ;; Found a complete command binding
       command-name
       (let [;; command-name might be a keyword or a vector like [:digit-argument 0]
             dispatch-vec (if (vector? command-name)
                           (into [:execute-command] command-name)
                           [:execute-command command-name])]
         {:fx [[:dispatch dispatch-vec]
               [:dispatch [:clear-prefix-key-state]]]})

       ;; Check if this might be a prefix for a multi-key sequence
       ;; by seeing if any keybinding starts with this sequence FOLLOWED BY A SPACE
       ;; This ensures single keys like "S" don't match "S-Insert" (which is a shifted key, not a sequence)
       (let [keymaps (:keymaps db)
             transient-keymap (:transient-keymap db)
             all-bindings (concat
                           ;; Include transient keymap if active
                           (when transient-keymap
                             (collect-all-bindings-from-keymap keymaps [:transient transient-keymap]))
                           (mapcat #(collect-all-bindings-from-keymap keymaps [:minor %])
                                   (get-active-minor-modes db))
                           (collect-all-bindings-from-keymap keymaps [:major (get-active-major-mode db)])
                           (collect-all-bindings-from-keymap keymaps [:global]))
             ;; For multi-key sequences like "C-x C-f", check if any binding starts with
             ;; the full-sequence followed by a space. This prevents "S" from matching "S-Insert".
             prefix-with-space (str full-sequence " ")]
         (some (fn [[key-str _cmd]]
                 (and (string? key-str)
                      (.startsWith key-str prefix-with-space)))
               all-bindings))
       {:fx [[:dispatch [:set-prefix-key-state full-sequence]]]}

       ;; Handle special keys that should insert
       (and (not prefix-state)
            (or (= key-str "SPC")   ; Space
                (= key-str "RET")   ; Enter/Return
                (= key-str "TAB"))) ; Tab
       (let [;; Handle prefix argument for repeat count
             prefix-arg (:prefix-arg db)
             repeat-count (if prefix-arg
                            (cond
                              (number? prefix-arg) prefix-arg
                              (list? prefix-arg) (first prefix-arg)
                              :else 1)
                            1)
             base-text (case key-str
                         "SPC" " "
                         "RET" "\n"
                         "TAB" "\t"
                         key-str)
             text-to-insert (apply str (repeat repeat-count base-text))
             ;; Delete-selection-mode: check if region should be deleted first
             delete-region? (and (delete-selection/delete-selection-enabled? db)
                                 (delete-selection/region-active? db))
             region-bounds (when delete-region? (delete-selection/get-region-bounds db))]
         ;; (println "✍️ Inserting special key:" key-str "as:" (pr-str text-to-insert))
         {:db (assoc db :last-command :self-insert-command)  ; Break consecutive kill chain
          :fx (cond-> []
                ;; If delete-selection is active, delete region first
                delete-region?
                (conj [:dispatch [:editor/queue-transaction
                                  {:op :delete-range
                                   :start (:start region-bounds)
                                   :length (- (:end region-bounds) (:start region-bounds))}]]
                      [:dispatch [:deactivate-mark]])
                ;; Then insert the text
                true
                (conj [:dispatch [:editor/queue-transaction {:op :insert :text text-to-insert}]]
                      [:dispatch [:clear-prefix-key-state]]
                      [:dispatch [:clear-prefix-arg]]))})

       ;; Issue #137: Route printable chars to minibuffer when *Completions* buffer is active
       ;; This is a user-friendly enhancement over Emacs (which just shows "read-only" error).
       ;; Allows typing to continue filtering completions even when focus is in the completions window.
       (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
             active-buffer (get (:buffers db) (:buffer-id active-window))
             in-completions? (= (:name active-buffer) "*Completions*")
             minibuffer-active? (get-in db [:minibuffer :active?])
             is-printable? (and (= (count key-str) 1)
                                (not prefix-state)
                                (not (re-matches #"[CM]-." key-str))
                                (>= (.charCodeAt key-str 0) 32))]
         (and in-completions? minibuffer-active? is-printable?))
       ;; Route to minibuffer: use minibuffer/set-input which handles all completion types
       (let [current-input (get-in db [:minibuffer :input] "")
             new-input (str current-input key-str)]
         {:fx [[:dom/focus-minibuffer nil]
               [:dispatch [:minibuffer/set-input new-input]]]})

       ;; No binding found - check if it's a printable character for insertion (Emacs mode - always insert)
       (and (= (count key-str) 1)
            (not prefix-state)
            (not (re-matches #"[CM]-." key-str))  ; Not a modifier combo
            (>= (.charCodeAt key-str 0) 32))     ; Printable ASCII
       (let [;; Phase 6.5: Use new prefix-arg location and convert to number
             prefix-arg (:prefix-arg db)
             repeat-count (if prefix-arg
                           (cond
                             (number? prefix-arg) prefix-arg
                             (list? prefix-arg) (first prefix-arg)
                             :else 1)
                           1)
             ;; Delete-selection-mode: check if region should be deleted first
             delete-region? (and (delete-selection/delete-selection-enabled? db)
                                 (delete-selection/region-active? db))
             region-bounds (when delete-region? (delete-selection/get-region-bounds db))
             ;; Electric-pair-mode: check if this is an opening delimiter
             char (first key-str)
             closing-char (when (electric-pair/electric-pair-enabled? db)
                            (electric-pair/get-closing-char char))
             ;; If electric-pair, insert opening + closing and move cursor back
             text-to-insert (if closing-char
                              (str (apply str (repeat repeat-count key-str))
                                   (apply str (repeat repeat-count closing-char)))
                              (apply str (repeat repeat-count key-str)))
             ;; For electric-pair, we need to move cursor back after insert
             ;; Pass cursor-adjust in transaction so it's applied after async insert completes
             cursor-adjustment (when closing-char (- repeat-count))
             transaction (cond-> {:op :insert :text text-to-insert}
                           cursor-adjustment (assoc :cursor-adjust cursor-adjustment))]
         {:db (assoc db :last-command :self-insert-command)  ; Break consecutive kill chain
          :fx (cond-> []
                ;; If delete-selection is active, delete region first
                delete-region?
                (conj [:dispatch [:editor/queue-transaction
                                  {:op :delete-range
                                   :start (:start region-bounds)
                                   :length (- (:end region-bounds) (:start region-bounds))}]]
                      [:dispatch [:deactivate-mark]])
                ;; Then insert the text
                true
                (conj [:dispatch [:editor/queue-transaction transaction]]
                      [:dispatch [:clear-prefix-key-state]])
                ;; Phase 6.5: Clear prefix-arg after self-insert
                prefix-arg (conj [:dispatch [:clear-prefix-arg]]))})

           ;; Unknown key sequence - clear state and ignore
           :else
           {:fx [[:dispatch [:clear-prefix-key-state]]]})))))))

;; -- Simple Test API Events --
;; These provide a simple interface for tests to set keybindings directly

(rf/reg-event-db
 :keymap/set-global
 (fn [db [_ key-sequence command]]
   "Set a global keybinding (test API).

   Args:
     key-sequence - String like \"C-x C-f\"
     command - Keyword or function to invoke"
   (assoc-in db [:keymaps :global :bindings key-sequence] command)))

(rf/reg-event-db
 :keymap/set-local
 (fn [db [_ buffer-id key-sequence command]]
   "Set a buffer-local keybinding (test API).

   Args:
     buffer-id - Buffer to set binding for
     key-sequence - String like \"C-x C-f\"
     command - Keyword or function to invoke"
   (assoc-in db [:buffers buffer-id :local-keymap key-sequence] command)))

(rf/reg-event-db
 :keymap/set-mode-key
 (fn [db [_ mode key-sequence command]]
   "Set a mode-specific keybinding (Issue #139).

   Args:
     mode - Mode symbol (e.g., 'dired-mode)
     key-sequence - String like \"n\" or \"RET\"
     command - Command symbol or function to invoke

   Mode keymaps are checked after local keymaps but before global."
   (let [mode-kw (if (symbol? mode) (keyword mode) mode)]
     (assoc-in db [:keymaps :mode mode-kw key-sequence] command))))

(rf/reg-event-fx
 :input/keys
 (fn [{:keys [db]} [_ key-sequence]]
   "Simulate key input for testing (test API).

   Accumulates in :pending-keys until a complete binding is found.

   Args:
     key-sequence - String like \"C-x\" or \"C-f\""
   (let [pending (get-in db [:pending-keys] "")
         full-sequence (if (empty? pending)
                        key-sequence
                        (str pending " " key-sequence))
         active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         buffer-id (:buffer-id active-window)
         ;; Issue #139: Get buffer's major mode for mode-specific keymaps
         buffer-mode (get-in db [:buffers buffer-id :major-mode])

         ;; Try to resolve: buffer-local -> mode keymap -> global
         command (or (get-in db [:buffers buffer-id :local-keymap full-sequence])
                     (when buffer-mode
                       (get-in db [:keymaps :mode buffer-mode full-sequence]))
                     (get-in db [:keymaps :global :bindings full-sequence]))]

     (if command
       ;; Found complete binding - invoke and clear
       {:db (-> db
                (assoc :last-invoked-command command)
                (assoc :pending-keys ""))
        :fx [[:dispatch [:execute-command command]]]}
       ;; No complete binding - accumulate
       {:db (assoc db :pending-keys full-sequence)}))))
