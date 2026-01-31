(ns lexicon.core.kmacro
  "Keyboard Macro System - Record and replay key sequences.

  Emacs-compatible keyboard macro functionality:
  - F3: Start recording (or insert counter if already recording)
  - F4: Stop recording / execute last macro
  - C-x (: Start recording (alternative)
  - C-x ): Stop recording (alternative)
  - C-x e: Execute last macro

  The macro system intercepts key sequences during recording and
  stores them for later playback."
  (:require [re-frame.core :as rf]
            [lexicon.core.api.message :as msg]))

;; =============================================================================
;; State Atoms
;; =============================================================================

;; Whether we're currently recording a macro
(defonce recording? (atom false))

;; The keys being recorded in the current macro
(defonce current-macro (atom []))

;; Ring of saved macros (most recent first)
(defonce macro-ring (atom []))

;; Counter for kmacro-counter feature
(defonce kmacro-counter (atom 0))

;; Counter format string
(defonce kmacro-counter-format (atom "%d"))

;; Flag to prevent recursive macro execution
(defonce executing? (atom false))

;; =============================================================================
;; Core Functions
;; =============================================================================

(defn recording?-fn
  "Return true if currently recording a macro."
  []
  @recording?)

(defn executing?-fn
  "Return true if currently executing a macro."
  []
  @executing?)

(defn start-recording!
  "Start recording a keyboard macro.
   If prefix-arg is provided, set the counter to that value."
  ([]
   (start-recording! nil))
  ([prefix-arg]
   (when-not @recording?
     (reset! recording? true)
     (reset! current-macro [])
     (when (number? prefix-arg)
       (reset! kmacro-counter prefix-arg))
     (msg/message "Defining keyboard macro..."))))

(defn stop-recording!
  "Stop recording and save the macro to the ring.
   Empty macros are not saved."
  []
  (when @recording?
    (reset! recording? false)
    (let [macro @current-macro]
      (if (seq macro)
        (do
          ;; Add to front of ring
          (swap! macro-ring #(vec (cons macro (take 15 %))))  ; Keep max 16 macros
          (msg/message (str "Keyboard macro defined (" (count macro) " keys)")))
        (msg/message "Empty keyboard macro not added to ring")))))

(defn record-key!
  "Record a key sequence during macro recording.
   Called by the keymap handler for each key pressed."
  [key-str]
  (when (and @recording? (not @executing?))
    ;; Don't record F3/F4 as they control the macro system
    (when-not (contains? #{"F3" "F4" "<f3>" "<f4>"} key-str)
      (swap! current-macro conj key-str))))

(defn get-last-macro
  "Return the last recorded macro, or nil if none."
  []
  (first @macro-ring))

(defn insert-counter!
  "Insert the current counter value and increment it.
   Used when F3 is pressed during recording."
  [increment]
  (let [value @kmacro-counter
        formatted (try
                    (js/sprintf @kmacro-counter-format value)
                    (catch :default _
                      (str value)))]
    (swap! kmacro-counter + (or increment 1))
    ;; Dispatch insert event
    (rf/dispatch [:editor/queue-transaction {:op :insert :text formatted}])))

(defn execute-macro-impl!
  "Internal implementation that executes the macro synchronously.
   Called via setTimeout to allow proper event dispatch."
  [macro repeat-count]
  (reset! executing? true)
  (try
    (dotimes [_ repeat-count]
      (doseq [key-str macro]
        ;; Dispatch synchronously - this works because we're outside the event handler
        (rf/dispatch-sync [:handle-key-sequence key-str])))
    (finally
      (reset! executing? false))))

(defn execute-macro!
  "Execute a macro (sequence of key strings).
   If repeat-count is provided, execute that many times.
   Uses setTimeout to escape the current event handler context."
  ([macro]
   (execute-macro! macro 1))
  ([macro repeat-count]
   (when (and (seq macro) (not @executing?) (not @recording?))
     ;; Schedule execution on next tick to escape the event handler context
     ;; This allows dispatch-sync to work properly
     (js/setTimeout #(execute-macro-impl! macro repeat-count) 0))))

(defn call-last-macro!
  "Execute the last recorded macro.
   With prefix arg, execute that many times."
  ([]
   (call-last-macro! 1))
  ([repeat-count]
   (if-let [macro (get-last-macro)]
     (execute-macro! macro repeat-count)
     (msg/message "No keyboard macro defined"))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :kmacro/start-recording
 (fn [{:keys [db]} [_ prefix-arg]]
   "Start recording a keyboard macro (F3 or C-x ()."
   (if @recording?
     ;; Already recording - insert counter
     (do
       (insert-counter! (or prefix-arg 1))
       {:db db})
     ;; Start new recording
     (do
       (start-recording! prefix-arg)
       {:db (assoc db :kmacro-recording? true)}))))

(rf/reg-event-fx
 :kmacro/stop-recording
 (fn [{:keys [db]} [_]]
   "Stop recording a keyboard macro (C-x ))."
   (stop-recording!)
   {:db (assoc db :kmacro-recording? false)}))

(rf/reg-event-fx
 :kmacro/end-or-call
 (fn [{:keys [db]} [_ prefix-arg]]
   "End recording or call last macro (F4).
    If recording, stop. Otherwise, execute last macro."
   (if @recording?
     (do
       (stop-recording!)
       {:db (assoc db :kmacro-recording? false)})
     (do
       (call-last-macro! (or prefix-arg 1))
       {:db db}))))

(rf/reg-event-fx
 :kmacro/call-macro
 (fn [{:keys [db]} [_ prefix-arg]]
   "Execute the last keyboard macro (C-x e)."
   (call-last-macro! (or prefix-arg 1))
   {:db db}))

(rf/reg-event-fx
 :kmacro/record-key
 (fn [{:keys [db]} [_ key-str]]
   "Record a key during macro recording."
   (record-key! key-str)
   {:db db}))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :kmacro/recording?
 (fn [db _]
   (:kmacro-recording? db false)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-kmacro!
  "Initialize the keyboard macro system and register commands."
  []
  ;; Register F3 command
  (rf/dispatch [:register-command :kmacro-start-macro
                {:docstring "Start defining a keyboard macro (F3)"
                 :handler [:kmacro/start-recording]}])

  ;; Register F4 command
  (rf/dispatch [:register-command :kmacro-end-or-call-macro
                {:docstring "End macro definition or call last macro (F4)"
                 :handler [:kmacro/end-or-call]}])

  ;; Register C-x ( command
  (rf/dispatch [:register-command :start-kbd-macro
                {:docstring "Start defining a keyboard macro (C-x ()"
                 :handler [:kmacro/start-recording]}])

  ;; Register C-x ) command
  (rf/dispatch [:register-command :end-kbd-macro
                {:docstring "End keyboard macro definition (C-x ))"
                 :handler [:kmacro/stop-recording]}])

  ;; Register C-x e command
  (rf/dispatch [:register-command :call-last-kbd-macro
                {:docstring "Execute the last keyboard macro (C-x e)"
                 :handler [:kmacro/call-macro]}])

  ;; Set up key bindings
  (rf/dispatch [:keymap/set-global "F3" :kmacro-start-macro])
  (rf/dispatch [:keymap/set-global "F4" :kmacro-end-or-call-macro])
  (rf/dispatch [:keymap/set-global "C-x (" :start-kbd-macro])
  (rf/dispatch [:keymap/set-global "C-x )" :end-kbd-macro])
  (rf/dispatch [:keymap/set-global "C-x e" :call-last-kbd-macro]))
