(ns lexicon.evil.fsm.events
  "Pure FSM state transition system for modal editing"
  (:require [re-frame.core :as rf]
            [clojure.spec.alpha :as s]))

;; -- FSM State Specification --

(s/def ::fsm-state #{:normal :insert :visual :visual-line :visual-block :operator-pending :replace :command-line})

(s/def ::selection-mode #{:normal :line :block})

(s/def ::search-direction #{:forward :backward})

(s/def ::fsm-context 
  (s/keys :req-un [::current-state ::previous-state]
          :opt-un [::state-context ::operator-pending ::motion-pending ::count-register ::register-name 
                   ::selection-mode ::selection-anchor ::last-search ::repeat-last-command 
                   ::macro-recording ::macro-registry ::command-history ::transition-hooks]))

;; -- Pure State Transition Functions --

(defn valid-transition?
  "Check if a state transition is valid"
  [from-state to-state]
  (case from-state
    :normal       (contains? #{:insert :visual :visual-line :visual-block :operator-pending :replace :command-line} to-state)
    :insert       (contains? #{:normal} to-state)
    :visual       (contains? #{:normal :visual-line :visual-block} to-state)
    :visual-line  (contains? #{:normal :visual :visual-block} to-state)
    :visual-block (contains? #{:normal :visual :visual-line} to-state)
    :operator-pending (contains? #{:normal} to-state)
    :replace      (contains? #{:normal} to-state)
    :command-line (contains? #{:normal} to-state)
    false))

(defn transition-fsm-state
  "Pure function to transition FSM state with validation"
  [fsm-state new-state & {:keys [context operator motion count register] 
                          :or {context {}}}]
  (let [current-state (:current-state fsm-state)
        previous-state current-state]
    
    (if (valid-transition? current-state new-state)
      (-> fsm-state
          (assoc :current-state new-state)
          (assoc :previous-state previous-state)
          (assoc :state-context context)
          (cond->
            operator (assoc :operator-pending operator)
            motion   (assoc :motion-pending motion)
            count    (assoc :count-register count)
            register (assoc :register-name register)
            
            ;; Clear operator/motion state when transitioning to normal
            (= new-state :normal) (-> (assoc :operator-pending nil)
                                      (assoc :motion-pending nil)
                                      (assoc :count-register nil)
                                      (assoc :register-name nil))))
      
      ;; Invalid transition - return unchanged state
      (do
        (js/console.warn "Invalid FSM transition:" current-state "->" new-state)
        fsm-state))))

(defn get-keymap-for-state
  "Get the appropriate keymap for a given FSM state"
  [state]
  (case state
    :normal :normal-keymap
    :insert :insert-keymap
    :visual :visual-keymap
    :visual-line :visual-keymap
    :visual-block :visual-keymap
    :operator-pending :operator-pending-keymap
    :replace :insert-keymap
    :command-line :minibuffer-keymap
    :normal-keymap)) ; default fallback

(defn clear-pending-operations
  "Clear all pending operations from FSM state"
  [fsm-state]
  (-> fsm-state
      (assoc :operator-pending nil)
      (assoc :motion-pending nil)
      (assoc :count-register nil)
      (assoc :register-name nil)))

(defn set-pending-operator
  "Set a pending operator in FSM state"
  [fsm-state operator-fn]
  (assoc fsm-state :operator-pending operator-fn))

(defn set-pending-motion
  "Set a pending motion in FSM state"
  [fsm-state motion-fn]
  (assoc fsm-state :motion-pending motion-fn))

(defn set-count-register
  "Set a count register for command repetition"
  [fsm-state count]
  (assoc fsm-state :count-register count))

(defn record-command-history
  "Add a command to the FSM command history"
  [fsm-state command]
  (update fsm-state :command-history 
          (fn [history]
            (take 100 (conj history {:command command :timestamp (js/Date.now)})))))

;; -- Core FSM State Transition Event Handler --

(rf/reg-event-db
 :fsm/update-state
 (fn [db [_ new-state & {:keys [context operator motion count register] :as opts}]]
   "Pure FSM state transition handler - core entry point for all state changes"
   (let [current-fsm (:fsm db)
         updated-fsm (transition-fsm-state current-fsm new-state 
                                          :context context
                                          :operator operator
                                          :motion motion
                                          :count count
                                          :register register)
         new-keymap (get-keymap-for-state (:current-state updated-fsm))]
     
     ;; Update the FSM state and active keymap
     (-> db
         (assoc :fsm updated-fsm)
         (assoc-in [:fsm :active-keymap] new-keymap)))))

;; -- Specific FSM Operation Events --

(rf/reg-event-db
 :fsm/set-operator-pending
 (fn [db [_ operator-fn]]
   "Set a pending operator and transition to operator-pending state"
   (let [updated-fsm (-> (:fsm db)
                         (set-pending-operator operator-fn)
                         (record-command-history {:type :operator :fn operator-fn}))]
     (-> db
         (assoc :fsm updated-fsm)
         (assoc-in [:fsm :current-state] :operator-pending)
         (assoc-in [:fsm :active-keymap] :operator-pending-keymap)))))

(rf/reg-event-db
 :fsm/set-motion-pending
 (fn [db [_ motion-fn]]
   "Set a pending motion in the FSM state"
   (update db :fsm set-pending-motion motion-fn)))

(rf/reg-event-db
 :fsm/clear-pending-operations
 (fn [db [_]]
   "Clear all pending operations and return to normal state"
   (let [updated-fsm (-> (:fsm db)
                         (clear-pending-operations)
                         (assoc :current-state :normal))]
     (-> db
         (assoc :fsm updated-fsm)
         (assoc-in [:fsm :active-keymap] :normal-keymap)))))

(rf/reg-event-db
 :fsm/set-count-register
 (fn [db [_ count]]
   "Set the count register for command repetition"
   (update db :fsm set-count-register count)))

(rf/reg-event-db
 :fsm/record-command
 (fn [db [_ command]]
   "Record a command in the FSM history"
   (update db :fsm record-command-history command)))

;; -- Selection Management for Visual Modes --

(rf/reg-event-db
 :fsm/set-selection-anchor
 (fn [db [_ position]]
   "Set the selection anchor for visual mode"
   (assoc-in db [:fsm :selection-anchor] position)))

(rf/reg-event-db
 :fsm/clear-selection-anchor
 (fn [db [_]]
   "Clear the selection anchor"
   (assoc-in db [:fsm :selection-anchor] nil)))

(rf/reg-event-db
 :fsm/set-selection-mode
 (fn [db [_ mode]]
   "Set the visual selection mode (:normal, :line, :block)"
   (if (contains? #{:normal :line :block} mode)
     (assoc-in db [:fsm :selection-mode] mode)
     (do
       (js/console.warn "Invalid selection mode:" mode)
       db))))

;; -- Macro Recording Support --

(rf/reg-event-db
 :fsm/start-macro-recording
 (fn [db [_ register-name]]
   "Start recording a macro to the specified register"
   (-> db
       (assoc-in [:fsm :macro-recording] register-name)
       (assoc-in [:fsm :macro-registry register-name] []))))

(rf/reg-event-db
 :fsm/stop-macro-recording
 (fn [db [_]]
   "Stop macro recording"
   (assoc-in db [:fsm :macro-recording] nil)))

(rf/reg-event-db
 :fsm/add-to-macro
 (fn [db [_ command]]
   "Add a command to the currently recording macro"
   (let [recording-register (:macro-recording (:fsm db))]
     (if recording-register
       (update-in db [:fsm :macro-registry recording-register] conj command)
       db))))

;; -- Search State Management --

(rf/reg-event-db
 :fsm/set-last-search
 (fn [db [_ pattern direction case-sensitive]]
   "Update the last search information"
   (assoc-in db [:fsm :last-search] 
             {:pattern pattern 
              :direction direction 
              :case-sensitive case-sensitive})))

;; -- Command Repetition Support --

(rf/reg-event-db
 :fsm/set-repeat-command
 (fn [db [_ command]]
   "Set the last command for . (repeat) operation"
   (assoc-in db [:fsm :repeat-last-command] command)))

;; -- Query Functions (for subscriptions) --

(rf/reg-sub
 :fsm/current-state
 (fn [db _]
   "Get the current FSM state"
   (get-in db [:fsm :current-state])))

(rf/reg-sub
 :fsm/operator-pending
 (fn [db _]
   "Get the pending operator, if any"
   (get-in db [:fsm :operator-pending])))

(rf/reg-sub
 :fsm/motion-pending
 (fn [db _]
   "Get the pending motion, if any"
   (get-in db [:fsm :motion-pending])))

(rf/reg-sub
 :fsm/count-register
 (fn [db _]
   "Get the current count register value"
   (get-in db [:fsm :count-register])))

(rf/reg-sub
 :fsm/active-keymap
 (fn [db _]
   "Get the active keymap for the current FSM state"
   (get-in db [:fsm :active-keymap])))

(rf/reg-sub
 :fsm/selection-mode
 (fn [db _]
   "Get the current visual selection mode"
   (get-in db [:fsm :selection-mode])))

(rf/reg-sub
 :fsm/selection-anchor
 (fn [db _]
   "Get the selection anchor position"
   (get-in db [:fsm :selection-anchor])))

(rf/reg-sub
 :fsm/macro-recording
 (fn [db _]
   "Get the register name of the currently recording macro, if any"
   (get-in db [:fsm :macro-recording])))

(rf/reg-sub
 :fsm/last-search
 (fn [db _]
   "Get the last search information"
   (get-in db [:fsm :last-search])))

(rf/reg-sub
 :fsm/repeat-command
 (fn [db _]
   "Get the command available for . (repeat) operation"
   (get-in db [:fsm :repeat-last-command])))