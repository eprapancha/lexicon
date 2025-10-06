(ns lexicon.fsm.interceptors
  "Re-frame interceptors for FSM state lifecycle hooks"
  (:require [re-frame.core :as rf]
            [lexicon.fsm.events]))

;; -- Hook Registry --

(defonce state-enter-hooks (atom {}))
(defonce state-exit-hooks (atom {}))

(defn register-state-enter-hook!
  "Register a hook function to be called when entering a state.
   Hook function signature: (fn [old-state new-state fsm-context db] ...)"
  [state hook-fn]
  (swap! state-enter-hooks update state (fnil conj []) hook-fn))

(defn register-state-exit-hook!
  "Register a hook function to be called when exiting a state.
   Hook function signature: (fn [old-state new-state fsm-context db] ...)"
  [state hook-fn]
  (swap! state-exit-hooks update state (fnil conj []) hook-fn))

(defn unregister-state-enter-hook!
  "Unregister a specific enter hook for a state"
  [state hook-fn]
  (swap! state-enter-hooks update state 
         (fn [hooks] (vec (remove #(= % hook-fn) hooks)))))

(defn unregister-state-exit-hook!
  "Unregister a specific exit hook for a state"
  [state hook-fn]
  (swap! state-exit-hooks update state 
         (fn [hooks] (vec (remove #(= % hook-fn) hooks)))))

(defn clear-hooks-for-state!
  "Clear all hooks for a specific state"
  [state]
  (swap! state-enter-hooks dissoc state)
  (swap! state-exit-hooks dissoc state))

;; -- Hook Execution Helpers --

(defn execute-exit-hooks
  "Execute all exit hooks for a state"
  [old-state new-state fsm-context db]
  (let [exit-hooks (get @state-exit-hooks old-state [])]
    (doseq [hook exit-hooks]
      (try
        (hook old-state new-state fsm-context db)
        (catch js/Error e
          (js/console.error "Error in FSM exit hook for state" old-state ":" (.-message e))
          (js/console.error e))))))

(defn execute-enter-hooks
  "Execute all enter hooks for a state"
  [old-state new-state fsm-context db]
  (let [enter-hooks (get @state-enter-hooks new-state [])]
    (doseq [hook enter-hooks]
      (try
        (hook old-state new-state fsm-context db)
        (catch js/Error e
          (js/console.error "Error in FSM enter hook for state" new-state ":" (.-message e))
          (js/console.error e))))))

;; -- Core Interceptors --

(def fsm-on-exit-interceptor
  "Interceptor that executes exit hooks before FSM state transitions"
  (rf/->interceptor
    :id :fsm/on-exit
    :before (fn [{:keys [coeffects] :as context}]
              (let [db (:db coeffects)
                    current-state (get-in db [:fsm :current-state])
                    fsm-context (get-in db [:fsm :state-context])
                    event (:event coeffects)
                    [event-id new-state & _] event]
                
                ;; Only execute exit hooks for FSM state transition events
                (when (and (= event-id :fsm/update-state)
                           current-state
                           (not= current-state new-state))
                  (execute-exit-hooks current-state new-state fsm-context db))
                
                context))))

(def fsm-on-enter-interceptor
  "Interceptor that executes enter hooks after FSM state transitions"
  (rf/->interceptor
    :id :fsm/on-enter
    :after (fn [{:keys [effects] :as context}]
             (let [db (:db effects)
                   event (get-in context [:coeffects :event])
                   [event-id new-state & _] event]
               
               ;; Only execute enter hooks for FSM state transition events
               (when (and (= event-id :fsm/update-state) db)
                 (let [new-fsm-state (get-in db [:fsm :current-state])
                       previous-state (get-in db [:fsm :previous-state])
                       fsm-context (get-in db [:fsm :state-context])]
                   
                   (when (and new-fsm-state 
                              (not= previous-state new-fsm-state))
                     (execute-enter-hooks previous-state new-fsm-state fsm-context db))))
               
               context))))

;; -- Enhanced FSM State Transition Event with Interceptors --

(rf/reg-event-db
 :fsm/transition-to
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [db [_ new-state & {:keys [context] :as opts}]]
   "Transition FSM to new state with lifecycle hooks"
   (rf/dispatch [:fsm/update-state new-state :context context])
   db))

;; -- Convenience Functions for Common State Transitions --

(rf/reg-event-fx
 :fsm/enter-normal-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_]]
   "Enter normal mode with hooks"
   {:fx [[:dispatch [:fsm/update-state :normal]]]}))

(rf/reg-event-fx
 :fsm/enter-insert-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_ & {:keys [position] :or {position :at-cursor}}]]
   "Enter insert mode with hooks"
   (let [context {:insert-position position}]
     {:fx [[:dispatch [:fsm/update-state :insert :context context]]]})))

(rf/reg-event-fx
 :fsm/enter-visual-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_ & {:keys [mode] :or {mode :normal}}]]
   "Enter visual mode with hooks"
   (let [cursor-pos (get-in db [:ui :cursor-position])
         context {:selection-start cursor-pos :visual-mode mode}]
     {:fx [[:dispatch [:fsm/update-state :visual :context context]]
           [:dispatch [:fsm/set-selection-anchor cursor-pos]]
           [:dispatch [:fsm/set-selection-mode mode]]]})))

(rf/reg-event-fx
 :fsm/enter-visual-line-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_]]
   "Enter visual line mode with hooks"
   {:fx [[:dispatch [:fsm/enter-visual-mode :mode :line]]]}))

(rf/reg-event-fx
 :fsm/enter-visual-block-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_]]
   "Enter visual block mode with hooks"
   {:fx [[:dispatch [:fsm/enter-visual-mode :mode :block]]]}))

(rf/reg-event-fx
 :fsm/enter-operator-pending-mode
 [fsm-on-exit-interceptor fsm-on-enter-interceptor]
 (fn [{:keys [db]} [_ operator-fn]]
   "Enter operator-pending mode with hooks"
   (let [context {:operator operator-fn}]
     {:fx [[:dispatch [:fsm/update-state :operator-pending :context context :operator operator-fn]]]})))

;; -- Default State Hooks --

(defn cursor-style-hook
  "Hook to update cursor style based on FSM state"
  [old-state new-state fsm-context db]
  (case new-state
    :normal (js/console.log "🎯 FSM: Setting block cursor for normal mode")
    :insert (js/console.log "🎯 FSM: Setting line cursor for insert mode")
    :visual (js/console.log "🎯 FSM: Setting selection cursor for visual mode")
    (js/console.log "🎯 FSM: Default cursor for state" new-state)))

(defn mode-line-hook
  "Hook to update mode line display"
  [old-state new-state fsm-context db]
  (js/console.log "📊 FSM: Mode line updated -" old-state "→" new-state))

(defn selection-hook
  "Hook to manage selection state for visual modes"
  [old-state new-state fsm-context db]
  (case new-state
    (:visual :visual-line :visual-block)
    (js/console.log "🔤 FSM: Visual selection activated")
    
    :normal
    (when (contains? #{:visual :visual-line :visual-block} old-state)
      (js/console.log "🔤 FSM: Visual selection cleared")
      (rf/dispatch [:fsm/clear-selection-anchor]))
    
    nil))

(defn operator-pending-hook
  "Hook to handle operator-pending state setup"
  [old-state new-state fsm-context db]
  (when (= new-state :operator-pending)
    (let [operator (:operator fsm-context)]
      (js/console.log "⏳ FSM: Waiting for motion after operator:" operator))))

;; -- Register Default Hooks --

(register-state-enter-hook! :normal cursor-style-hook)
(register-state-enter-hook! :insert cursor-style-hook)
(register-state-enter-hook! :visual cursor-style-hook)
(register-state-enter-hook! :visual-line cursor-style-hook)
(register-state-enter-hook! :visual-block cursor-style-hook)

(register-state-enter-hook! :normal mode-line-hook)
(register-state-enter-hook! :insert mode-line-hook)
(register-state-enter-hook! :visual mode-line-hook)
(register-state-enter-hook! :visual-line mode-line-hook)
(register-state-enter-hook! :visual-block mode-line-hook)
(register-state-enter-hook! :operator-pending mode-line-hook)

(register-state-enter-hook! :visual selection-hook)
(register-state-enter-hook! :visual-line selection-hook)
(register-state-enter-hook! :visual-block selection-hook)
(register-state-exit-hook! :visual selection-hook)
(register-state-exit-hook! :visual-line selection-hook)
(register-state-exit-hook! :visual-block selection-hook)

(register-state-enter-hook! :operator-pending operator-pending-hook)

;; -- Hook Management Events --

(rf/reg-event-db
 :fsm/register-enter-hook
 (fn [db [_ state hook-fn]]
   "Register an enter hook for a state"
   (register-state-enter-hook! state hook-fn)
   db))

(rf/reg-event-db
 :fsm/register-exit-hook
 (fn [db [_ state hook-fn]]
   "Register an exit hook for a state"
   (register-state-exit-hook! state hook-fn)
   db))

(rf/reg-event-db
 :fsm/unregister-enter-hook
 (fn [db [_ state hook-fn]]
   "Unregister an enter hook for a state"
   (unregister-state-enter-hook! state hook-fn)
   db))

(rf/reg-event-db
 :fsm/unregister-exit-hook
 (fn [db [_ state hook-fn]]
   "Unregister an exit hook for a state"
   (unregister-state-exit-hook! state hook-fn)
   db))

(rf/reg-event-db
 :fsm/clear-hooks
 (fn [db [_ state]]
   "Clear all hooks for a state"
   (clear-hooks-for-state! state)
   db))