(ns lexicon.evil.command.dispatcher
  "Command dispatcher with metadata inspection and operator-motion composition"
  (:require [re-frame.core :as rf]
            [lexicon.evil.fsm.events]))

;; -- Command Registry --

(defonce command-registry (atom {}))

(defn register-command!
  "Register a command function in the command registry with metadata"
  [command-key command-fn]
  (swap! command-registry assoc command-key command-fn)
  (js/console.log "🎮 Registered command:" command-key "with metadata:" (meta command-fn)))

(defn get-command
  "Get a command function from the registry"
  [command-key]
  (get @command-registry command-key))

(defn get-command-metadata
  "Get metadata for a command by inspecting the function var"
  [command-key]
  (when-let [command-fn (get-command command-key)]
    (meta command-fn)))

(defn list-commands-by-type
  "List all commands of a specific type"
  [type]
  (reduce-kv 
   (fn [acc command-key command-fn]
     (let [metadata (meta command-fn)]
       (if (= (:type metadata) type)
         (conj acc {:key command-key :fn command-fn :metadata metadata})
         acc)))
   []
   @command-registry))

(defn is-motion?
  "Check if a command is a motion"
  [command-key]
  (let [metadata (get-command-metadata command-key)]
    (:motion? metadata false)))

(defn is-operator?
  "Check if a command is an operator"
  [command-key]
  (let [metadata (get-command-metadata command-key)]
    (:operator? metadata false)))

(defn is-text-object?
  "Check if a command is a text object"
  [command-key]
  (let [metadata (get-command-metadata command-key)]
    (:text-object? metadata false)))

(defn accepts-count?
  "Check if a command accepts a count prefix"
  [command-key]
  (let [metadata (get-command-metadata command-key)]
    (:count? metadata false)))

(defn is-repeatable?
  "Check if a command is repeatable with ."
  [command-key]
  (let [metadata (get-command-metadata command-key)]
    (:repeatable? metadata false)))

;; -- Motion Execution with Range Calculation --

(defn execute-motion
  "Execute a motion and return the text range it covers"
  [motion-key count cursor-pos]
  (when-let [motion-fn (get-command motion-key)]
    (let [metadata (get-command-metadata motion-key)
          result (motion-fn count)
          motion-type (:type metadata :exclusive)]
      
      ;; Motion functions should return new cursor position
      ;; For now, we simulate range calculation
      (let [new-pos (or result (+ cursor-pos count))]
        {:start (min cursor-pos new-pos)
         :end (max cursor-pos new-pos)
         :type motion-type}))))

;; -- Operator-Motion Composition --

(defn execute-operator-with-motion
  "Execute operator with motion using verb-noun grammar"
  [operator-key motion-key]
  (let [operator-fn (get-command operator-key)
        motion-fn (get-command motion-key)
        operator-metadata (get-command-metadata operator-key)
        motion-metadata (get-command-metadata motion-key)]
    
    (when (and operator-fn motion-fn 
               (:operator? operator-metadata)
               (:motion? motion-metadata))
      
      (js/console.log "🔄 Executing operator-motion composition:"
                      operator-key "+" motion-key)
      
      ;; Get current cursor position from app state
      (let [cursor-pos (get-in @re-frame.db/app-db [:ui :cursor-position] 0)
            count (get-in @re-frame.db/app-db [:fsm :count-register] 1)
            motion-range (execute-motion motion-key count cursor-pos)]
        
        ;; Execute operator with the text range
        (when motion-range
          (operator-fn motion-range)
          
          ;; Record command for repeat (.)
          (when (:repeatable? operator-metadata)
            (rf/dispatch [:fsm/set-repeat-command 
                         {:operator operator-key 
                          :motion motion-key 
                          :count count}]))
          
          ;; Clear FSM state
          (rf/dispatch [:fsm/clear-pending-operations])
          
          motion-range)))))

;; -- Enhanced Command Dispatcher --

(rf/reg-event-fx
 :command/dispatch
 (fn [{:keys [db]} [_ command-key & args]]
   "Enhanced command dispatcher with metadata inspection"
   (let [command-fn (get-command command-key)
         command-metadata (get-command-metadata command-key)
         fsm-state (:fsm db)
         current-state (:current-state fsm-state)
         operator-pending (:operator-pending fsm-state)
         count-register (:count-register fsm-state)]
     
     (cond
       ;; No command found
       (not command-fn)
       (do
         (js/console.warn "Command not found:" command-key)
         {:db db})
       
       ;; We're in operator-pending state and received a motion
       (and operator-pending (:motion? command-metadata))
       (let [result (execute-operator-with-motion operator-pending command-key)]
         (js/console.log "✅ Operator-motion composition completed:" result)
         {:db db})
       
       ;; Received an operator command
       (:operator? command-metadata)
       (do
         (js/console.log "⏳ Setting operator pending:" command-key)
         {:fx [[:dispatch [:fsm/enter-operator-pending-mode command-key]]]})
       
       ;; Simple command execution
       :else
       (do
         (js/console.log "▶️ Executing command:" command-key)

         ;; Execute command with appropriate arguments
         (let [final-args (if (:count? command-metadata)
                           (cons (or count-register 1) args)
                           args)]
           (apply command-fn final-args)

           ;; Record command for repeat if repeatable
           (when (:repeatable? command-metadata)
             (rf/dispatch [:fsm/set-repeat-command
                          {:command command-key
                           :args final-args}])))

         {:db db})))))

;; -- Repeat Command Support --

(rf/reg-event-fx
 :command/repeat-last
 (fn [{:keys [db]} [_]]
   "Repeat the last repeatable command (. in Vim)"
   (let [last-command (get-in db [:fsm :repeat-last-command])]
     (if last-command
       (do
         (js/console.log "🔁 Repeating last command:" last-command)
         (cond
           ;; Operator-motion composition
           (and (:operator last-command) (:motion last-command))
           (let [result (execute-operator-with-motion
                        (:operator last-command)
                        (:motion last-command))]
             (js/console.log "🔁 Repeated operator-motion:" result)
             {:db db})
           
           ;; Simple command
           (:command last-command)
           {:fx [[:dispatch (into [:command/dispatch (:command last-command)] 
                                 (:args last-command))]]}
           
           :else
           (do
             (js/console.warn "Invalid repeat command format:" last-command)
             {:db db})))

       (do
         (js/console.log "No command to repeat")
         {:db db})))))

;; -- Text Object Support --

(defn execute-text-object
  "Execute a text object and return the selected range"
  [text-object-key inner? cursor-pos]
  (when-let [text-object-fn (get-command text-object-key)]
    (let [metadata (get-command-metadata text-object-key)]
      (when (:text-object? metadata)
        (text-object-fn inner?)))))

;; -- Count Register Management --

(rf/reg-event-fx
 :command/set-count
 (fn [{:keys [db]} [_ digit]]
   "Build up count register from numeric input"
   (let [current-count (get-in db [:fsm :count-register] 0)
         new-count (+ (* current-count 10) digit)]
     {:fx [[:dispatch [:fsm/set-count-register new-count]]]})))

(rf/reg-event-fx
 :command/clear-count
 (fn [{:keys [db]} [_]]
   "Clear the count register"
   {:fx [[:dispatch [:fsm/set-count-register nil]]]}))

;; -- Command Inspection and Debug Support --

(rf/reg-event-fx
 :command/inspect
 (fn [{:keys [db]} [_ command-key]]
   "Inspect a command's metadata for debugging"
   (let [metadata (get-command-metadata command-key)]
     (js/console.log "🔍 Command inspection for" command-key ":" metadata)
     {:db db})))

(rf/reg-event-fx
 :command/list-by-type
 (fn [{:keys [db]} [_ type]]
   "List all commands of a specific type"
   (let [commands (list-commands-by-type type)]
     (js/console.log "📋 Commands of type" type ":" commands)
     {:db db})))

;; -- Integration with Existing Command System --

(rf/reg-event-fx
 :modal/dispatch-key
 (fn [{:keys [db]} [_ key-input]]
   "FSM-aware command dispatcher that handles operator-motion composition"
   (let [fsm-state (:fsm db)
         current-state (:current-state fsm-state)
         active-keymap (:active-keymap fsm-state)
         keymap (get-in db [:keymaps active-keymap])
         command-key (get keymap key-input)]
     
     (if command-key
       {:fx [[:dispatch [:command/dispatch command-key]]]}
       (do
         (js/console.log "No command bound to key:" key-input "in keymap:" active-keymap)
         {:db db})))))

;; -- Utility Functions --

(defn get-all-motions
  "Get all registered motion commands"
  []
  (list-commands-by-type :motion))

(defn get-all-operators
  "Get all registered operator commands"
  []
  (list-commands-by-type :operator))

(defn get-all-text-objects
  "Get all registered text object commands"
  []
  (list-commands-by-type :text-object))