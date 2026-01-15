(ns lexicon.advice
  "Advice system for modifying function behavior.

  Allows packages to wrap or modify existing functions without directly editing them.

  Advice types:
  - :before - Run before original function (args -> modified-args)
  - :after - Run after original function (result -> modified-result)
  - :around - Wrap original function (fn [original-fn & args] ...)
  - :override - Replace original function entirely

  Multiple advice can stack on the same function.

  Usage:
    (advice-add! :my-function :before my-advice-fn)
    (advice-add! :my-function :after another-advice-fn)
    (advice-remove! :my-function :before my-advice-fn)

  When function is called, advice chain executes:
    1. All :before advice (can modify args)
    2. All :around advice (can wrap execution)
    3. Original function (or :override if present)
    4. All :after advice (can modify result)"
  (:require [re-frame.core :as rf]
            [lexicon.log :as log]))

;; -- Advice Registry --

(defn advice-add!
  "Add ADVICE-FN as TYPE advice to FUNCTION-KEY.

  Args:
  - function-key: Keyword identifying the function (:buffer/insert, etc.)
  - type: Advice type (:before, :after, :around, :override)
  - advice-fn: Function implementing the advice
  - props: Optional properties map {:name 'advice-name' :depth 0}

  Depth controls order (lower depth runs first)."
  [function-key type advice-fn & [props]]
  (rf/dispatch [:advice/add function-key type advice-fn props]))

(rf/reg-event-db
  :advice/add
  (fn [db [_ function-key type advice-fn props]]
    (let [advice-entry {:type type
                       :function advice-fn
                       :name (or (:name props) (gensym "advice-"))
                       :depth (or (:depth props) 0)}
          advice-path [:advice function-key type]]
      ;; Add advice to list for this type
      (update-in db advice-path
                 (fn [advice-list]
                   (let [advice-list (or advice-list [])]
                     ;; Sort by depth
                     (sort-by :depth (conj advice-list advice-entry))))))))

(defn advice-remove!
  "Remove ADVICE-FN from FUNCTION-KEY.

  Args:
  - function-key: Keyword identifying the function
  - type: Advice type (optional, removes from all types if nil)
  - advice-fn: Function to remove (optional, removes all if nil)"
  [function-key & [type advice-fn]]
  (rf/dispatch [:advice/remove function-key type advice-fn]))

(rf/reg-event-db
  :advice/remove
  (fn [db [_ function-key type advice-fn]]
    (if type
      ;; Remove from specific type
      (update-in db [:advice function-key type]
                 (fn [advice-list]
                   (if advice-fn
                     ;; Remove specific function
                     (vec (remove #(= (:function %) advice-fn) (or advice-list [])))
                     ;; Remove all advice of this type
                     [])))
      ;; Remove all advice for this function
      (update-in db [:advice] dissoc function-key))))

(defn get-advice
  "Get all advice for FUNCTION-KEY of TYPE.

  Returns: Vector of advice entries sorted by depth"
  [db function-key type]
  (get-in db [:advice function-key type] []))

;; -- Advice Application --

(defn apply-before-advice
  "Apply all :before advice to ARGS.

  Each advice receives args and returns modified args.
  Advice are chained (output of one becomes input to next)."
  [before-advice args]
  (reduce (fn [current-args advice]
           (try
             (apply (:function advice) current-args)
             (catch js/Error e
               (log/warn (str "Before advice error: " e))
               current-args)))
         args
         before-advice))

(defn apply-after-advice
  "Apply all :after advice to RESULT.

  Each advice receives result and returns modified result."
  [after-advice result]
  (reduce (fn [current-result advice]
           (try
             ((:function advice) current-result)
             (catch js/Error e
               (log/warn (str "After advice error: " e))
               current-result)))
         result
         after-advice))

(defn apply-around-advice
  "Apply all :around advice, wrapping ORIGINAL-FN.

  Each :around advice receives (original-fn & args) and must call original-fn.
  Around advice are nested (outermost runs first)."
  [around-advice original-fn]
  (reduce (fn [wrapped-fn advice]
           (fn [& args]
             (try
               (apply (:function advice) wrapped-fn args)
               (catch js/Error e
                 (log/warn (str "Around advice error: " e))
                 (apply wrapped-fn args)))))
         original-fn
         (reverse around-advice)))  ; Reverse so outer advice wraps inner

(defn make-advised-function
  "Create an advised version of ORIGINAL-FN with all ADVICE.

  Args:
  - original-fn: Original function to wrap
  - advice-map: Map of {:before [...] :after [...] :around [...] :override [...]}

  Returns: Wrapped function that applies all advice"
  [original-fn advice-map]
  (let [before-advice (or (:before advice-map) [])
        after-advice (or (:after advice-map) [])
        around-advice (or (:around advice-map) [])
        override-advice (first (:override advice-map))]  ; Only one override
    (fn [& args]
      ;; 1. Apply :before advice (can modify args)
      (let [modified-args (apply-before-advice before-advice args)

            ;; 2. Determine which function to call
            base-fn (if override-advice
                     (:function override-advice)
                     original-fn)

            ;; 3. Apply :around advice (wraps base function)
            wrapped-fn (if (seq around-advice)
                        (apply-around-advice around-advice base-fn)
                        base-fn)

            ;; 4. Call the wrapped function
            result (apply wrapped-fn modified-args)

            ;; 5. Apply :after advice (can modify result)
            final-result (apply-after-advice after-advice result)]

        final-result))))

;; -- Advised Function Registry --

;; Store original functions before advising
(def original-functions (atom {}))

(defn advise-function!
  "Replace FUNCTION-KEY's implementation with advised version.

  This is called automatically when advice is added.
  Original function is stored and wrapped with advice."
  [function-key original-fn]
  (swap! original-functions assoc function-key original-fn))

(defn get-original-function
  "Get original (unadvised) function for FUNCTION-KEY."
  [function-key]
  (get @original-functions function-key))

(defn call-with-advice
  "Call ORIGINAL-FN with advice from DB for FUNCTION-KEY.

  Args:
  - db: Application database
  - function-key: Keyword identifying the function
  - original-fn: Original function
  - args: Arguments to pass

  Returns: Result of advised function call"
  [db function-key original-fn & args]
  (let [advice-map {:before (get-advice db function-key :before)
                   :after (get-advice db function-key :after)
                   :around (get-advice db function-key :around)
                   :override (get-advice db function-key :override)}
        advised-fn (make-advised-function original-fn advice-map)]
    (apply advised-fn args)))

;; -- Subscriptions --

(rf/reg-sub
  :advice/get
  (fn [db [_ function-key type]]
    (get-advice db function-key type)))

(rf/reg-sub
  :advice/all
  (fn [db [_ function-key]]
    (get-in db [:advice function-key] {})))

(rf/reg-sub
  :advice/has-advice?
  (fn [db [_ function-key]]
    (seq (get-in db [:advice function-key]))))

;; -- Initialization --

(defn initialize-advice!
  "Initialize advice system."
  []
  (rf/dispatch-sync [:advice/initialize]))

(rf/reg-event-db
  :advice/initialize
  (fn [db [_]]
    (assoc db :advice {})))

;; Auto-initialize on namespace load
(initialize-advice!)

;; -- Helper Utilities --

(defn advice-member-p
  "Check if ADVICE-FN is advised on FUNCTION-KEY with TYPE."
  [db function-key advice-fn & [type]]
  (let [types (if type [type] [:before :after :around :override])]
    (some (fn [t]
           (some #(= (:function %) advice-fn)
                (get-advice db function-key t)))
         types)))

(defn advice-mapc
  "Call FUNCTION for each advice on FUNCTION-KEY.

  FUNCTION receives advice entry map."
  [function function-key]
  (let [db @re-frame.db/app-db
        all-advice (get-in db [:advice function-key] {})]
    (doseq [[type advice-list] all-advice
            advice advice-list]
      (function advice))))

;; -- Example Advice Functions --

;; Example: Log function calls
(defn advice-log-call [original-fn & args]
  (log/info (str "Calling function with args: " (pr-str args)))
  (let [result (apply original-fn args)]
    (log/info (str "Function returned: " (pr-str result)))
    result))

;; Example: Validate arguments
(defn advice-validate-non-nil [& args]
  (when (some nil? args)
    (log/warn "Function called with nil argument"))
  args)

;; Example: Cache results
(def advice-cache (atom {}))

(defn advice-memoize [original-fn & args]
  (let [cache-key [original-fn args]]
    (if-let [cached (get @advice-cache cache-key)]
      (do
        (log/info "Returning cached result")
        cached)
      (let [result (apply original-fn args)]
        (swap! advice-cache assoc cache-key result)
        result))))
