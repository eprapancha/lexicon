(ns lexicon.core.hooks
  "Hook system implementation for Lexicon.

  Provides extension points throughout the editor lifecycle, enabling packages
  to observe and react to events without modifying core code.

  Based on the Hook System Specification (docs/core/hooks.md)."
  (:require [re-frame.core :as rf]))

;; -- Helper Functions --

(defn create-hook-entry
  "Create a hook entry from function and options."
  [fn-or-entry & {:keys [id priority enabled? package-id doc]
                  :or {priority 50
                       enabled? true}}]
  (if (map? fn-or-entry)
    ;; Full entry map provided
    (merge {:priority 50 :enabled? true}
           fn-or-entry)
    ;; Function provided, create entry
    {:id (or id (keyword (str "hook-" (random-uuid))))
     :priority priority
     :fn fn-or-entry
     :enabled? enabled?
     :package-id package-id
     :doc doc
     :added-at (js/Date.)}))

(defn sort-by-priority
  "Sort hook entries by priority (lowest first), then by insertion order."
  [entries]
  (->> entries
       (sort-by (juxt :priority :added-at))))

(defn add-hook-entry
  "Add a hook entry to a hook vector, maintaining priority order."
  [hook-vec entry]
  (sort-by-priority (conj hook-vec entry)))

(defn remove-hook-entry
  "Remove a hook entry from a hook vector by ID or function."
  [hook-vec hook-fn-or-id]
  (if (keyword? hook-fn-or-id)
    ;; Remove by ID
    (vec (remove #(= (:id %) hook-fn-or-id) hook-vec))
    ;; Remove by function reference
    (vec (remove #(identical? (:fn %) hook-fn-or-id) hook-vec))))

(defn find-hook-entry
  "Find a hook entry by ID or function."
  [hook-vec hook-fn-or-id]
  (if (keyword? hook-fn-or-id)
    ;; Find by ID
    (first (filter #(= (:id %) hook-fn-or-id) hook-vec))
    ;; Find by function reference
    (first (filter #(identical? (:fn %) hook-fn-or-id) hook-vec))))

(defn run-hook-entry
  "Execute a single hook entry with error isolation.
  Returns nil on success, error on failure."
  [entry context]
  (try
    (when (:enabled? entry true)
      ((:fn entry) context))
    nil
    (catch :default e
      (js/console.error
       (str "❌ Hook failed: " (:id entry))
       (str "in context: " context)
       e)
      e)))

;; -- Event Handlers --

(rf/reg-event-db
 :hook/add
 (fn [db [_ hook-id fn-or-entry & options]]
   "Add a hook to the registry."
   (let [entry (apply create-hook-entry fn-or-entry options)
         hook-path [:hooks hook-id]
         current-hooks (get-in db hook-path [])]
     (assoc-in db hook-path (add-hook-entry current-hooks entry)))))

(rf/reg-event-db
 :hook/remove
 (fn [db [_ hook-id hook-fn-or-id]]
   "Remove a hook from the registry."
   (let [hook-path [:hooks hook-id]
         current-hooks (get-in db hook-path [])]
     (assoc-in db hook-path (remove-hook-entry current-hooks hook-fn-or-id)))))

(rf/reg-event-db
 :hook/enable
 (fn [db [_ hook-id entry-id]]
   "Enable a hook entry."
   (let [hook-path [:hooks hook-id]
         current-hooks (get-in db hook-path [])
         updated-hooks (mapv (fn [entry]
                               (if (= (:id entry) entry-id)
                                 (assoc entry :enabled? true)
                                 entry))
                             current-hooks)]
     (assoc-in db hook-path updated-hooks))))

(rf/reg-event-db
 :hook/disable
 (fn [db [_ hook-id entry-id]]
   "Disable a hook entry."
   (let [hook-path [:hooks hook-id]
         current-hooks (get-in db hook-path [])
         updated-hooks (mapv (fn [entry]
                               (if (= (:id entry) entry-id)
                                 (assoc entry :enabled? false)
                                 entry))
                             current-hooks)]
     (assoc-in db hook-path updated-hooks))))

(rf/reg-event-fx
 :hook/run
 (fn [{:keys [db]} [_ hook-id context]]
   "Run all hooks for a given hook ID with the provided context.
   Returns effects to dispatch any errors."
   (let [hook-entries (get-in db [:hooks hook-id] [])
         errors (atom [])]
     ;; Execute each hook entry
     (doseq [entry hook-entries]
       (when-let [err (run-hook-entry entry context)]
         (swap! errors conj {:hook-id hook-id
                             :entry-id (:id entry)
                             :error err
                             :context context})))
     ;; Return effects for errors if any
     (if (seq @errors)
       {:fx (mapv (fn [err]
                    [:dispatch [:echo/message
                                (str "Hook error: " (:entry-id err))]])
                  @errors)}
       {:db db}))))

(rf/reg-event-db
 :hook/clear
 (fn [db [_ hook-id]]
   "Clear all entries for a hook (or all hooks if hook-id is nil)."
   (if hook-id
     (assoc-in db [:hooks hook-id] [])
     (assoc db :hooks {}))))

;; -- Subscriptions --

(rf/reg-sub
 :hook/list
 (fn [db [_ hook-id]]
   "List all hook entries for a hook, or all hooks if hook-id is nil."
   (if hook-id
     (get-in db [:hooks hook-id] [])
     (get db :hooks {}))))

(rf/reg-sub
 :hook/entry
 (fn [db [_ hook-id entry-id]]
   "Get a specific hook entry."
   (let [hook-entries (get-in db [:hooks hook-id] [])]
     (find-hook-entry hook-entries entry-id))))

;; -- Public API Functions --

(defn add-hook
  "Add a hook to the registry.

  Parameters:
  - hook-id: Keyword identifying the hook (:after-command-hook, etc.)
  - fn-or-entry: Function or full entry map
  - options: Keyword args for entry metadata

  Options:
  - :id - Unique identifier (default: auto-generated)
  - :priority - Execution priority 0-100 (default: 50)
  - :enabled? - Enabled state (default: true)
  - :package-id - Owner package
  - :doc - Description

  Returns: Hook entry ID"
  [hook-id fn-or-entry & options]
  (let [entry (apply create-hook-entry fn-or-entry options)]
    (rf/dispatch [:hook/add hook-id fn-or-entry options])
    (:id entry)))

(defn remove-hook
  "Remove a hook from the registry.

  Parameters:
  - hook-id: Keyword identifying the hook
  - hook-fn-or-id: Function reference or entry ID

  Returns: true if removed, false if not found"
  [hook-id hook-fn-or-id]
  (rf/dispatch [:hook/remove hook-id hook-fn-or-id])
  true)

(defn enable-hook
  "Enable a previously disabled hook entry.

  Parameters:
  - hook-id: Keyword identifying the hook
  - entry-id: Entry ID to enable

  Returns: true if enabled, false if not found"
  [hook-id entry-id]
  (rf/dispatch [:hook/enable hook-id entry-id])
  true)

(defn disable-hook
  "Disable a hook entry without removing it.

  Parameters:
  - hook-id: Keyword identifying the hook
  - entry-id: Entry ID to disable

  Returns: true if disabled, false if not found"
  [hook-id entry-id]
  (rf/dispatch [:hook/disable hook-id entry-id])
  true)

(defn run-hooks
  "Run all hooks for a given hook ID with the provided context.

  Parameters:
  - hook-id: Keyword identifying the hook
  - context: Context map to pass to hooks

  Returns: nil (side effects only)"
  [hook-id context]
  (rf/dispatch [:hook/run hook-id context])
  nil)

(defn list-hooks
  "List all hook entries or entries for a specific hook.

  Parameters:
  - hook-id: (optional) Keyword identifying the hook

  Returns:
  - If hook-id provided: vector of hook entries
  - If no hook-id: map of all hooks"
  ([]
   @(rf/subscribe [:hook/list nil]))
  ([hook-id]
   @(rf/subscribe [:hook/list hook-id])))

(defn clear-hooks
  "Clear all hook entries (or all entries for a specific hook).

  Parameters:
  - hook-id: (optional) Keyword identifying the hook

  Returns: nil"
  ([]
   (rf/dispatch [:hook/clear nil])
   nil)
  ([hook-id]
   (rf/dispatch [:hook/clear hook-id])
   nil))
