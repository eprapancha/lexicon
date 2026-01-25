(ns lexicon.core.packages.savehist
  "Minibuffer history persistence (savehist-mode).

  Saves minibuffer input history across sessions:
  - Per-command history (M-x, find-file, switch-to-buffer, etc.)
  - Persist to localStorage
  - Configurable history limits
  - Auto-save on input

  Histories tracked:
  - command-history (M-x commands)
  - file-history (C-x C-f files)
  - buffer-history (C-x b buffers)
  - search-history (C-s search patterns)
  - Custom histories per command"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; -- Configuration --

(def default-history-max
  "Maximum entries per history."
  100)

(def savehist-storage-key "lexicon-savehist")

;; Histories to save
(def savehist-variables
  "Set of history variable names to persist."
  #{:command-history
    :file-history
    :buffer-history
    :search-history
    :query-replace-history
    :minibuffer-history})

;; -- History Management --

(defn add-to-history
  "Add ENTRY to HISTORY, maintaining max size and removing duplicates.

  Args:
  - history: Vector of history entries
  - entry: String to add
  - max-size: Maximum history size

  Returns: Updated history vector"
  [history entry max-size]
  (when-not (str/blank? entry)
    (let [;; Remove existing occurrence
          cleaned (remove #(= % entry) history)
          ;; Add to front
          updated (cons entry cleaned)
          ;; Limit size
          limited (take max-size updated)]
      (vec limited))))

(defn get-history
  "Get history for HISTORY-NAME.

  Args:
  - db: App database
  - history-name: Keyword (:command-history, :file-history, etc.)

  Returns: Vector of history entries"
  [db history-name]
  (get-in db [:savehist :histories history-name] []))

;; -- Persistence (localStorage) --

(defn save-histories!
  "Save all histories to localStorage."
  [histories]
  (try
    (let [;; Convert to plain data structure
          data (into {} (for [[k v] histories]
                         [(name k) v]))
          json-str (js/JSON.stringify (clj->js data))]
      (.setItem js/localStorage savehist-storage-key json-str))
    (catch js/Error e
      (js/console.warn "Failed to save histories:" e))))

(defn load-histories
  "Load histories from localStorage.

  Returns: Map of history-name -> [entries...]"
  []
  (try
    (when-let [json-str (.getItem js/localStorage savehist-storage-key)]
      (let [data (js->clj (js/JSON.parse json-str))
            ;; Convert string keys back to keywords
            histories (into {} (for [[k v] data]
                                [(keyword k) (vec v)]))]
        histories))
    (catch js/Error e
      (js/console.warn "Failed to load histories:" e)
      {})))

;; -- Re-frame Events & Subscriptions --

;; Initialize savehist
(rf/reg-event-db
  :savehist/initialize
  (fn [db [_]]
    (let [saved-histories (load-histories)
          max-size (or (get-in db [:savehist :max-size]) default-history-max)]
      (-> db
          (assoc-in [:savehist :enabled?] true)
          (assoc-in [:savehist :max-size] max-size)
          (assoc-in [:savehist :histories] saved-histories)))))

;; Add entry to history
(rf/reg-event-fx
  :savehist/add
  (fn [{:keys [db]} [_ history-name entry]]
    (when (savehist-variables history-name)
      (let [current-history (get-history db history-name)
            max-size (get-in db [:savehist :max-size] default-history-max)
            updated-history (add-to-history current-history entry max-size)
            all-histories (assoc (get-in db [:savehist :histories] {})
                                history-name
                                updated-history)]
        ;; Save to localStorage
        (save-histories! all-histories)
        {:db (assoc-in db [:savehist :histories history-name] updated-history)}))))

;; Clear specific history
(rf/reg-event-fx
  :savehist/clear-history
  (fn [{:keys [db]} [_ history-name]]
    (let [all-histories (dissoc (get-in db [:savehist :histories] {})
                                history-name)]
      ;; Save to localStorage
      (save-histories! all-histories)
      {:db (-> db
              (update-in [:savehist :histories] dissoc history-name)
              (assoc :message (str "Cleared " (name history-name))))})))

;; Clear all histories
(rf/reg-event-fx
  :savehist/clear-all
  (fn [{:keys [db]} [_]]
    (save-histories! {})
    {:db (-> db
            (assoc-in [:savehist :histories] {})
            (assoc :message "All histories cleared"))}))

;; Get specific history
(rf/reg-sub
  :savehist/history
  (fn [db [_ history-name]]
    (get-history db history-name)))

;; Check if savehist is enabled
(rf/reg-sub
  :savehist/enabled?
  (fn [db _]
    (get-in db [:savehist :enabled?] false)))

;; Get all histories
(rf/reg-sub
  :savehist/all-histories
  (fn [db _]
    (get-in db [:savehist :histories] {})))

;; -- Integration with Minibuffer --

;; When minibuffer completes, add to appropriate history
(defn add-minibuffer-input-to-history
  "Add minibuffer input to appropriate history based on command.

  Args:
  - command: Command being executed (:execute-extended-command, :find-file, etc.)
  - input: User input string"
  [command input]
  (let [history-name (case command
                      :execute-extended-command :command-history
                      :find-file :file-history
                      :switch-to-buffer :buffer-history
                      :isearch-forward :search-history
                      :query-replace :query-replace-history
                      :minibuffer-history)]  ; Default
    (rf/dispatch [:savehist/add history-name input])))

;; Hook into minibuffer completion
(defn hook-minibuffer-complete! []
  ;; In real implementation, would add interceptor to minibuffer events
  ;; For now, events.cljs should manually call add-minibuffer-input-to-history
  nil)

;; -- Commands --

;; View history for current minibuffer
(rf/reg-event-fx
  :savehist/view-history
  (fn [{:keys [db]} [_ history-name]]
    (let [history (get-history db history-name)
          content (str/join "\n" history)]
      {:fx [[:dispatch [:create-buffer (str "*History: " (name history-name) "*")
                       :content content
                       :major-mode :special-mode
                       :read-only true]]]})))

;; Toggle savehist-mode
(rf/reg-event-db
  :savehist-mode
  (fn [db [_ enable?]]
    (assoc-in db [:savehist :enabled?] enable?)))

;; -- Auto-save --

;; Auto-save histories periodically (every 5 minutes)
(defn start-autosave! []
  (js/setInterval
   (fn []
     (let [db @re-frame.db/app-db
           enabled? (get-in db [:savehist :enabled?] false)]
       (when enabled?
         (let [histories (get-in db [:savehist :histories] {})]
           (save-histories! histories)))))
   (* 5 60 1000)))  ; 5 minutes

;; -- Initialization --

(defn initialize-savehist! []
  ;; Initialize savehist system
  (rf/dispatch-sync [:savehist/initialize])

  ;; Hook into minibuffer
  (hook-minibuffer-complete!)

  ;; Start auto-save
  (start-autosave!))

;; Auto-initialize on namespace load
(initialize-savehist!)
