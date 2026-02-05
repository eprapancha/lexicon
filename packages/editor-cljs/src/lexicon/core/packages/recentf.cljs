(ns lexicon.core.packages.recentf
  "Recent files tracking (recentf-mode).

  Maintains a list of recently opened files:
  - Auto-track files on open
  - Persist list across sessions (via localStorage)
  - Limit list size (default: 100)
  - Provide completion interface for reopening
  - Cleanup removed/non-existent files

  Commands:
  - recentf-open (C-x C-r): Open recent file
  - recentf-clear: Clear recent file list
  - recentf-edit-list: Edit list in buffer"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.completion.metadata :as metadata]))

;; -- Configuration --

(def default-recentf-max
  "Maximum number of recent files to remember."
  100)

(def recentf-exclude-patterns
  "Patterns for files to exclude from recent list."
  #{#"^\*.*\*$"           ; Special buffers (*scratch*, *Messages*, etc.)
    #"^COMMIT_EDITMSG$"   ; Git commit messages
    #"\.elc$"             ; Compiled Elisp
    #"\.tmp$"             ; Temporary files
    #"\.log$"})           ; Log files

(defn excluded-file?
  "Check if FILE-PATH should be excluded from recent list."
  [file-path]
  (some (fn [pattern]
         (re-find pattern file-path))
       recentf-exclude-patterns))

;; -- Recent File List Management --

(defn add-to-recentf
  "Add FILE-PATH to recent file list.
  Moves to front if already present, removes duplicates."
  [recent-files file-path max-files]
  (if (excluded-file? file-path)
    recent-files
    (let [;; Remove existing occurrence
          cleaned (remove #(= % file-path) recent-files)
          ;; Add to front
          updated (cons file-path cleaned)
          ;; Limit size
          limited (take max-files updated)]
      (vec limited))))

(defn remove-from-recentf
  "Remove FILE-PATH from recent file list."
  [recent-files file-path]
  (vec (remove #(= % file-path) recent-files)))

;; -- Persistence (localStorage) --

(def recentf-storage-key "lexicon-recentf-list")

(defn save-recentf-list!
  "Save recent file list to localStorage."
  [recent-files]
  (try
    (let [json-str (js/JSON.stringify (clj->js recent-files))]
      (.setItem js/localStorage recentf-storage-key json-str))
    (catch js/Error e
      (js/console.warn "Failed to save recent files:" e))))

(defn load-recentf-list
  "Load recent file list from localStorage.
  Returns empty vector if not found."
  []
  (try
    (when-let [json-str (.getItem js/localStorage recentf-storage-key)]
      (vec (js->clj (js/JSON.parse json-str))))
    (catch js/Error e
      (js/console.warn "Failed to load recent files:" e)
      [])))

;; -- Re-frame Events & Subscriptions --

;; Initialize recentf
(rf/reg-event-db
  :recentf/initialize
  (fn [db [_]]
    (let [saved-list (load-recentf-list)
          max-files (or (get-in db [:recentf :max-files]) default-recentf-max)]
      (-> db
          (assoc-in [:recentf :enabled?] true)
          (assoc-in [:recentf :max-files] max-files)
          (assoc-in [:recentf :list] saved-list)))))

;; Add file to recent list
(rf/reg-event-fx
  :recentf/add-file
  (fn [{:keys [db]} [_ file-path]]
    (let [current-list (get-in db [:recentf :list] [])
          max-files (get-in db [:recentf :max-files] default-recentf-max)
          updated-list (add-to-recentf current-list file-path max-files)]
      ;; Save to localStorage
      (save-recentf-list! updated-list)
      {:db (assoc-in db [:recentf :list] updated-list)})))

;; Remove file from recent list
(rf/reg-event-fx
  :recentf/remove-file
  (fn [{:keys [db]} [_ file-path]]
    (let [current-list (get-in db [:recentf :list] [])
          updated-list (remove-from-recentf current-list file-path)]
      ;; Save to localStorage
      (save-recentf-list! updated-list)
      {:db (assoc-in db [:recentf :list] updated-list)})))

;; Clear all recent files
(rf/reg-event-fx
  :recentf/clear
  (fn [{:keys [db]} [_]]
    (save-recentf-list! [])
    {:db (-> db
            (assoc-in [:recentf :list] [])
            (assoc :message "Recent file list cleared"))}))

;; Get recent file list
(rf/reg-sub
  :recentf/list
  (fn [db _]
    (get-in db [:recentf :list] [])))

;; Check if recentf is enabled
(rf/reg-sub
  :recentf/enabled?
  (fn [db _]
    (get-in db [:recentf :enabled?] false)))

;; -- Commands --

;; recentf-open (C-x C-r in Emacs)
(rf/reg-event-fx
  :recentf-open
  (fn [{:keys [db]} [_]]
    (let [recent-files (get-in db [:recentf :list] [])
          metadata (metadata/make-metadata
                    :category :file
                    :annotation-function nil)]
      (if (seq recent-files)
        {:fx [[:dispatch [:minibuffer/activate
                          {:prompt "Recent file: "
                           :completions recent-files
                           :metadata metadata
                           :on-confirm [:recentf/open-file]}]]]}
        {:db (assoc db :message "No recent files")}))))

(rf/reg-event-fx
  :recentf/open-file
  (fn [{:keys [db]} [_ file-path]]
    {:fx [[:dispatch [:find-file file-path]]]}))

;; recentf-edit-list
(rf/reg-event-fx
  :recentf-edit-list
  (fn [{:keys [db]} [_]]
    (let [recent-files (get-in db [:recentf :list] [])
          content (str/join "\n" recent-files)]
      {:fx [[:dispatch [:create-buffer "*Recent Files*"
                       :content content
                       :major-mode :special-mode
                       :read-only true]]]})))


;; -- Initialization --

(defn init!
  "Initialize recentf module and register commands."
  []
  ;; Initialize state
  (rf/dispatch [:recentf/initialize])

  ;; Register commands
  (rf/dispatch [:register-command :recentf-open-files
                {:docstring "Open a recently visited file"
                 :interactive nil
                 :handler [:recentf-open]}])

  (rf/dispatch [:register-command :recentf-edit-list
                {:docstring "Edit the recent file list"
                 :interactive nil
                 :handler [:recentf-edit-list]}])

  (rf/dispatch [:register-command :recentf-mode
                {:docstring "Toggle recentf mode - track recently opened files"
                 :interactive nil
                 :handler [:command/recentf-mode]}]))

(rf/reg-event-fx
 :command/recentf-mode
 (fn [{:keys [db]} [_]]
   (let [currently-enabled? (get-in db [:recentf :enabled?] false)]
     {:db (assoc-in db [:recentf :enabled?] (not currently-enabled?))
      :fx [[:dispatch [:echo/message (if currently-enabled?
                                       "recentf-mode disabled"
                                       "recentf-mode enabled")]]]})))
