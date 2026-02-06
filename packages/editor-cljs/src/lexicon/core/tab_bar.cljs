(ns lexicon.core.tab-bar
  "Tab bar mode - frame-local tabs with named window configurations.

  Each tab stores a complete window configuration. Switching tabs
  restores the window layout.

  Commands:
  - tab-bar-new-tab: Create new tab (C-x t 2)
  - tab-close: Close current tab (C-x t 0)
  - tab-next: Switch to next tab (C-x t o or C-TAB)
  - tab-previous: Switch to previous tab (C-S-TAB)
  - tab-bar-mode: Toggle tab bar display

  Based on Emacs lisp/tab-bar.el"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

;; =============================================================================
;; State
;; =============================================================================

;; Whether tab-bar-mode is enabled
(defonce tab-bar-mode-enabled? (atom false))

;; List of tabs: [{:name "tab1" :window-tree {...} :active-window-id id} ...]
(defonce tabs (atom []))

;; Current tab index
(defonce current-tab-index (atom 0))

;; Tab counter for naming
(defonce tab-counter (atom 0))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn generate-tab-name
  "Generate a unique tab name."
  []
  (swap! tab-counter inc)
  (str "Tab " @tab-counter))

(defn current-tab
  "Get the current tab."
  []
  (get @tabs @current-tab-index))

(defn save-current-tab!
  "Save the current window configuration to the current tab."
  [db]
  (when (seq @tabs)
    (swap! tabs assoc @current-tab-index
           {:name (:name (current-tab))
            :window-tree (:window-tree db)
            :active-window-id (:active-window-id db)})))

(defn restore-tab
  "Restore a tab's window configuration."
  [db tab]
  (-> db
      (assoc :window-tree (:window-tree tab))
      (assoc :active-window-id (:active-window-id tab))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-fx
 :tab-bar/new-tab
 (fn [{:keys [db]} [_]]
   "Create a new tab with current window configuration."
   (let [new-tab {:name (generate-tab-name)
                  :window-tree (:window-tree db)
                  :active-window-id (:active-window-id db)}]
     ;; Save current tab first
     (save-current-tab! db)
     ;; Add new tab after current
     (swap! tabs #(vec (concat (take (inc @current-tab-index) %)
                               [new-tab]
                               (drop (inc @current-tab-index) %))))
     ;; Switch to new tab
     (swap! current-tab-index inc)
     {:db db
      :fx [[:dispatch [:echo/message (str "Created tab: " (:name new-tab))]]]})))

(rf/reg-event-fx
 :tab-bar/close-tab
 (fn [{:keys [db]} [_]]
   "Close the current tab."
   (if (<= (count @tabs) 1)
     {:db db
      :fx [[:dispatch [:echo/message "Cannot close the only tab"]]]}
     (let [closed-name (:name (current-tab))]
       ;; Remove current tab
       (swap! tabs #(vec (concat (take @current-tab-index %)
                                 (drop (inc @current-tab-index) %))))
       ;; Adjust index if needed
       (when (>= @current-tab-index (count @tabs))
         (swap! current-tab-index dec))
       ;; Restore the now-current tab
       (let [tab (current-tab)]
         {:db (restore-tab db tab)
          :fx [[:dispatch [:echo/message (str "Closed tab: " closed-name)]]]})))))

(rf/reg-event-fx
 :tab-bar/next-tab
 (fn [{:keys [db]} [_]]
   "Switch to the next tab."
   (if (empty? @tabs)
     {:db db
      :fx [[:dispatch [:echo/message "No tabs"]]]}
     (do
       ;; Save current tab
       (save-current-tab! db)
       ;; Move to next (wrap around)
       (swap! current-tab-index #(mod (inc %) (count @tabs)))
       (let [tab (current-tab)]
         {:db (restore-tab db tab)
          :fx [[:dispatch [:echo/message (str "Switched to: " (:name tab))]]]})))))

(rf/reg-event-fx
 :tab-bar/previous-tab
 (fn [{:keys [db]} [_]]
   "Switch to the previous tab."
   (if (empty? @tabs)
     {:db db
      :fx [[:dispatch [:echo/message "No tabs"]]]}
     (do
       ;; Save current tab
       (save-current-tab! db)
       ;; Move to previous (wrap around)
       (swap! current-tab-index #(mod (dec %) (count @tabs)))
       (let [tab (current-tab)]
         {:db (restore-tab db tab)
          :fx [[:dispatch [:echo/message (str "Switched to: " (:name tab))]]]})))))

(rf/reg-event-fx
 :tab-bar/switch-to-tab
 (fn [{:keys [db]} [_ index]]
   "Switch to a specific tab by index (0-based)."
   (if (or (empty? @tabs) (< index 0) (>= index (count @tabs)))
     {:db db
      :fx [[:dispatch [:echo/message "Invalid tab index"]]]}
     (do
       ;; Save current tab
       (save-current-tab! db)
       ;; Switch to specified tab
       (reset! current-tab-index index)
       (let [tab (current-tab)]
         {:db (restore-tab db tab)
          :fx [[:dispatch [:echo/message (str "Switched to: " (:name tab))]]]})))))

(rf/reg-event-fx
 :tab-bar/list-tabs
 (fn [{:keys [db]} [_]]
   "List all tabs."
   (if (empty? @tabs)
     {:db db
      :fx [[:dispatch [:echo/message "No tabs"]]]}
     (let [tab-list (->> @tabs
                         (map-indexed (fn [i tab]
                                        (str (if (= i @current-tab-index) "* " "  ")
                                             (:name tab))))
                         (str/join "\n"))]
       {:db db
        :fx [[:dispatch [:echo/message (str "Tabs:\n" tab-list)]]]}))))

(rf/reg-event-fx
 :tab-bar-mode
 (fn [{:keys [db]} [_ enable?]]
   "Toggle or set tab-bar-mode."
   (let [new-state (if (nil? enable?)
                     (not @tab-bar-mode-enabled?)
                     (boolean enable?))]
     (reset! tab-bar-mode-enabled? new-state)
     (if new-state
       ;; Initialize with current config as first tab
       (do
         (reset! tabs [{:name (generate-tab-name)
                        :window-tree (:window-tree db)
                        :active-window-id (:active-window-id db)}])
         (reset! current-tab-index 0)
         {:db (assoc db :tab-bar-mode true)
          :fx [[:dispatch [:echo/message "Tab bar mode enabled"]]]})
       (do
         (reset! tabs [])
         (reset! current-tab-index 0)
         {:db (assoc db :tab-bar-mode false)
          :fx [[:dispatch [:echo/message "Tab bar mode disabled"]]]})))))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :tab-bar/tabs
 (fn [_db _]
   @tabs))

(rf/reg-sub
 :tab-bar/current-index
 (fn [_db _]
   @current-tab-index))

(rf/reg-sub
 :tab-bar/enabled?
 (fn [db _]
   (:tab-bar-mode db)))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init!
  "Initialize tab-bar-mode commands and keybindings."
  []
  ;; Register commands
  (rf/dispatch [:register-command :tab-bar-mode
                {:docstring "Toggle tab bar mode"
                 :handler [:tab-bar-mode nil]}])

  (rf/dispatch [:register-command :tab-bar-new-tab
                {:docstring "Create a new tab (C-x t 2)"
                 :handler [:tab-bar/new-tab]}])

  (rf/dispatch [:register-command :tab-close
                {:docstring "Close the current tab (C-x t 0)"
                 :handler [:tab-bar/close-tab]}])

  (rf/dispatch [:register-command :tab-next
                {:docstring "Switch to next tab (C-x t o)"
                 :handler [:tab-bar/next-tab]}])

  (rf/dispatch [:register-command :tab-previous
                {:docstring "Switch to previous tab"
                 :handler [:tab-bar/previous-tab]}])

  (rf/dispatch [:register-command :tab-list
                {:docstring "List all tabs"
                 :handler [:tab-bar/list-tabs]}])

  ;; Set up key bindings (C-x t prefix)
  ;; Note: We'd need a proper prefix keymap for C-x t, for now use M-x
  )
