(ns lexicon.packages.tab-bar
  "Tab bar mode - frame-local tabs with named window configurations.

  Each tab stores a complete window configuration. Switching tabs
  restores the window layout.

  Commands:
  - tab-bar-new-tab: Create new tab (C-x t 2)
  - tab-close: Close current tab (C-x t 0)
  - tab-next: Switch to next tab (C-x t o or C-TAB)
  - tab-previous: Switch to previous tab (C-S-TAB)
  - tab-bar-mode: Toggle tab bar display

  Based on Emacs lisp/tab-bar.el

  This package uses only lisp.cljs primitives."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [lexicon.lisp :as lisp]))

;; =============================================================================
;; State
;; =============================================================================

;; Whether tab-bar-mode is enabled
(defonce tab-bar-mode-enabled? (r/atom false))

;; List of tabs: [{:name "tab1" :window-tree {...} :active-window-id id} ...]
(defonce tabs (r/atom []))

;; Current tab index
(defonce current-tab-index (r/atom 0))

;; Tab counter for naming
(defonce tab-counter (r/atom 0))

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
  []
  (when (seq @tabs)
    (let [config (lisp/current-window-configuration)]
      (swap! tabs assoc @current-tab-index
             (merge (current-tab) config)))))

(defn restore-tab!
  "Restore a tab's window configuration."
  [tab]
  (lisp/set-window-configuration tab))

;; =============================================================================
;; Tab Commands
;; =============================================================================

(defn tab-bar-new-tab!
  "Create a new tab with current window configuration."
  []
  (let [config (lisp/current-window-configuration)
        new-tab (assoc config :name (generate-tab-name))]
    ;; Save current tab first
    (save-current-tab!)
    ;; Add new tab after current
    (swap! tabs #(vec (concat (take (inc @current-tab-index) %)
                              [new-tab]
                              (drop (inc @current-tab-index) %))))
    ;; Switch to new tab
    (swap! current-tab-index inc)
    (lisp/message (str "Created tab: " (:name new-tab)))))

(defn tab-close!
  "Close the current tab."
  []
  (if (<= (count @tabs) 1)
    (lisp/message "Cannot close the only tab")
    (let [closed-name (:name (current-tab))]
      ;; Remove current tab
      (swap! tabs #(vec (concat (take @current-tab-index %)
                                (drop (inc @current-tab-index) %))))
      ;; Adjust index if needed
      (when (>= @current-tab-index (count @tabs))
        (swap! current-tab-index dec))
      ;; Restore the now-current tab
      (let [tab (current-tab)]
        (restore-tab! tab)
        (lisp/message (str "Closed tab: " closed-name))))))

(defn tab-next!
  "Switch to the next tab."
  []
  (if (empty? @tabs)
    (lisp/message "No tabs")
    (do
      ;; Save current tab
      (save-current-tab!)
      ;; Move to next (wrap around)
      (swap! current-tab-index #(mod (inc %) (count @tabs)))
      (let [tab (current-tab)]
        (restore-tab! tab)
        (lisp/message (str "Switched to: " (:name tab)))))))

(defn tab-previous!
  "Switch to the previous tab."
  []
  (if (empty? @tabs)
    (lisp/message "No tabs")
    (do
      ;; Save current tab
      (save-current-tab!)
      ;; Move to previous (wrap around)
      (swap! current-tab-index #(mod (dec %) (count @tabs)))
      (let [tab (current-tab)]
        (restore-tab! tab)
        (lisp/message (str "Switched to: " (:name tab)))))))

(defn tab-switch-to!
  "Switch to a specific tab by index (0-based)."
  [index]
  (if (or (empty? @tabs) (< index 0) (>= index (count @tabs)))
    (lisp/message "Invalid tab index")
    (do
      ;; Save current tab
      (save-current-tab!)
      ;; Switch to specified tab
      (reset! current-tab-index index)
      (let [tab (current-tab)]
        (restore-tab! tab)
        (lisp/message (str "Switched to: " (:name tab)))))))

(defn tab-list!
  "List all tabs."
  []
  (if (empty? @tabs)
    (lisp/message "No tabs")
    (let [tab-list (->> @tabs
                        (map-indexed (fn [i tab]
                                       (str (if (= i @current-tab-index) "* " "  ")
                                            (:name tab))))
                        (str/join "\n"))]
      (lisp/message (str "Tabs:\n" tab-list)))))

(defn tab-bar-mode!
  "Toggle or set tab-bar-mode."
  []
  (let [new-state (not @tab-bar-mode-enabled?)]
    (reset! tab-bar-mode-enabled? new-state)
    (if new-state
      ;; Initialize with current config as first tab
      (let [config (lisp/current-window-configuration)]
        (reset! tabs [(assoc config :name (generate-tab-name))])
        (reset! current-tab-index 0)
        (lisp/message "Tab bar mode enabled"))
      (do
        (reset! tabs [])
        (reset! current-tab-index 0)
        (lisp/message "Tab bar mode disabled")))))

;; =============================================================================
;; Visual Component
;; =============================================================================

(defn tab-bar-tab
  "Render a single tab in the tab bar strip."
  [index tab]
  (let [active? (= index @current-tab-index)
        multiple-tabs? (> (count @tabs) 1)]
    [:div.tab-bar-tab
     {:style    {:display          "inline-flex"
                 :align-items      "center"
                 :padding          "0 12px"
                 :height           "100%"
                 :cursor           "pointer"
                 :font-family      "monospace"
                 :font-size        "12px"
                 :white-space      "nowrap"
                 :border-bottom    (if active?
                                     "2px solid var(--lexicon-fg-default, #d4d4d4)"
                                     "2px solid transparent")
                 :background-color (if active?
                                     "var(--lexicon-bg-mode-line, #2d2d2d)"
                                     "transparent")
                 :color            (if active?
                                     "var(--lexicon-fg-default, #d4d4d4)"
                                     "var(--lexicon-fg-shadow, #888)")}
      :on-click (fn [_e]
                  (tab-switch-to! index))}
     [:span (:name tab)]
     (when multiple-tabs?
       [:button
        {:style    {:margin-left      "6px"
                    :background       "none"
                    :border           "none"
                    :color            "inherit"
                    :cursor           "pointer"
                    :font-size        "12px"
                    :padding          "0 2px"
                    :line-height      "1"
                    :opacity          "0.6"}
         :on-click (fn [e]
                     (.stopPropagation e)
                     (tab-close!))}
        "\u00d7"])]))

(defn tab-bar-strip
  "Render the tab bar strip at the top of the editor."
  []
  [:div.tab-bar-strip
   {:style {:display          "flex"
            :flex-direction   "row"
            :align-items      "stretch"
            :height           "28px"
            :background-color "var(--lexicon-bg-dim, #1a1a1a)"
            :border-bottom    "1px solid var(--lexicon-bg-mode-line, #2d2d2d)"
            :font-family      "monospace"
            :font-size        "12px"
            :user-select      "none"
            :flex-shrink      "0"}}
   (doall
     (map-indexed
       (fn [i tab]
         ^{:key (str "tab-" i)}
         [tab-bar-tab i tab])
       @tabs))
   [:button.tab-bar-new
    {:style    {:background  "none"
                :border      "none"
                :color       "var(--lexicon-fg-shadow, #888)"
                :cursor      "pointer"
                :font-size   "16px"
                :padding     "0 10px"
                :line-height "28px"}
     :on-click (fn [_e]
                 (tab-bar-new-tab!))}
    "+"]])

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-tab-bar!
  "Initialize tab-bar-mode commands and keybindings."
  []
  (lisp/define-command 'tab-bar-mode
    tab-bar-mode!
    "Toggle tab bar mode")

  (lisp/define-command 'tab-bar-new-tab
    tab-bar-new-tab!
    "Create a new tab (C-x t 2)")

  (lisp/define-command 'tab-close
    tab-close!
    "Close the current tab (C-x t 0)")

  (lisp/define-command 'tab-next
    tab-next!
    "Switch to next tab (C-x t o)")

  (lisp/define-command 'tab-previous
    tab-previous!
    "Switch to previous tab")

  (lisp/define-command 'tab-list
    tab-list!
    "List all tabs")

  ;; Set up key bindings (C-x t prefix)
  (lisp/global-set-key "C-x t 2" :tab-bar-new-tab)
  (lisp/global-set-key "C-x t 0" :tab-close)
  (lisp/global-set-key "C-x t o" :tab-next))
