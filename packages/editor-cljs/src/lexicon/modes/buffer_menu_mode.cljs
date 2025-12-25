(ns lexicon.modes.buffer-menu-mode
  "Buffer Menu Mode - Interactive buffer list display.

  Buffer-menu-mode displays a list of all buffers with their properties.
  It's a special-mode derivative for managing buffers interactively.

  Key bindings (inherited from special-mode):
  - q: Quit buffer menu
  - g: Refresh buffer list
  - RET: Switch to buffer at point
  - d: Mark buffer for deletion
  - x: Execute marked deletions
  - s: Save buffer
  - u: Unmark buffer"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.ui.mode-line :as mode-line]))

;; -- Buffer Menu Keymap --

(def buffer-menu-map
  "Additional keybindings for buffer-menu-mode (merged with special-mode-map)."
  {:RET [:buffer-menu/select-buffer]
   :d   [:buffer-menu/mark-for-deletion]
   :x   [:buffer-menu/execute]
   :s   [:buffer-menu/save-buffer]
   :u   [:buffer-menu/unmark]})

;; -- Buffer List Generation --

(defn- pad-string
  "Pad a string to a given width with spaces."
  [s width]
  (let [s (str s)
        len (count s)]
    (if (>= len width)
      (subs s 0 width)
      (str s (apply str (repeat (- width len) " "))))))

(defn- format-buffer-line
  "Format a single buffer as a line in the buffer menu.
  Format: MR  Name                Size  Mode        File"
  [buffer]
  (let [modified-flag (if (:modified? buffer) "*" " ")
        readonly-flag (if (:is-read-only? buffer) "%" " ")
        name (pad-string (:name buffer "") 20)
        size (str (or (:size buffer) 0))
        mode (pad-string (name (:major-mode buffer :fundamental-mode)) 15)
        file (or (:file-name buffer) "")]
    (str modified-flag readonly-flag "  " name "  " size "  " mode "  " file)))

(defn- generate-buffer-list-content
  "Generate the full buffer list content string."
  [buffers]
  (let [header "MR  Name                  Size  Mode            File\n"
        separator "--- ----                  ----  ----            ----\n"
        buffer-lines (map format-buffer-line (vals buffers))]
    (str header separator (str/join "\n" buffer-lines))))

;; -- Event Handlers --

;; Show buffer menu
(rf/reg-event-fx
  :buffer-menu/list-buffers
  (fn [{:keys [db]} _]
    (let [buffers (:buffers db)
          content (generate-buffer-list-content buffers)
          buffer-name "*Buffer List*"

          ;; Find or create buffer
          existing-buffer-id (some (fn [[id buf]]
                                    (when (= (:name buf) buffer-name) id))
                                  buffers)
          buffer-id (or existing-buffer-id
                       (inc (apply max 0 (keys buffers))))

          ;; Create WASM instance
          wasm-constructor (get-in db [:wasm :constructor])
          wasm-instance (when wasm-constructor (new wasm-constructor content))]

      (println "📋 Creating buffer menu")

      {:db (-> db
               ;; Create/update buffer
               (assoc-in [:buffers buffer-id]
                        {:id buffer-id
                         :name buffer-name
                         :wasm-instance wasm-instance
                         :major-mode :buffer-menu-mode
                         :is-read-only? true
                         :modified? false
                         :mode-line-format mode-line/special-mode-line-format
                         :mode-data {:mode-line-name "Buffer Menu"
                                    :revert-fn [:buffer-menu/revert buffer-id]}
                         :text-properties {}
                         :overlays {}
                         :next-overlay-id 1}))
       :fx [[:dispatch [:echo/message "Buffer list created"]]]})))

;; Revert/refresh buffer list
(rf/reg-event-fx
  :buffer-menu/revert
  (fn [{:keys [db]} [_ buffer-id]]
    (println "🔄 Refreshing buffer list")
    (let [buffers (:buffers db)
          content (generate-buffer-list-content buffers)
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])]

      ;; Update buffer content
      (when wasm-instance
        (.setText ^js wasm-instance content))

      {:fx [[:dispatch [:echo/message "Buffer list refreshed"]]]})))

;; Select buffer at point (placeholder)
(rf/reg-event-fx
  :buffer-menu/select-buffer
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "Select buffer (not yet implemented)"]]]}))

;; Mark buffer for deletion (placeholder)
(rf/reg-event-fx
  :buffer-menu/mark-for-deletion
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "Mark for deletion (not yet implemented)"]]]}))

;; Execute marked deletions (placeholder)
(rf/reg-event-fx
  :buffer-menu/execute
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "Execute (not yet implemented)"]]]}))

;; Save buffer (placeholder)
(rf/reg-event-fx
  :buffer-menu/save-buffer
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "Save buffer (not yet implemented)"]]]}))

;; Unmark buffer (placeholder)
(rf/reg-event-fx
  :buffer-menu/unmark
  (fn [_ _]
    {:fx [[:dispatch [:echo/message "Unmark (not yet implemented)"]]]}))
