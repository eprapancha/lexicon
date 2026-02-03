(ns lexicon.core.modes.buffer-menu-mode
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
            [lexicon.core.ui.mode-line :as mode-line]))

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
    (let [;; If no buffer-id provided, use the active buffer
          active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                              (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
          buffer-id (or buffer-id (:buffer-id active-window))
          buffers (:buffers db)
          content (generate-buffer-list-content buffers)
          ^js wasm-instance (get-in db [:buffers buffer-id :wasm-instance])]

      ;; Update buffer content
      (when wasm-instance
        (let [current-length (.length wasm-instance)]
          (.delete wasm-instance 0 current-length)
          (.insert wasm-instance 0 content)))

      {:fx [[:dispatch [:echo/message "Buffer list refreshed"]]]})))

;; NOTE: :buffer-menu/select-buffer is defined in events/buffer.cljs

;; -- State for marked buffers --
;; Store marks: {buffer-menu-buffer-id -> {line-number -> mark-char}}
(defonce buffer-menu-marks (atom {}))

(defn- get-buffer-name-at-line
  "Get the buffer name at a specific line in the buffer list.
   Lines 0-1 are header, buffers start at line 2."
  [db buffer-menu-id line-number]
  (when (>= line-number 2)
    (let [buffers (vals (:buffers db))
          ;; Filter out special buffers for display order
          sorted-buffers (sort-by :name buffers)
          buffer-index (- line-number 2)]
      (when (< buffer-index (count sorted-buffers))
        (:name (nth sorted-buffers buffer-index))))))

(defn- get-current-line-in-buffer-menu
  "Get the current line number in the buffer menu."
  [db]
  (let [cursor-pos (get-in db [:ui :cursor-position] 0)
        active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                           (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
        buffer-id (:buffer-id active-window)
        wasm (get-in db [:buffers buffer-id :wasm-instance])
        text (when wasm (.getText wasm))]
    (when text
      (count (filter #(= % \newline) (take cursor-pos text))))))

;; Mark buffer for deletion (Issue #115)
(rf/reg-event-fx
  :buffer-menu/mark-for-deletion
  (fn [{:keys [db]} _]
    (let [active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                              (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
          buffer-menu-id (:buffer-id active-window)
          line-number (get-current-line-in-buffer-menu db)
          buffer-name (get-buffer-name-at-line db buffer-menu-id line-number)]
      (if buffer-name
        (do
          (swap! buffer-menu-marks assoc-in [buffer-menu-id line-number] "D")
          {:fx [[:dispatch [:echo/message (str "Marked for deletion: " buffer-name)]]
                [:dispatch [:next-line]]]})
        {:fx [[:dispatch [:echo/message "No buffer on this line"]]]}))))

;; Execute marked deletions (Issue #115)
(rf/reg-event-fx
  :buffer-menu/execute
  (fn [{:keys [db]} _]
    (let [active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                              (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
          buffer-menu-id (:buffer-id active-window)
          marks (get @buffer-menu-marks buffer-menu-id {})
          delete-marks (filter (fn [[_ mark]] (= mark "D")) marks)
          buffer-names (keep (fn [[line _]]
                               (get-buffer-name-at-line db buffer-menu-id line))
                             delete-marks)]
      (if (seq buffer-names)
        (do
          ;; Kill each marked buffer
          (doseq [name buffer-names]
            (rf/dispatch-sync [:kill-buffer-by-name name]))
          ;; Clear marks for this buffer menu
          (swap! buffer-menu-marks dissoc buffer-menu-id)
          ;; Refresh the buffer list
          {:fx [[:dispatch [:buffer-menu/revert buffer-menu-id]]
                [:dispatch [:echo/message (str "Deleted " (count buffer-names) " buffer(s)")]]]})
        {:fx [[:dispatch [:echo/message "No buffers marked for deletion"]]]}))))

;; Save buffer at point (Issue #115)
(rf/reg-event-fx
  :buffer-menu/save-buffer
  (fn [{:keys [db]} _]
    (let [active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                              (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
          buffer-menu-id (:buffer-id active-window)
          line-number (get-current-line-in-buffer-menu db)
          buffer-name (get-buffer-name-at-line db buffer-menu-id line-number)
          target-buffer-id (when buffer-name
                             (some (fn [[id buf]] (when (= (:name buf) buffer-name) id))
                                   (:buffers db)))]
      (if target-buffer-id
        {:fx [[:dispatch [:save-buffer-by-id target-buffer-id]]
              [:dispatch [:echo/message (str "Saved: " buffer-name)]]]}
        {:fx [[:dispatch [:echo/message "No buffer on this line"]]]}))))

;; Unmark buffer (Issue #115)
(rf/reg-event-fx
  :buffer-menu/unmark
  (fn [{:keys [db]} _]
    (let [active-window (some (fn [[id w]] (when (= id (:active-window-id db)) w))
                              (tree-seq map? #(filter map? (vals %)) (:window-tree db)))
          buffer-menu-id (:buffer-id active-window)
          line-number (get-current-line-in-buffer-menu db)]
      (when line-number
        (swap! buffer-menu-marks update buffer-menu-id dissoc line-number))
      {:fx [[:dispatch [:echo/message "Unmarked"]]
            [:dispatch [:next-line]]]})))

;; NOTE: :buffer-menu/select-buffer is defined in events/buffer.cljs

;; Quit buffer menu (back to previous buffer)
(rf/reg-event-fx
  :buffer-menu/quit
  (fn [{:keys [db]} _]
    ;; Kill the buffer menu and return to previous buffer
    ;; Passing empty string to kill-buffer-by-name kills the current buffer
    {:fx [[:dispatch [:kill-buffer-by-name ""]]]}))

;; -- Commands Registration (Issue #115) --

(defn init-buffer-menu-commands!
  "Register buffer-menu commands. Call from main.cljs."
  []
  ;; Register list-buffers command (C-x C-b)
  ;; Uses the :list-buffers event from events/buffer.cljs which creates
  ;; the buffer AND switches to it in one atomic transaction
  (rf/dispatch-sync
   [:register-command :list-buffers
    {:docstring "Display a list of all buffers"
     :interactive nil
     :handler [:list-buffers]}])

  ;; Register buffer-menu command (alias for list-buffers)
  ;; In Emacs: buffer-menu displays in current window, list-buffers in other
  ;; For simplicity, we treat them the same
  (rf/dispatch-sync
   [:register-command :buffer-menu
    {:docstring "Display a list of all buffers"
     :interactive nil
     :handler [:list-buffers]}])

  ;; Register the event-named command (used by hardcoded keybindings in db.cljs)
  (rf/dispatch-sync
   [:register-command :buffer-menu/select-buffer
    {:docstring "Select the buffer on this line"
     :handler [:buffer-menu/select-buffer]}])

  ;; Register Emacs-style Buffer-menu-* commands for M-x access
  (rf/dispatch-sync
   [:register-command :Buffer-menu-select
    {:docstring "Select the buffer on this line"
     :handler [:buffer-menu/select-buffer]}])

  (rf/dispatch-sync
   [:register-command :Buffer-menu-mark
    {:docstring "Mark the buffer on this line"
     :handler [:buffer-menu/mark-for-deletion]}])

  (rf/dispatch-sync
   [:register-command :Buffer-menu-delete
    {:docstring "Mark the buffer on this line for deletion"
     :handler [:buffer-menu/mark-for-deletion]}])

  (rf/dispatch-sync
   [:register-command :Buffer-menu-unmark
    {:docstring "Unmark the buffer on this line"
     :handler [:buffer-menu/unmark]}])

  (rf/dispatch-sync
   [:register-command :Buffer-menu-execute
    {:docstring "Execute marked buffer operations"
     :handler [:buffer-menu/execute]}])

  (rf/dispatch-sync
   [:register-command :Buffer-menu-save
    {:docstring "Save the buffer on this line"
     :handler [:buffer-menu/save-buffer]}])

  (rf/dispatch-sync
   [:register-command :buffer-menu/quit
    {:docstring "Quit buffer menu"
     :handler [:buffer-menu/quit]}])

  (rf/dispatch-sync
   [:register-command :buffer-menu/revert
    {:docstring "Refresh the buffer list"
     :handler [:buffer-menu/revert]}])

  ;; Register buffer menu mode keybindings
  ;; These must use keywords matching registered command names
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "RET" :Buffer-menu-select])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "d" :Buffer-menu-delete])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "x" :Buffer-menu-execute])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "s" :Buffer-menu-save])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "u" :Buffer-menu-unmark])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "q" :buffer-menu/quit])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "g" :buffer-menu/revert])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "n" :next-line])
  (rf/dispatch [:keymap/set-mode-key :buffer-menu-mode "p" :previous-line])

  ;; Register C-x C-b keybinding
  (rf/dispatch [:keymap/set-global "C-x C-b" :list-buffers]))
