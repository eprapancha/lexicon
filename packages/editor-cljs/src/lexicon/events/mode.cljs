(ns lexicon.events.mode
  "Event handlers for major/minor modes and hook management.

  Handles:
  - Major mode activation
  - Minor mode toggling, enabling, disabling
  - Mode-line display options (line/column numbers)
  - Hook registration and execution"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; -- Major and Minor Mode Architecture --

(rf/reg-event-fx
 :set-major-mode
 (fn [{:keys [db]} [_ mode-keyword]]
   "Set the major mode for the active buffer and run mode hook"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         ;; Get mode-specific hook
         mode-hook-name (keyword (str (name mode-keyword) "-hook"))
         mode-hook-commands (get-in db [:hooks mode-hook-name] [])]
     {:db (assoc-in db [:buffers active-buffer-id :major-mode] mode-keyword)
      ;; Run mode hook commands
      :fx (mapv (fn [cmd] [:dispatch cmd]) mode-hook-commands)})))

(rf/reg-event-db
 :toggle-minor-mode
 (fn [db [_ mode-keyword]]
   "Toggle a minor mode for the active buffer"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (if (contains? current-modes mode-keyword)
       ;; Remove the mode
       (assoc-in db [:buffers active-buffer-id :minor-modes]
                 (disj current-modes mode-keyword))
       ;; Add the mode
       (assoc-in db [:buffers active-buffer-id :minor-modes]
                 (conj current-modes mode-keyword))))))

(rf/reg-event-db
 :enable-minor-mode
 (fn [db [_ mode-keyword]]
   "Enable a minor mode for the active buffer"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (assoc-in db [:buffers active-buffer-id :minor-modes]
               (conj current-modes mode-keyword)))))

(rf/reg-event-db
 :disable-minor-mode
 (fn [db [_ mode-keyword]]
   "Disable a minor mode for the active buffer"
   (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
         active-buffer-id (:buffer-id active-window)
         current-modes (get-in db [:buffers active-buffer-id :minor-modes] #{})]
     (assoc-in db [:buffers active-buffer-id :minor-modes]
               (disj current-modes mode-keyword)))))

;; -- Minor Mode Implementations (Phase 5) --

(rf/reg-event-db
 :line-number-mode
 (fn [db [_]]
   "Toggle line number display in mode line"
   (update-in db [:ui :show-line-numbers?] not)))

(rf/reg-event-db
 :column-number-mode
 (fn [db [_]]
   "Toggle column number display in mode line"
   (update-in db [:ui :show-column-number?] not)))

;; -- Hook Management --

(rf/reg-event-db
 :add-hook
 (fn [db [_ hook-name command-keyword]]
   "Add a command to a hook"
   (update-in db [:hooks hook-name] (fnil conj []) command-keyword)))

(rf/reg-event-db
 :remove-hook
 (fn [db [_ hook-name command-keyword]]
   "Remove a command from a hook"
   (update-in db [:hooks hook-name]
              (fn [commands]
                (vec (remove #(= % command-keyword) commands))))))

(rf/reg-event-fx
 :run-hook
 (fn [{:keys [db]} [_ hook-name]]
   "Run all commands registered for a hook"
   (let [hook-commands (get-in db [:hooks hook-name] [])]
     {:fx (mapv (fn [command-name]
                  [:dispatch [:execute-command command-name]])
                hook-commands)})))
