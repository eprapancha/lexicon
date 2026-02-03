(ns lexicon.core.which-func
  "Which-func-mode: Display the current function in the mode line.

  This minor mode displays the name of the function/method/class the cursor
  is currently in. It uses imenu to determine the current function.

  Based on Emacs lisp/which-func.el

  Issue #130"
  (:require [re-frame.core :as rf]
            [lexicon.core.packages.imenu :as imenu]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Which-Func State
;; =============================================================================

;; State stored in db:
;; [:which-func :enabled?] - Global enable flag
;; [:buffers buffer-id :which-func-format] - Current function name for display

;; =============================================================================
;; Function Detection
;; =============================================================================

(defn find-current-function
  "Find the function containing the cursor position.

   Args:
     index - Imenu index (list of {:name :position})
     cursor-pos - Current cursor position (linear)

   Returns:
     Function name string or nil if not in a function."
  [index cursor-pos]
  (when (and (seq index) cursor-pos)
    (let [;; Sort index by position descending to find the nearest function before cursor
          sorted (sort-by :position > index)
          ;; Find the first function whose start position is before cursor
          current-fn (first (filter #(<= (:position %) cursor-pos) sorted))]
      (:name current-fn))))

(defn update-which-func
  "Update which-func display for a buffer.

   Returns updated db with which-func-format set."
  [db buffer-id cursor-pos]
  (let [index (imenu/create-imenu-index db buffer-id)
        flat-index (when index (imenu/flatten-imenu-index index))
        current-fn (find-current-function flat-index cursor-pos)]
    (assoc-in db [:buffers buffer-id :which-func-format]
              (when current-fn (str "[" current-fn "]")))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-db
 :which-func/update
 (fn [db [_ buffer-id cursor-pos]]
   "Update which-func display for current cursor position."
   (if (get-in db [:which-func :enabled?])
     (update-which-func db buffer-id cursor-pos)
     db)))

(rf/reg-event-db
 :which-func/enable
 (fn [db [_]]
   "Enable which-func-mode globally."
   (assoc-in db [:which-func :enabled?] true)))

(rf/reg-event-db
 :which-func/disable
 (fn [db [_]]
   "Disable which-func-mode globally."
   (assoc-in db [:which-func :enabled?] false)))

(rf/reg-event-fx
 :which-func/toggle
 (fn [{:keys [db]} [_]]
   "Toggle which-func-mode."
   (let [enabled? (get-in db [:which-func :enabled?] false)]
     {:db (assoc-in db [:which-func :enabled?] (not enabled?))
      :fx [[:dispatch [:message (if enabled?
                                  "Which-func-mode disabled"
                                  "Which-func-mode enabled")]]]})))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :which-func/enabled?
 (fn [db _]
   (get-in db [:which-func :enabled?] false)))

(rf/reg-sub
 :which-func/current
 (fn [db [_ buffer-id]]
   "Get current function name for a buffer."
   (get-in db [:buffers buffer-id :which-func-format])))

;; Combined subscription for mode-line display
(rf/reg-sub
 :which-func/mode-line-display
 (fn [[_ buffer-id]]
   [(rf/subscribe [:which-func/enabled?])
    (rf/subscribe [:which-func/current buffer-id])])
 (fn [[enabled? current-fn] _]
   "Get which-func display string for mode-line.
    Returns nil if disabled or no function detected."
   (when (and enabled? current-fn)
     current-fn)))

;; =============================================================================
;; Integration with Cursor Movement
;; =============================================================================

;; Hook into cursor movement to update which-func
;; This is called from cursor movement events
(defn on-cursor-move
  "Called when cursor moves to update which-func display.
   Should be called from cursor/set-position event."
  [db buffer-id cursor-pos]
  (if (get-in db [:which-func :enabled?])
    (update-which-func db buffer-id cursor-pos)
    db))

;; =============================================================================
;; Command Registration
;; =============================================================================

(defn init-which-func-commands!
  "Register which-func commands. Call from main.cljs."
  []
  (rf/dispatch
   [:register-command :which-func-mode
    {:docstring "Toggle display of current function in mode line"
     :interactive nil
     :handler (fn []
                (rf/dispatch [:which-func/toggle]))}]))
