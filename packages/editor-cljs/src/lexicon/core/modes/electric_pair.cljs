(ns lexicon.core.modes.electric-pair
  "Electric Pair Mode - Automatically insert matching delimiters.

  When enabled, typing an opening delimiter like ( [ { automatically
  inserts the corresponding closing delimiter ) ] }."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Pair Definitions
;; =============================================================================

(def pair-map
  "Map of opening to closing delimiters."
  {\( \)
   \[ \]
   \{ \}
   \" \"
   \' \'})

(def opening-chars
  "Set of opening delimiter characters."
  (set (keys pair-map)))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-electric-pair-mode!
  "Initialize electric-pair-mode."
  []
  (modes/define-minor-mode
   :electric-pair-mode
   "Toggle automatic insertion of matching delimiters"
   {:lighter " Pair"
    :buffer-local? true
    :init-value false}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn electric-pair-enabled?
  "Check if electric-pair-mode is enabled for the active buffer."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    ;; Check the standard minor-modes set, not a custom flag
    (contains? (get-in db [:buffers buffer-id :minor-modes]) :electric-pair-mode)))

(defn get-closing-char
  "Get the closing delimiter for an opening delimiter, or nil if not a pair."
  [char]
  (get pair-map char))

;; =============================================================================
;; Cursor Adjustment
;; =============================================================================

(rf/reg-event-fx
 :electric-pair/adjust-cursor
 (fn [{:keys [db]} [_ count]]
   "Move cursor back by COUNT positions (to place cursor between pair).
    Called after inserting opening+closing delimiters."
   (let [current-pos (get-in db [:ui :cursor-position] 0)
         new-pos (max 0 (- current-pos count))]
     {:fx [[:dispatch [:cursor/set-position new-pos]]]})))
