(ns lexicon.core.modes.hl-line
  "Hl-Line Mode - Highlight the current line.

  When enabled, the line containing point is highlighted with the hl-line face.
  This provides a visual indicator of cursor position."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-hl-line-mode!
  "Initialize hl-line-mode."
  []
  (modes/define-minor-mode
   :hl-line-mode
   "Toggle highlighting of the current line"
   {:lighter " HL"
    :buffer-local? true
    :init-value false}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn hl-line-enabled?
  "Check if hl-line-mode is enabled for the active buffer."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (contains? (get-in db [:buffers buffer-id :minor-modes]) :hl-line-mode)))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :hl-line/enabled?
 (fn [db _]
   (hl-line-enabled? db)))

(rf/reg-sub
 :hl-line/current-line
 (fn [db _]
   "Get the current line number for highlighting (if hl-line-mode is enabled)."
   (when (hl-line-enabled? db)
     (get-in db [:ui :cursor-position :line]))))
