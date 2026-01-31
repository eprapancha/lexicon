(ns lexicon.core.modes.delete-selection
  "Delete Selection Mode - Replace selection when typing.

  When enabled, if there is an active region (text selected) and the user
  types a character, the region is deleted before inserting the new text.
  This matches the behavior of most modern editors."
  (:require [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]
            [lexicon.core.api.buffer :as buf]))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-delete-selection-mode!
  "Initialize delete-selection-mode."
  []
  (modes/define-minor-mode
   :delete-selection-mode
   "Toggle whether typing replaces the active region"
   {:lighter " DelSel"
    :buffer-local? true
    :init-value false}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn delete-selection-enabled?
  "Check if delete-selection-mode is enabled for the active buffer."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (contains? (get-in db [:buffers buffer-id :minor-modes]) :delete-selection-mode)))

(defn region-active?
  "Check if there is an active region (mark set and different from point)."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        mark-pos (:mark-position active-window)
        current-pos (buf/point db)]
    (and mark-pos current-pos (not= mark-pos current-pos))))

(defn get-region-bounds
  "Get the start and end positions of the active region.
   Returns {:start N :end M} or nil if no active region."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        mark-pos (:mark-position active-window)
        current-pos (buf/point db)]
    (when (and mark-pos current-pos (not= mark-pos current-pos))
      {:start (min mark-pos current-pos)
       :end (max mark-pos current-pos)})))
