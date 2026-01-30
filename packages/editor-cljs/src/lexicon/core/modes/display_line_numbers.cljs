(ns lexicon.core.modes.display-line-numbers
  "Display Line Numbers Mode - Show line numbers in the gutter.

  When enabled, displays line numbers in the left margin (gutter).
  This is the modern Emacs 26+ `display-line-numbers-mode`.

  Note: `line-number-mode` (separate) shows line number in mode-line."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-display-line-numbers-mode!
  "Initialize display-line-numbers-mode."
  []
  (modes/define-minor-mode
   :display-line-numbers-mode
   "Toggle display of line numbers in the gutter"
   {:lighter " LN"
    :buffer-local? true
    :init-value true}))  ; Enabled by default

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn display-line-numbers-enabled?
  "Check if display-line-numbers-mode is enabled for a window's buffer."
  [db window-id]
  (let [window (db/find-window-in-tree (:window-tree db) window-id)
        buffer-id (:buffer-id window)
        minor-modes (get-in db [:buffers buffer-id :minor-modes] #{})]
    (contains? minor-modes :display-line-numbers-mode)))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :display-line-numbers/enabled?
 (fn [db [_ window-id]]
   (display-line-numbers-enabled? db window-id)))
