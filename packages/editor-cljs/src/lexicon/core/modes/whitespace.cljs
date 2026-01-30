(ns lexicon.core.modes.whitespace
  "Whitespace Mode - Visualize whitespace characters.

  When enabled, renders whitespace characters as visible symbols:
  - Spaces as middle dots (·)
  - Tabs as arrows (→) followed by spaces to tab stop
  - Trailing whitespace highlighted

  Based on Emacs whitespace.el."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]
            [lexicon.core.db :as db]
            [clojure.string :as str]))

;; =============================================================================
;; Whitespace Visualization
;; =============================================================================

(def space-char "\u00B7")  ; Middle dot (·)
(def tab-char "\u2192")    ; Right arrow (→)

(defn visualize-whitespace
  "Convert whitespace characters to visible representations.
  Returns string with spaces as · and tabs as → followed by spaces."
  [text]
  (-> text
      (str/replace #" " space-char)
      (str/replace #"\t" (str tab-char "   "))))  ; Tab as arrow + 3 spaces (simulating tab width 4)

(defn has-trailing-whitespace?
  "Check if line has trailing whitespace."
  [line]
  (boolean (re-find #"[ \t]+$" line)))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-whitespace-mode!
  "Initialize whitespace-mode."
  []
  (modes/define-minor-mode
   :whitespace-mode
   "Toggle whitespace visualization"
   {:lighter " WS"
    :buffer-local? true
    :init-value false}))

;; =============================================================================
;; Query Functions
;; =============================================================================

(defn whitespace-mode-enabled?
  "Check if whitespace-mode is enabled for the active buffer."
  [db]
  (let [active-window (db/find-window-in-tree (:window-tree db) (:active-window-id db))
        buffer-id (:buffer-id active-window)]
    (contains? (get-in db [:buffers buffer-id :minor-modes]) :whitespace-mode)))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :whitespace/enabled?
 (fn [db _]
   (whitespace-mode-enabled? db)))
