(ns lexicon.core.windmove
  "Directional window selection (windmove).

  S-<arrow> keys select the window in that direction:
  - S-left:  windmove-left
  - S-right: windmove-right
  - S-up:    windmove-up
  - S-down:  windmove-down

  Based on Emacs lisp/windmove.el"
  (:require [re-frame.core :as rf]
            [lexicon.core.db :as db]))

;; =============================================================================
;; Helper Functions
;; =============================================================================

(defn window-center
  "Get the center point of a window's dimensions."
  [window]
  (let [{:keys [x y width height]} (:dimensions window)]
    {:x (+ x (/ width 2))
     :y (+ y (/ height 2))}))

(defn window-edges
  "Get the edges of a window."
  [window]
  (let [{:keys [x y width height]} (:dimensions window)]
    {:left x
     :right (+ x width)
     :top y
     :bottom (+ y height)}))

(defn windows-overlap-horizontally?
  "Check if two windows overlap on the horizontal axis (for up/down movement)."
  [w1 w2]
  (let [e1 (window-edges w1)
        e2 (window-edges w2)]
    (and (< (:left e1) (:right e2))
         (< (:left e2) (:right e1)))))

(defn windows-overlap-vertically?
  "Check if two windows overlap on the vertical axis (for left/right movement)."
  [w1 w2]
  (let [e1 (window-edges w1)
        e2 (window-edges w2)]
    (and (< (:top e1) (:bottom e2))
         (< (:top e2) (:bottom e1)))))

(defn find-window-in-direction
  "Find the best window in the given direction from the current window.
   Direction is one of :left, :right, :up, :down."
  [all-windows current-window direction]
  (let [current-center (window-center current-window)
        current-edges (window-edges current-window)

        ;; Filter candidates based on direction
        candidates
        (case direction
          :left
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(< (:right (window-edges %)) (:left current-edges)))
               (filter #(windows-overlap-vertically? current-window %)))

          :right
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(> (:left (window-edges %)) (:right current-edges)))
               (filter #(windows-overlap-vertically? current-window %)))

          :up
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(< (:bottom (window-edges %)) (:top current-edges)))
               (filter #(windows-overlap-horizontally? current-window %)))

          :down
          (->> all-windows
               (filter #(not= (:id %) (:id current-window)))
               (filter #(> (:top (window-edges %)) (:bottom current-edges)))
               (filter #(windows-overlap-horizontally? current-window %)))

          nil)]

    ;; Select the closest candidate
    (when (seq candidates)
      (case direction
        :left  (apply max-key #(:right (window-edges %)) candidates)
        :right (apply min-key #(:left (window-edges %)) candidates)
        :up    (apply max-key #(:bottom (window-edges %)) candidates)
        :down  (apply min-key #(:top (window-edges %)) candidates)))))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(defn windmove-event
  "Create a windmove event handler for a direction."
  [direction direction-name]
  (fn [{:keys [db]} [_]]
    (let [window-tree (:window-tree db)
          active-window-id (:active-window-id db)
          all-windows (db/get-all-leaf-windows window-tree)
          current-window (db/find-window-in-tree window-tree active-window-id)
          target-window (find-window-in-direction all-windows current-window direction)]
      (if target-window
        {:db (-> db
                 (assoc :active-window-id (:id target-window))
                 (assoc :cursor-owner (:id target-window)))}
        {:db db
         :fx [[:dispatch [:echo/message (str "No window " direction-name)]]]}))))

(rf/reg-event-fx :windmove/left (windmove-event :left "left"))
(rf/reg-event-fx :windmove/right (windmove-event :right "right"))
(rf/reg-event-fx :windmove/up (windmove-event :up "above"))
(rf/reg-event-fx :windmove/down (windmove-event :down "below"))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn init-windmove!
  "Initialize windmove commands and keybindings."
  []
  ;; Register commands
  (rf/dispatch [:register-command :windmove-left
                {:docstring "Select the window to the left (S-left)"
                 :handler [:windmove/left]}])

  (rf/dispatch [:register-command :windmove-right
                {:docstring "Select the window to the right (S-right)"
                 :handler [:windmove/right]}])

  (rf/dispatch [:register-command :windmove-up
                {:docstring "Select the window above (S-up)"
                 :handler [:windmove/up]}])

  (rf/dispatch [:register-command :windmove-down
                {:docstring "Select the window below (S-down)"
                 :handler [:windmove/down]}])

  ;; Set up key bindings (Shift + arrow keys)
  (rf/dispatch [:keymap/set-global "S-<left>" :windmove-left])
  (rf/dispatch [:keymap/set-global "S-<right>" :windmove-right])
  (rf/dispatch [:keymap/set-global "S-<up>" :windmove-up])
  (rf/dispatch [:keymap/set-global "S-<down>" :windmove-down]))
