(ns lexicon.ui.frames
  "Child frames (popups) for floating UI elements.

  Child frames are absolutely positioned div elements that appear above the
  main editor content. Used for:
  - In-buffer completion popups (Corfu)
  - Hover documentation
  - Tooltips
  - Context menus
  - Floating palettes

  Unlike overlays (which annotate buffer text), child frames are independent
  UI elements that can appear anywhere on screen."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

;; -- Child Frame Storage Structure --
;;
;; Child frames are stored in :child-frames at app level:
;; {:corfu-popup {:id :corfu-popup
;;                :visible? false
;;                :x 100                    ; Pixels from left
;;                :y 200                    ; Pixels from top
;;                :width 300                ; Width in pixels
;;                :height 200               ; Height in pixels
;;                :content ["Item 1" "Item 2"]  ; Content to display
;;                :selected-index 0         ; Currently selected item (optional)
;;                :face :default}}          ; Face to apply

;; -- Re-frame Event Handlers --

;; Create or update a child frame.
(rf/reg-event-db
  :frames/make
  (fn [db [_ frame-id & {:keys [x y width height content face visible? selected-index]
                         :or {x 0 y 0 width 300 height 200 visible? false face :default}}]]
    (assoc-in db [:child-frames frame-id]
              (merge {:id frame-id
                      :visible? visible?
                      :x x
                      :y y
                      :width width
                      :height height
                      :content content
                      :face face}
                     (when selected-index {:selected-index selected-index})))))

;; Show a child frame (make it visible).
(rf/reg-event-db
  :frames/show
  (fn [db [_ frame-id]]
    (assoc-in db [:child-frames frame-id :visible?] true)))

;; Hide a child frame (make it invisible but don't delete).
(rf/reg-event-db
  :frames/hide
  (fn [db [_ frame-id]]
    (assoc-in db [:child-frames frame-id :visible?] false)))

;; Delete a child frame completely.
(rf/reg-event-db
  :frames/delete
  (fn [db [_ frame-id]]
    (update db :child-frames dissoc frame-id)))

;; Move a frame to new position.
(rf/reg-event-db
  :frames/move
  (fn [db [_ frame-id x y]]
    (-> db
        (assoc-in [:child-frames frame-id :x] x)
        (assoc-in [:child-frames frame-id :y] y))))

;; Resize a frame.
(rf/reg-event-db
  :frames/resize
  (fn [db [_ frame-id width height]]
    (-> db
        (assoc-in [:child-frames frame-id :width] width)
        (assoc-in [:child-frames frame-id :height] height))))

;; Update frame content.
(rf/reg-event-db
  :frames/set-content
  (fn [db [_ frame-id content]]
    (assoc-in db [:child-frames frame-id :content] content)))

;; Set the selected index in a frame (for lists/menus).
(rf/reg-event-db
  :frames/set-selected-index
  (fn [db [_ frame-id index]]
    (assoc-in db [:child-frames frame-id :selected-index] index)))

;; Set a property on a frame.
(rf/reg-event-db
  :frames/put
  (fn [db [_ frame-id prop-name prop-value]]
    (assoc-in db [:child-frames frame-id prop-name] prop-value)))

;; -- Re-frame Subscriptions --

;; Get all visible child frames.
(rf/reg-sub
  :frames/all-visible
  (fn [db _]
    (->> (:child-frames db)
         vals
         (filter :visible?))))

;; Get a specific frame by ID.
(rf/reg-sub
  :frames/get
  (fn [db [_ frame-id]]
    (get-in db [:child-frames frame-id])))

;; Check if a frame exists.
(rf/reg-sub
  :frames/exists?
  (fn [db [_ frame-id]]
    (contains? (:child-frames db) frame-id)))

;; Check if a frame is visible.
(rf/reg-sub
  :frames/visible?
  (fn [db [_ frame-id]]
    (get-in db [:child-frames frame-id :visible?] false)))

;; -- Specialized Frame Functions --

;; Create a completion popup (for Corfu).
;; Positioned at cursor, shows completion candidates.
(rf/reg-event-fx
  :frames/completion-popup
  (fn [{:keys [db]} [_ x y candidates selected-index]]
    {:fx [[:dispatch [:frames/make :completion-popup
                      :x x
                      :y y
                      :width 400
                      :height (* 20 (min 10 (count candidates)))  ; 20px per item, max 10
                      :content candidates
                      :selected-index selected-index
                      :face :default
                      :visible? true]]]}))

;; Create a documentation hover popup.
(rf/reg-event-fx
  :frames/documentation-popup
  (fn [{:keys [db]} [_ x y doc-string]]
    {:fx [[:dispatch [:frames/make :documentation-popup
                      :x x
                      :y y
                      :width 500
                      :height 300
                      :content [doc-string]
                      :face :default
                      :visible? true]]]}))

;; Hide all frames (useful when changing buffers or modes).
(rf/reg-event-db
  :frames/hide-all
  (fn [db _]
    (update db :child-frames
            (fn [frames]
              (into {}
                    (map (fn [[id frame]]
                           [id (assoc frame :visible? false)])
                         frames))))))

;; Delete all frames.
(rf/reg-event-db
  :frames/clear-all
  (fn [db _]
    (assoc db :child-frames {})))

;; -- Reagent Components --

;; Child frame view component (to be used in main view).
;; This is a Reagent component that renders a single child frame.
(defn child-frame-view
  "Render a child frame as an absolutely positioned div."
  [frame]
  (let [face-class @(rf/subscribe [:faces/class-for (:face frame)])
        selected-index (:selected-index frame)]
    [:div.child-frame
     {:class face-class
      :style {:position "absolute"
              :left (str (:x frame) "px")
              :top (str (:y frame) "px")
              :width (str (:width frame) "px")
              :height (str (:height frame) "px")
              :visibility (if (:visible? frame) "visible" "hidden")
              :z-index 1000  ; Above all other content
              :overflow "auto"
              :border "1px solid var(--lexicon-border-default)"
              :box-shadow "0 4px 6px rgba(0, 0, 0, 0.1)"}}
     ;; Render content
     (if (vector? (:content frame))
       ;; List of items
       [:div.frame-content
        (map-indexed
         (fn [idx item]
           ^{:key idx}
           [:div.frame-item
            {:class (when (= idx selected-index) "selected")
             :style {:padding "4px 8px"
                     :cursor "pointer"
                     :background-color (when (= idx selected-index)
                                         "var(--lexicon-bg-highlight)")}}
            (str item)])
         (:content frame))]
       ;; Single string content
       [:div.frame-content
        {:style {:padding "8px"}}
        (str (:content frame))])]))

;; Container component for all child frames.
;; This should be rendered at the top level of the app.
(defn frames-container
  "Render all visible child frames."
  []
  (let [visible-frames @(rf/subscribe [:frames/all-visible])]
    [:div.child-frames-container
     {:style {:position "absolute"
              :top 0
              :left 0
              :width "100%"
              :height "100%"
              :pointer-events "none"}}  ; Allow clicks to pass through to editor
     (for [frame visible-frames]
       ^{:key (:id frame)}
       [:div {:style {:pointer-events "auto"}}  ; Re-enable clicks for frames
        [child-frame-view frame]])]))
