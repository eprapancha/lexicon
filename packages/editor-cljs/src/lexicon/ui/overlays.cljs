(ns lexicon.ui.overlays
  "Overlay system for temporary visual annotations independent of buffer text.

  Overlays are visual annotations that:
  - Don't move when text is edited (stay at fixed positions)
  - Have priority (higher priority overlays override lower ones)
  - Don't get copied when text is killed/yanked
  - Can have before-string and after-string (insert virtual text)
  - Can evaporate (auto-delete when range becomes empty)

  Common uses:
  - Visual mode selection (Evil-mode)
  - Search match highlighting
  - Linting error underlines
  - Completion UI selection
  - Temporary highlights"
  (:require [re-frame.core :as rf]))

;; -- Overlay Storage Structure --
;;
;; Overlays are stored as a map by ID:
;; {1 {:id 1
;;     :start 10
;;     :end 20
;;     :face :highlight
;;     :priority 0
;;     :before-string nil
;;     :after-string nil
;;     :evaporate false}}

;; -- Helper Functions --

(defn- overlays-at-pos
  "Get all overlays that cover a given position.
  Returns seq of overlays sorted by priority (highest first)."
  [overlays-map pos]
  (->> overlays-map
       vals
       (filter (fn [overlay]
                 (and (>= pos (:start overlay))
                      (< pos (:end overlay)))))
       (sort-by :priority >)))  ; Sort by priority descending

(defn- next-overlay-id
  "Get the next available overlay ID for a buffer."
  [db buffer-id]
  (get-in db [:buffers buffer-id :next-overlay-id] 1))

;; -- Re-frame Event Handlers --

;; Create a new overlay on a range of text.
;; Returns overlay ID in the :db effect.
(rf/reg-event-fx
  :overlays/make
  (fn [{:keys [db]} [_ buffer-id start end & {:keys [face priority before-string after-string evaporate]
                                               :or {priority 0 evaporate false}}]]
    (let [overlay-id (next-overlay-id db buffer-id)
          overlay {:id overlay-id
                   :start start
                   :end end
                   :face face
                   :priority priority
                   :before-string before-string
                   :after-string after-string
                   :evaporate evaporate}]
      {:db (-> db
               (assoc-in [:buffers buffer-id :overlays overlay-id] overlay)
               (update-in [:buffers buffer-id :next-overlay-id] inc))
       :overlay-id overlay-id})))  ; Return the ID for caller

;; Delete an overlay by ID.
(rf/reg-event-db
  :overlays/delete
  (fn [db [_ buffer-id overlay-id]]
    (update-in db [:buffers buffer-id :overlays] dissoc overlay-id)))

;; Set a property on an existing overlay.
(rf/reg-event-db
  :overlays/put
  (fn [db [_ buffer-id overlay-id prop-name prop-value]]
    (assoc-in db [:buffers buffer-id :overlays overlay-id prop-name] prop-value)))

;; Get a property from an overlay.
(rf/reg-sub
  :overlays/get
  (fn [db [_ buffer-id overlay-id prop-name]]
    (get-in db [:buffers buffer-id :overlays overlay-id prop-name])))

;; Get entire overlay data by ID.
(rf/reg-sub
  :overlays/get-overlay
  (fn [db [_ buffer-id overlay-id]]
    (get-in db [:buffers buffer-id :overlays overlay-id])))

;; Get all overlays for a buffer.
(rf/reg-sub
  :overlays/all
  (fn [db [_ buffer-id]]
    (vals (get-in db [:buffers buffer-id :overlays] {}))))

;; Get all overlays at a specific position, sorted by priority.
(rf/reg-sub
  :overlays/at-pos
  (fn [db [_ buffer-id pos]]
    (let [overlays-map (get-in db [:buffers buffer-id :overlays] {})]
      (overlays-at-pos overlays-map pos))))

;; Move an overlay to a new range.
(rf/reg-event-db
  :overlays/move
  (fn [db [_ buffer-id overlay-id new-start new-end]]
    (-> db
        (assoc-in [:buffers buffer-id :overlays overlay-id :start] new-start)
        (assoc-in [:buffers buffer-id :overlays overlay-id :end] new-end))))

;; Delete all overlays in a buffer.
(rf/reg-event-db
  :overlays/clear-buffer
  (fn [db [_ buffer-id]]
    (-> db
        (assoc-in [:buffers buffer-id :overlays] {})
        (assoc-in [:buffers buffer-id :next-overlay-id] 1))))

;; Delete overlays in a range.
(rf/reg-event-db
  :overlays/delete-range
  (fn [db [_ buffer-id start end]]
    (let [overlays (get-in db [:buffers buffer-id :overlays] {})
          overlays-to-keep (into {}
                                 (filter (fn [[id overlay]]
                                           (or (>= (:start overlay) end)
                                               (<= (:end overlay) start)))
                                         overlays))]
      (assoc-in db [:buffers buffer-id :overlays] overlays-to-keep))))

;; -- Specialized Overlay Functions --

;; Create a highlight overlay (common operation).
;; Returns overlay ID.
(rf/reg-event-fx
  :overlays/highlight
  (fn [{:keys [db]} [_ buffer-id start end face & {:keys [priority] :or {priority 0}}]]
    {:fx [[:dispatch [:overlays/make buffer-id start end
                      :face face
                      :priority priority]]]}))

;; Create a visual selection overlay for Evil-mode.
(rf/reg-event-fx
  :overlays/visual-selection
  (fn [{:keys [db]} [_ buffer-id start end]]
    {:fx [[:dispatch [:overlays/make buffer-id start end
                      :face :region
                      :priority 10]]]}))  ; High priority for selection

;; -- Face Priority Resolution --

;; Get the effective face at a position, considering both overlays and text properties.
;; Priority: highest-priority overlay > text property face > font-lock-face > default
(rf/reg-sub
  :overlays/effective-face-at
  (fn [[_ buffer-id pos]]
    [(rf/subscribe [:overlays/at-pos buffer-id pos])
     (rf/subscribe [:text-properties/face-at buffer-id pos])])
  (fn [[overlays text-prop-face] [_ buffer-id pos]]
    (or (some :face overlays)          ; First overlay with a face (highest priority)
        text-prop-face                  ; Text property face
        :default)))                     ; Default face

;; -- Utility Functions --

;; Check if an overlay exists.
(rf/reg-sub
  :overlays/exists?
  (fn [db [_ buffer-id overlay-id]]
    (contains? (get-in db [:buffers buffer-id :overlays] {}) overlay-id)))

;; Count overlays in a buffer.
(rf/reg-sub
  :overlays/count
  (fn [db [_ buffer-id]]
    (count (get-in db [:buffers buffer-id :overlays] {}))))
