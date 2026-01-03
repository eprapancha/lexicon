(ns lexicon.markers
  "First-class marker abstraction for Lexicon.

  Markers are positions in buffers that automatically move with text edits.
  Unlike raw integers, markers track their position through insertions and deletions.

  Marker types:
  - :point - Cursor position (main editing point)
  - :mark - Region anchor (for selections)
  - :overlay-start - Start of overlay region
  - :overlay-end - End of overlay region
  - :custom - User-defined markers

  Marker metadata structure:
  {:id           marker-id
   :buffer-id    buffer-id
   :kind         :point | :mark | :overlay-start | :overlay-end | :custom
   :wasm-id      wasm-marker-id  ; ID in the WASM engine
   :created-at   timestamp
   :metadata     {...}}           ; Optional user metadata"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Marker Creation and Deletion
;; =============================================================================

(defn create-marker!
  "Create a new marker in BUFFER-ID at POSITION.

  Options:
  - :kind - Marker type (:point, :mark, :overlay-start, :overlay-end, :custom)
  - :metadata - Optional metadata map

  Returns marker ID (integer)."
  ([buffer-id position]
   (create-marker! buffer-id position {}))
  ([buffer-id position {:keys [kind metadata] :or {kind :custom metadata {}}}]
   (rf/dispatch [:markers/create buffer-id position kind metadata])))

(rf/reg-event-fx
  :markers/create
  (fn [{:keys [db]} [_ buffer-id position kind metadata]]
    (let [wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          marker-id (get-in db [:markers :next-id] 0)
          wasm-marker-id (.createMarker wasm-instance position)]

      (if (>= wasm-marker-id 0)
        (let [marker {:id marker-id
                      :buffer-id buffer-id
                      :kind kind
                      :wasm-id wasm-marker-id
                      :created-at (js/Date.now)
                      :metadata metadata}]
          {:db (-> db
                   (assoc-in [:markers :table marker-id] marker)
                   (update-in [:markers :next-id] inc)
                   (update-in [:buffers buffer-id :markers] (fnil conj #{}) marker-id))
           :fx [[:dispatch [:markers/created marker-id]]]})
        ;; Marker creation failed
        {:db db
         :fx [[:dispatch [:show-error (str "Failed to create marker at position " position)]]]}))))

(rf/reg-event-db
  :markers/created
  (fn [db [_ marker-id]]
    ;; Hook point for marker creation
    db))

(defn delete-marker!
  "Delete marker MARKER-ID."
  [marker-id]
  (rf/dispatch [:markers/delete marker-id]))

(rf/reg-event-fx
  :markers/delete
  (fn [{:keys [db]} [_ marker-id]]
    (let [marker (get-in db [:markers :table marker-id])
          buffer-id (:buffer-id marker)
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          wasm-marker-id (:wasm-id marker)]

      (when wasm-instance
        (.deleteMarker wasm-instance wasm-marker-id))

      {:db (-> db
               (update-in [:markers :table] dissoc marker-id)
               (update-in [:buffers buffer-id :markers] disj marker-id))})))

;; =============================================================================
;; Marker Queries
;; =============================================================================

(rf/reg-sub
  :markers/position
  (fn [db [_ marker-id]]
    (let [marker (get-in db [:markers :table marker-id])
          buffer-id (:buffer-id marker)
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          wasm-marker-id (:wasm-id marker)]
      (when wasm-instance
        (let [pos (.getMarkerPosition wasm-instance wasm-marker-id)]
          (when (>= pos 0)
            pos))))))

(rf/reg-sub
  :markers/get
  (fn [db [_ marker-id]]
    (get-in db [:markers :table marker-id])))

(rf/reg-sub
  :markers/buffer-markers
  (fn [db [_ buffer-id]]
    (let [marker-ids (get-in db [:buffers buffer-id :markers] #{})]
      (mapv #(get-in db [:markers :table %]) marker-ids))))

(rf/reg-sub
  :markers/by-kind
  (fn [db [_ buffer-id kind]]
    (let [marker-ids (get-in db [:buffers buffer-id :markers] #{})
          markers (map #(get-in db [:markers :table %]) marker-ids)]
      (filter #(= (:kind %) kind) markers))))

;; =============================================================================
;; Marker Mutation
;; =============================================================================

(defn move-marker!
  "Move MARKER-ID to NEW-POSITION in its buffer."
  [marker-id new-position]
  (rf/dispatch [:markers/move marker-id new-position]))

(rf/reg-event-fx
  :markers/move
  (fn [{:keys [db]} [_ marker-id new-position]]
    (let [marker (get-in db [:markers :table marker-id])
          buffer-id (:buffer-id marker)
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          wasm-marker-id (:wasm-id marker)]

      (if (and wasm-instance (.moveMarker wasm-instance wasm-marker-id new-position))
        {:db db}
        {:db db
         :fx [[:dispatch [:show-error (str "Failed to move marker " marker-id " to " new-position)]]]}))))

(defn update-marker-metadata!
  "Update metadata for MARKER-ID.

  UPDATES can be:
  - A map to merge with existing metadata
  - A function (fn [old-metadata] new-metadata)"
  [marker-id updates]
  (rf/dispatch [:markers/update-metadata marker-id updates]))

(rf/reg-event-db
  :markers/update-metadata
  (fn [db [_ marker-id updates]]
    (update-in db [:markers :table marker-id :metadata]
               (if (fn? updates)
                 updates
                 #(merge % updates)))))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn marker-position
  "Get current position of MARKER-ID.
  Returns position or nil if marker doesn't exist."
  [db marker-id]
  @(rf/subscribe [:markers/position marker-id]))

(defn marker-exists?
  "Check if MARKER-ID exists."
  [db marker-id]
  (boolean (get-in db [:markers :table marker-id])))

(defn buffer-marker-count
  "Get number of markers in BUFFER-ID."
  [db buffer-id]
  (count (get-in db [:buffers buffer-id :markers] #{})))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn initialize-markers!
  "Initialize marker system."
  []
  (rf/dispatch [:markers/initialize]))

(rf/reg-event-db
  :markers/initialize
  (fn [db [_]]
    (assoc db :markers {:table {}
                        :next-id 0})))

;; Auto-initialize
(initialize-markers!)

;; =============================================================================
;; Point (Cursor) Marker - Step 3
;; =============================================================================

(defn create-point-marker!
  "Create the point (cursor) marker for BUFFER-ID at POSITION.
  Returns the marker ID."
  [buffer-id position]
  (rf/dispatch [:markers/create-point buffer-id position]))

(rf/reg-event-fx
  :markers/create-point
  (fn [{:keys [db]} [_ buffer-id position]]
    (let [wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          marker-id (get-in db [:markers :next-id] 0)
          wasm-marker-id (.createMarker wasm-instance position)]

      (if (>= wasm-marker-id 0)
        (let [marker {:id marker-id
                      :buffer-id buffer-id
                      :kind :point
                      :wasm-id wasm-marker-id
                      :created-at (js/Date.now)
                      :metadata {}}]
          {:db (-> db
                   (assoc-in [:markers :table marker-id] marker)
                   (update-in [:markers :next-id] inc)
                   (assoc-in [:buffers buffer-id :point-marker-id] marker-id)
                   (update-in [:buffers buffer-id :markers] (fnil conj #{}) marker-id))})
        {:db db
         :fx [[:dispatch [:show-error (str "Failed to create point marker for buffer " buffer-id)]]]}))))

(defn point-marker-id
  "Get the point marker ID for BUFFER-ID."
  [db buffer-id]
  (get-in db [:buffers buffer-id :point-marker-id]))

(defn point
  "Get current point (cursor) position for BUFFER-ID using markers.
  Falls back to [:ui :cursor-position] if marker not initialized."
  [db buffer-id]
  (if-let [marker-id (point-marker-id db buffer-id)]
    (let [marker (get-in db [:markers :table marker-id])
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          wasm-marker-id (:wasm-id marker)]
      (when wasm-instance
        (let [pos (.getMarkerPosition wasm-instance wasm-marker-id)]
          (if (>= pos 0)
            pos
            ;; Fallback to UI cursor position
            (get-in db [:ui :cursor-position] 0)))))
    ;; No marker yet, use old system
    (get-in db [:ui :cursor-position] 0)))

(defn set-point!
  "Set point (cursor) position for BUFFER-ID to POSITION using markers."
  [buffer-id position]
  (rf/dispatch [:markers/set-point buffer-id position]))

(rf/reg-event-fx
  :markers/set-point
  (fn [{:keys [db]} [_ buffer-id position]]
    (if-let [marker-id (point-marker-id db buffer-id)]
      ;; Use marker system
      {:fx [[:dispatch [:markers/move marker-id position]]
            [:dispatch [:update-cursor-position position]]]}
      ;; Fallback to old system
      {:fx [[:dispatch [:update-cursor-position position]]]})))

(rf/reg-sub
  :markers/point
  (fn [db [_ buffer-id]]
    (point db buffer-id)))

;; =============================================================================
;; Mark (Region Anchor) Marker - Step 4
;; =============================================================================

(defn create-mark-marker!
  "Create the mark (region anchor) marker for BUFFER-ID at POSITION.
  Returns the marker ID."
  [buffer-id position]
  (rf/dispatch [:markers/create-mark buffer-id position]))

(rf/reg-event-fx
  :markers/create-mark
  (fn [{:keys [db]} [_ buffer-id position]]
    (let [wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          marker-id (get-in db [:markers :next-id] 0)
          wasm-marker-id (.createMarker wasm-instance position)]

      (if (>= wasm-marker-id 0)
        (let [marker {:id marker-id
                      :buffer-id buffer-id
                      :kind :mark
                      :wasm-id wasm-marker-id
                      :created-at (js/Date.now)
                      :metadata {}}]
          {:db (-> db
                   (assoc-in [:markers :table marker-id] marker)
                   (update-in [:markers :next-id] inc)
                   (assoc-in [:buffers buffer-id :mark-marker-id] marker-id)
                   (assoc-in [:buffers buffer-id :mark-active?] true)
                   (update-in [:buffers buffer-id :markers] (fnil conj #{}) marker-id))})
        {:db db
         :fx [[:dispatch [:show-error (str "Failed to create mark marker for buffer " buffer-id)]]]}))))

(defn mark-marker-id
  "Get the mark marker ID for BUFFER-ID."
  [db buffer-id]
  (get-in db [:buffers buffer-id :mark-marker-id]))

(defn mark
  "Get current mark position for BUFFER-ID using markers.
  Returns nil if mark is not active."
  [db buffer-id]
  (when (get-in db [:buffers buffer-id :mark-active?])
    (if-let [marker-id (mark-marker-id db buffer-id)]
      (let [marker (get-in db [:markers :table marker-id])
            wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
            wasm-marker-id (:wasm-id marker)]
        (when wasm-instance
          (let [pos (.getMarkerPosition wasm-instance wasm-marker-id)]
            (when (>= pos 0)
              pos))))
      ;; Fallback to old system
      (get-in db [:buffers buffer-id :mark]))))

(defn set-mark!
  "Set mark position for BUFFER-ID to POSITION using markers."
  [buffer-id position]
  (rf/dispatch [:markers/set-mark buffer-id position]))

(rf/reg-event-fx
  :markers/set-mark
  (fn [{:keys [db]} [_ buffer-id position]]
    (if-let [marker-id (mark-marker-id db buffer-id)]
      ;; Move existing marker
      {:fx [[:dispatch [:markers/move marker-id position]]]}
      ;; Create new mark marker
      {:fx [[:dispatch [:markers/create-mark buffer-id position]]]})))

(defn deactivate-mark!
  "Deactivate the mark for BUFFER-ID."
  [buffer-id]
  (rf/dispatch [:markers/deactivate-mark buffer-id]))

(rf/reg-event-db
  :markers/deactivate-mark
  (fn [db [_ buffer-id]]
    (assoc-in db [:buffers buffer-id :mark-active?] false)))

(defn activate-mark!
  "Activate the mark for BUFFER-ID."
  [buffer-id]
  (rf/dispatch [:markers/activate-mark buffer-id]))

(rf/reg-event-db
  :markers/activate-mark
  (fn [db [_ buffer-id]]
    (assoc-in db [:buffers buffer-id :mark-active?] true)))

(rf/reg-sub
  :markers/mark
  (fn [db [_ buffer-id]]
    (mark db buffer-id)))

(rf/reg-sub
  :markers/mark-active?
  (fn [db [_ buffer-id]]
    (get-in db [:buffers buffer-id :mark-active?] false)))

;; =============================================================================
;; Region (Point and Mark) - Step 4
;; =============================================================================

(defn region
  "Get the active region as [start end] for BUFFER-ID.
  Returns nil if mark is not active."
  [db buffer-id]
  (when-let [mark-pos (mark db buffer-id)]
    (let [point-pos (point db buffer-id)]
      (if (<= point-pos mark-pos)
        [point-pos mark-pos]
        [mark-pos point-pos]))))

(rf/reg-sub
  :markers/region
  (fn [db [_ buffer-id]]
    (region db buffer-id)))

;; =============================================================================
;; Overlay Markers - Step 5
;; =============================================================================

(defn create-overlay-markers!
  "Create start and end markers for an overlay in BUFFER-ID.
  Dispatches event to create markers asynchronously."
  [buffer-id start-pos end-pos metadata]
  (rf/dispatch [:markers/create-overlay-pair buffer-id start-pos end-pos metadata]))

(rf/reg-event-fx
  :markers/create-overlay-pair
  (fn [{:keys [db]} [_ buffer-id start-pos end-pos metadata]]
    (let [wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          start-marker-id (get-in db [:markers :next-id] 0)
          end-marker-id (inc start-marker-id)
          start-wasm-id (.createMarker wasm-instance start-pos)
          end-wasm-id (.createMarker wasm-instance end-pos)]

      (if (and (>= start-wasm-id 0) (>= end-wasm-id 0))
        (let [start-marker {:id start-marker-id
                           :buffer-id buffer-id
                           :kind :overlay-start
                           :wasm-id start-wasm-id
                           :created-at (js/Date.now)
                           :metadata metadata}
              end-marker {:id end-marker-id
                         :buffer-id buffer-id
                         :kind :overlay-end
                         :wasm-id end-wasm-id
                         :created-at (js/Date.now)
                         :metadata metadata}]
          {:db (-> db
                   (assoc-in [:markers :table start-marker-id] start-marker)
                   (assoc-in [:markers :table end-marker-id] end-marker)
                   (update-in [:markers :next-id] + 2)
                   (update-in [:buffers buffer-id :markers] (fnil conj #{}) start-marker-id end-marker-id))
           :fx [[:dispatch [:markers/overlay-pair-created buffer-id start-marker-id end-marker-id]]]})
        {:db db
         :fx [[:dispatch [:show-error (str "Failed to create overlay markers for buffer " buffer-id)]]]})))

(rf/reg-event-db
  :markers/overlay-pair-created
  (fn [db [_ buffer-id start-marker-id end-marker-id]]
    ;; Hook point for overlay marker pair creation
    db))

(defn delete-overlay-markers!
  "Delete both start and end markers for an overlay."
  [{:keys [start-marker-id end-marker-id]}]
  (when start-marker-id
    (delete-marker! start-marker-id))
  (when end-marker-id
    (delete-marker! end-marker-id)))

(defn overlay-region
  "Get the current region [start end] for an overlay's markers.
  Returns nil if markers don't exist."
  [db {:keys [start-marker-id end-marker-id]}]
  (when (and start-marker-id end-marker-id)
    (let [start-marker (get-in db [:markers :table start-marker-id])
          end-marker (get-in db [:markers :table end-marker-id])
          buffer-id (:buffer-id start-marker)
          wasm-instance (get-in db [:buffers buffer-id :wasm-instance])
          start-wasm-id (:wasm-id start-marker)
          end-wasm-id (:wasm-id end-marker)]
      (when (and wasm-instance start-marker end-marker)
        (let [start-pos (.getMarkerPosition wasm-instance start-wasm-id)
              end-pos (.getMarkerPosition wasm-instance end-wasm-id)]
          (when (and (>= start-pos 0) (>= end-pos 0))
            [start-pos end-pos]))))))

(defn move-overlay-markers!
  "Move both markers of an overlay to new positions."
  [{:keys [start-marker-id end-marker-id]} new-start new-end]
  (when start-marker-id
    (move-marker! start-marker-id new-start))
  (when end-marker-id
    (move-marker! end-marker-id new-end)))

(rf/reg-sub
  :markers/overlay-region
  (fn [db [_ overlay-markers]]
    (overlay-region db overlay-markers)))
