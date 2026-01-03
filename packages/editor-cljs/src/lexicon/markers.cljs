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
