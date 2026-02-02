(ns lexicon.core.text-properties
  "Text properties system for Emacs-compatible buffer annotations.

  Text properties attach metadata to character ranges in a buffer.
  This enables features like:
  - Marking completions as interactive (mouse-face, cursor-face)
  - Storing completion values (completion--string)
  - Display properties for layout (display)
  - Syntax highlighting (face)

  Storage: Uses interval-based approach where each interval stores
  properties for a range [start, end). Intervals are kept sorted
  and non-overlapping for each property.

  Key Operations:
  - put-text-property: Set a property on a range
  - get-text-property: Get property value at a position
  - remove-text-properties: Remove properties from a range
  - next-single-property-change: Find next position where property changes
  - previous-single-property-change: Find previous position where property changes

  Data Structure:
  Buffer text properties are stored in db at:
  [:buffers buffer-id :text-properties]

  Format: Map of property names to sorted interval vectors
  {:mouse-face [{:start 0 :end 10 :value 'highlight}
                {:start 20 :end 30 :value 'highlight}]
   :completion--string [{:start 0 :end 10 :value \"foo\"}
                        {:start 20 :end 30 :value \"bar\"}]}

  See Issue #138, Emacs lisp/textprop.c"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Interval Operations (Pure Functions)
;; =============================================================================

(defn- intervals-overlapping
  "Find intervals that overlap with [start, end).
   Returns vector of [index interval] pairs."
  [intervals start end]
  (reduce-kv
   (fn [acc idx interval]
     (let [istart (:start interval)
           iend (:end interval)]
       ;; Overlap if: istart < end AND iend > start
       (if (and (< istart end) (> iend start))
         (conj acc [idx interval])
         acc)))
   []
   (vec intervals)))

(defn- insert-interval
  "Insert an interval into a sorted vector, maintaining sort order."
  [intervals {:keys [start] :as interval}]
  (let [idx (count (take-while #(< (:start %) start) intervals))]
    (vec (concat (take idx intervals) [interval] (drop idx intervals)))))

(defn- merge-adjacent-intervals
  "Merge adjacent intervals with the same value."
  [intervals]
  (reduce
   (fn [acc interval]
     (if (empty? acc)
       [interval]
       (let [last-interval (peek acc)]
         (if (and (= (:end last-interval) (:start interval))
                  (= (:value last-interval) (:value interval)))
           ;; Merge
           (conj (pop acc) (assoc last-interval :end (:end interval)))
           ;; Don't merge
           (conj acc interval)))))
   []
   intervals))

(declare remove-property-interval)

(defn- set-property-interval
  "Set a property value for range [start, end) in intervals.
   Handles splitting, merging, and overwriting existing intervals."
  [intervals start end value]
  (if (nil? value)
    ;; nil value means remove
    (remove-property-interval intervals start end)
    (let [;; Find overlapping intervals
          overlapping (intervals-overlapping intervals start end)
          ;; Indices to remove
          remove-indices (set (map first overlapping))
          ;; Keep non-overlapping intervals
          kept (vec (keep-indexed
                     (fn [idx interval]
                       (when-not (remove-indices idx) interval))
                     intervals))
          ;; Create new intervals from overlapping ones
          new-intervals
          (reduce
           (fn [acc [_ interval]]
             (let [istart (:start interval)
                   iend (:end interval)
                   ival (:value interval)]
               (cond-> acc
                 ;; Left part (before our range)
                 (< istart start)
                 (conj {:start istart :end start :value ival})
                 ;; Right part (after our range)
                 (> iend end)
                 (conj {:start end :end iend :value ival}))))
           []
           overlapping)
          ;; Add our new interval
          all-new (conj new-intervals {:start start :end end :value value})
          ;; Combine and sort
          combined (sort-by :start (concat kept all-new))]
      (merge-adjacent-intervals combined))))

(defn- remove-property-interval
  "Remove a property from range [start, end) in intervals."
  [intervals start end]
  (let [overlapping (intervals-overlapping intervals start end)
        remove-indices (set (map first overlapping))
        kept (vec (keep-indexed
                   (fn [idx interval]
                     (when-not (remove-indices idx) interval))
                   intervals))
        ;; Create new intervals from split overlapping ones
        new-intervals
        (reduce
         (fn [acc [_ interval]]
           (let [istart (:start interval)
                 iend (:end interval)
                 ival (:value interval)]
             (cond-> acc
               ;; Left part
               (< istart start)
               (conj {:start istart :end start :value ival})
               ;; Right part
               (> iend end)
               (conj {:start end :end iend :value ival}))))
         []
         overlapping)
        combined (sort-by :start (concat kept new-intervals))]
    (vec combined)))

(defn- get-property-at
  "Get property value at position pos from intervals.
   Returns nil if no interval contains pos."
  [intervals pos]
  (some
   (fn [{:keys [start end value]}]
     (when (and (<= start pos) (< pos end))
       value))
   intervals))

(defn- next-property-change
  "Find next position where property changes (interval starts or ends).
   Returns nil if no change after pos."
  [intervals pos]
  (let [;; Collect all boundary positions after pos
        boundaries (reduce
                    (fn [acc {:keys [start end]}]
                      (cond-> acc
                        (> start pos) (conj start)
                        (> end pos) (conj end)))
                    #{}
                    intervals)]
    (when (seq boundaries)
      (apply min boundaries))))

(defn- previous-property-change
  "Find previous position where property changes.
   Returns nil if no change before pos."
  [intervals pos]
  (let [boundaries (reduce
                    (fn [acc {:keys [start end]}]
                      (cond-> acc
                        (< start pos) (conj start)
                        (< end pos) (conj end)))
                    #{}
                    intervals)]
    (when (seq boundaries)
      (apply max boundaries))))

;; =============================================================================
;; Public API (Pure Functions)
;; =============================================================================

(defn put-text-property
  "Set PROPERTY to VALUE for range [START, END) in TEXT-PROPS.
   TEXT-PROPS is the text-properties map from a buffer.
   Returns updated text-properties map."
  [text-props start end property value]
  (let [intervals (get text-props property [])
        new-intervals (set-property-interval intervals start end value)]
    (if (empty? new-intervals)
      (dissoc text-props property)
      (assoc text-props property new-intervals))))

(defn get-text-property
  "Get PROPERTY value at POS from TEXT-PROPS.
   Returns nil if not set."
  [text-props pos property]
  (when-let [intervals (get text-props property)]
    (get-property-at intervals pos)))

(defn get-all-text-properties
  "Get all properties at POS as a map."
  [text-props pos]
  (reduce-kv
   (fn [acc prop intervals]
     (if-let [value (get-property-at intervals pos)]
       (assoc acc prop value)
       acc))
   {}
   text-props))

(defn remove-text-properties
  "Remove PROPERTY from range [START, END) in TEXT-PROPS.
   Returns updated text-properties map."
  [text-props start end property]
  (if-let [intervals (get text-props property)]
    (let [new-intervals (remove-property-interval intervals start end)]
      (if (empty? new-intervals)
        (dissoc text-props property)
        (assoc text-props property new-intervals)))
    text-props))

(defn remove-all-properties
  "Remove all properties from range [START, END) in TEXT-PROPS."
  [text-props start end]
  (reduce-kv
   (fn [acc prop intervals]
     (let [new-intervals (remove-property-interval intervals start end)]
       (if (empty? new-intervals)
         (dissoc acc prop)
         (assoc acc prop new-intervals))))
   text-props
   text-props))

(defn next-single-property-change
  "Find next position after POS where PROPERTY changes value.
   Returns nil if no change found."
  [text-props pos property]
  (when-let [intervals (get text-props property)]
    (next-property-change intervals pos)))

(defn previous-single-property-change
  "Find previous position before POS where PROPERTY changes value.
   Returns nil if no change found."
  [text-props pos property]
  (when-let [intervals (get text-props property)]
    (previous-property-change intervals pos)))

(defn next-property-change-any
  "Find next position where ANY property changes.
   Useful for iterating through all property boundaries."
  [text-props pos]
  (let [changes (keep
                 (fn [[_prop intervals]]
                   (next-property-change intervals pos))
                 text-props)]
    (when (seq changes)
      (apply min changes))))

(defn text-property-any
  "Return t if any character in range [START, END) has PROPERTY.
   Optionally checks if property equals VALUE."
  ([text-props start end property]
   (when-let [intervals (get text-props property)]
     (seq (intervals-overlapping intervals start end))))
  ([text-props start end property value]
   (when-let [intervals (get text-props property)]
     (some
      (fn [[_ interval]]
        (= (:value interval) value))
      (intervals-overlapping intervals start end)))))

;; =============================================================================
;; Buffer Integration Helpers
;; =============================================================================

(defn adjust-for-insertion
  "Adjust text properties when text is inserted at POS.
   LENGTH is the number of characters inserted.
   Properties are typically NOT extended into inserted text."
  [text-props pos length]
  (reduce-kv
   (fn [acc prop intervals]
     (assoc acc prop
            (mapv
             (fn [{:keys [start end value]}]
               {:start (if (> start pos) (+ start length) start)
                :end (if (>= end pos) (+ end length) end)
                :value value})
             intervals)))
   {}
   text-props))

(defn adjust-for-deletion
  "Adjust text properties when text is deleted from START to END.
   Properties in the deleted range are removed."
  [text-props start end]
  (let [length (- end start)]
    (reduce-kv
     (fn [acc prop intervals]
       (let [;; First remove the deleted range
             trimmed (remove-property-interval intervals start end)
             ;; Then shift positions after the deletion
             shifted (mapv
                      (fn [{:keys [start end value] :as interval}]
                        (cond
                          ;; Entirely after deletion
                          (>= (:start interval) end)
                          {:start (- start length) :end (- end length) :value value}
                          ;; Partially after (end is after deletion)
                          (> (:end interval) end)
                          (assoc interval :end (- (:end interval) length))
                          ;; Before or at deletion point
                          :else interval))
                      trimmed)]
         (if (empty? shifted)
           (dissoc acc prop)
           (assoc acc prop shifted))))
     {}
     text-props)))

;; =============================================================================
;; Re-frame Events
;; =============================================================================

(rf/reg-event-db
 :text-props/put
 (fn [db [_ buffer-id start end property value]]
   (update-in db [:buffers buffer-id :text-properties]
              put-text-property start end property value)))

(rf/reg-event-db
 :text-props/remove
 (fn [db [_ buffer-id start end property]]
   (update-in db [:buffers buffer-id :text-properties]
              remove-text-properties start end property)))

(rf/reg-event-db
 :text-props/remove-all
 (fn [db [_ buffer-id start end]]
   (update-in db [:buffers buffer-id :text-properties]
              remove-all-properties start end)))

;; =============================================================================
;; Re-frame Subscriptions
;; =============================================================================

(rf/reg-sub
 :text-props/get
 (fn [db [_ buffer-id pos property]]
   (let [text-props (get-in db [:buffers buffer-id :text-properties])]
     (get-text-property text-props pos property))))

(rf/reg-sub
 :text-props/get-all
 (fn [db [_ buffer-id pos]]
   (let [text-props (get-in db [:buffers buffer-id :text-properties])]
     (get-all-text-properties text-props pos))))

(rf/reg-sub
 :text-props/buffer-properties
 (fn [db [_ buffer-id]]
   (get-in db [:buffers buffer-id :text-properties] {})))
