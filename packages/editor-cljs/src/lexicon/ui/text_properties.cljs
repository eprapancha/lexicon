(ns lexicon.ui.text-properties
  "Text properties system for attaching metadata to buffer text.

  Text properties are persistent attributes attached to character ranges.
  Unlike overlays, they move with the text when edited and are copied when
  text is killed/yanked.

  Common properties:
  - :face - Face to apply (visual styling)
  - :font-lock-face - Lower priority face (for syntax highlighting)
  - :invisible - Hide text
  - :read-only - Prevent modification
  - :help-echo - Tooltip text on hover
  - :display - Replace text with image/string/space"
  (:require [re-frame.core :as rf]))

;; -- Text Property Storage Structure --
;;
;; Text properties are stored as a nested map structure:
;; {start-pos {end-pos {:face :highlight :read-only true}}}
;;
;; This allows efficient range queries and property retrieval.

;; -- Helper Functions --

(defn- find-properties-at-pos
  "Find all text properties that apply at a given position.
  Returns seq of [start end props] for all overlapping ranges."
  [text-props pos]
  (for [[start end-map] text-props
        [end props] end-map
        :when (and (>= pos start) (< pos end))]
    [start end props]))

(defn- merge-properties
  "Merge multiple property maps. Later properties override earlier ones."
  [& prop-maps]
  (apply merge prop-maps))

;; -- Re-frame Event Handlers --

;; Put a text property on a range of text.
;; Properties are merged if the range already has properties.
(rf/reg-event-db
  :text-properties/put
  (fn [db [_ buffer-id start end prop-name prop-value]]
    (let [path [:buffers buffer-id :text-properties start end prop-name]]
      (assoc-in db path prop-value))))

;; Put multiple properties on a range at once.
;; Props-map: {:face :highlight :read-only true}
(rf/reg-event-db
  :text-properties/put-properties
  (fn [db [_ buffer-id start end props-map]]
    (let [path [:buffers buffer-id :text-properties start end]]
      (update-in db path merge props-map))))

;; Get a specific property value at a position.
;; Returns the value of the property, or nil if not found.
(rf/reg-sub
  :text-properties/get
  (fn [db [_ buffer-id pos prop-name]]
    (let [text-props (get-in db [:buffers buffer-id :text-properties] {})]
      (some (fn [[_ _ props]]
              (get props prop-name))
            (find-properties-at-pos text-props pos)))))

;; Get all properties at a position.
;; Returns merged map of all properties applying at this position.
(rf/reg-sub
  :text-properties/get-all
  (fn [db [_ buffer-id pos]]
    (let [text-props (get-in db [:buffers buffer-id :text-properties] {})
          prop-ranges (find-properties-at-pos text-props pos)]
      (apply merge-properties (map #(nth % 2) prop-ranges)))))

;; Remove a specific property from a range.
(rf/reg-event-db
  :text-properties/remove
  (fn [db [_ buffer-id start end prop-name]]
    (update-in db [:buffers buffer-id :text-properties start end]
               dissoc prop-name)))

;; Remove all properties from a range.
(rf/reg-event-db
  :text-properties/remove-all
  (fn [db [_ buffer-id start end]]
    (update-in db [:buffers buffer-id :text-properties start]
               dissoc end)))

;; Clear all text properties for a buffer.
(rf/reg-event-db
  :text-properties/clear-buffer
  (fn [db [_ buffer-id]]
    (assoc-in db [:buffers buffer-id :text-properties] {})))

;; -- Specialized Property Setters --

;; Set the face property on a range (common operation).
(rf/reg-event-fx
  :text-properties/set-face
  (fn [_ [_ buffer-id start end face-name]]
    {:fx [[:dispatch [:text-properties/put buffer-id start end :face face-name]]]}))

;; Set the read-only property on a range.
(rf/reg-event-fx
  :text-properties/set-read-only
  (fn [_ [_ buffer-id start end read-only?]]
    {:fx [[:dispatch [:text-properties/put buffer-id start end :read-only read-only?]]]}))

;; Set the invisible property on a range.
(rf/reg-event-fx
  :text-properties/set-invisible
  (fn [_ [_ buffer-id start end invisible?]]
    {:fx [[:dispatch [:text-properties/put buffer-id start end :invisible invisible?]]]}))

;; -- Query Functions --

;; Check if text at position is read-only.
(rf/reg-sub
  :text-properties/read-only?
  (fn [db [_ buffer-id pos]]
    (let [text-props (get-in db [:buffers buffer-id :text-properties] {})]
      (boolean (some (fn [[_ _ props]]
                       (:read-only props))
                     (find-properties-at-pos text-props pos))))))

;; Check if text at position is invisible.
(rf/reg-sub
  :text-properties/invisible?
  (fn [db [_ buffer-id pos]]
    (let [text-props (get-in db [:buffers buffer-id :text-properties] {})]
      (boolean (some (fn [[_ _ props]]
                       (:invisible props))
                     (find-properties-at-pos text-props pos))))))

;; Get face at position (for rendering).
;; Returns face name or nil.
(rf/reg-sub
  :text-properties/face-at
  (fn [db [_ buffer-id pos]]
    (let [text-props (get-in db [:buffers buffer-id :text-properties] {})]
      (some (fn [[_ _ props]]
              (or (:face props) (:font-lock-face props)))
            (find-properties-at-pos text-props pos)))))
