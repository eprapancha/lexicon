(ns lexicon.core.events.text-properties
  "Event handlers for text properties system.

  Text properties attach invisible metadata to buffer text ranges.
  Used by: all packages (dired, org-mode, magit, completion, syntax highlighting).

  See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Priority 1: Text Properties"
  (:require [re-frame.core :as rf]
            [lexicon.core.log :as log]))

;;; =============================================================================
;;; Text Properties Core Event
;;; =============================================================================

(rf/reg-event-db
 :text-property/put
 (fn [db [_ buffer-id start end property value]]
   "Set text property on range [start, end) in buffer.

   Text property storage structure:
   {:buffers {buffer-id {:text-properties {start {end {property value ...}}}}}}

   Properties are stored as nested maps:
   - Outer map: start position -> range-data
   - Range-data: end position -> property map
   - Property map: property -> value

   Example:
   {0 {5 {'face 'bold 'invisible t}}
    10 {15 {'face 'italic}}}

   This means:
   - [0, 5): face=bold, invisible=t
   - [10, 15): face=italic

   Implementation Notes:
   - Multiple properties can exist on same range
   - Properties are merged if ranges overlap
   - Properties persist across edits (future: need adjustment on insert/delete)
   - NOT yet: property adjustment on buffer edits (markers will handle this)"

   (log/debug (str "Setting text property: buffer=" buffer-id
                   " range=[" start "," end ")"
                   " property=" property
                   " value=" value))

   (let [;; Get existing properties or empty map
         existing-props (get-in db [:buffers buffer-id :text-properties] {})

         ;; Get or create the start-position entry
         start-entry (get existing-props start {})

         ;; Get or create the end-position property map
         end-props (get start-entry end {})

         ;; Add the new property
         updated-end-props (assoc end-props property value)

         ;; Update the structure
         updated-start-entry (assoc start-entry end updated-end-props)
         updated-props (assoc existing-props start updated-start-entry)]

     ;; Store back in db
     (assoc-in db [:buffers buffer-id :text-properties] updated-props))))

(rf/reg-event-db
 :text-property/remove
 (fn [db [_ buffer-id start end property]]
   "Remove text property from range [start, end) in buffer.

   Removes a specific property from the range, leaving other properties intact."
   (log/debug (str "Removing text property: buffer=" buffer-id
                   " range=[" start "," end ")"
                   " property=" property))

   (let [existing-props (get-in db [:buffers buffer-id :text-properties] {})
         start-entry (get existing-props start {})
         end-props (get start-entry end {})
         updated-end-props (dissoc end-props property)

         ;; Clean up empty maps
         updated-start-entry (if (empty? updated-end-props)
                              (dissoc start-entry end)
                              (assoc start-entry end updated-end-props))
         updated-props (if (empty? updated-start-entry)
                        (dissoc existing-props start)
                        (assoc existing-props start updated-start-entry))]

     (assoc-in db [:buffers buffer-id :text-properties] updated-props))))

(rf/reg-event-db
 :text-property/clear-all
 (fn [db [_ buffer-id]]
   "Clear all text properties from buffer.

   Used for buffer cleanup, mode changes, etc."
   (log/debug (str "Clearing all text properties: buffer=" buffer-id))
   (assoc-in db [:buffers buffer-id :text-properties] {})))

;;; =============================================================================
;;; Future: Property Adjustment on Buffer Edits
;;; =============================================================================

;; TODO (Phase 2): Implement property range adjustment when text is inserted/deleted
;;
;; When text is inserted before a property range, shift the range forward.
;; When text is deleted from within a property range, shrink the range.
;;
;; This will be implemented alongside markers, as both need similar position tracking.
;; For now, properties are static and don't adjust with edits.
;;
;; See: docs/DIRED_CORE_PRIMITIVES_ANALYSIS.md - Integration notes

(comment
  "Future event for adjusting properties on insert/delete"

  (rf/reg-event-db
   :text-property/adjust-on-insert
   (fn [db [_ buffer-id position length]]
     "Adjust all property ranges after insert at position.

     - Ranges before position: unchanged
     - Ranges starting at/after position: shift start+end by length
     - Ranges spanning position: expand end by length"
     ;; Implementation deferred until markers are implemented
     db))

  (rf/reg-event-db
   :text-property/adjust-on-delete
   (fn [db [_ buffer-id start end]]
     "Adjust all property ranges after delete from [start, end).

     - Ranges before start: unchanged
     - Ranges within deleted region: clamp to start
     - Ranges after end: shift back by (end - start)"
     ;; Implementation deferred until markers are implemented
     db)))
