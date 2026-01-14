(ns lexicon.modes.auto-save
  "Auto-save mode - Automatically save buffer periodically (Phase 6.5 Week 5-6).

  Example minor mode showing buffer-local mode with timer-based behavior."
  (:require [re-frame.core :as rf]
            [lexicon.modes :as modes]))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-auto-save-mode!
  "Initialize auto-save-mode."
  []
  (modes/define-minor-mode
   :auto-save-mode
   "Toggle auto-save in current buffer"
   {:lighter " AS"
    :buffer-local? true
    :init-value false
    :on-enable [:auto-save/start]
    :on-disable [:auto-save/stop]}))

;; =============================================================================
;; Mode Behavior
;; =============================================================================

(rf/reg-event-db
 :auto-save/start
 (fn [db [_ buffer-id]]
   "Start auto-save timer for buffer (placeholder).

   In a full implementation, this would:
   - Set up a timer to periodically save the buffer
   - Store timer ID in buffer state
   - Check if buffer is modified before saving

   Args:
     buffer-id - Buffer ID"
   (assoc-in db [:buffers buffer-id :auto-save-enabled?] true)))

(rf/reg-event-db
 :auto-save/stop
 (fn [db [_ buffer-id]]
   "Stop auto-save timer for buffer.

   Args:
     buffer-id - Buffer ID"
   (assoc-in db [:buffers buffer-id :auto-save-enabled?] false)))

;; =============================================================================
;; Subscription
;; =============================================================================

(rf/reg-sub
 :auto-save/enabled?
 (fn [db [_ buffer-id]]
   "Check if auto-save is enabled in buffer.

   Args:
     buffer-id - Buffer ID

   Returns:
     true if auto-save is enabled"
   (get-in db [:buffers buffer-id :auto-save-enabled?] false)))
