(ns lexicon.core.modes.line-number
  "Line number mode - Display line numbers in buffer (Phase 6.5 Week 5-6).

  Example minor mode implementation showing buffer-local mode with
  buffer-specific state."
  (:require [re-frame.core :as rf]
            [lexicon.core.modes :as modes]))

;; =============================================================================
;; Mode Definition
;; =============================================================================

(defn init-line-number-mode!
  "Initialize line-number-mode."
  []
  (modes/define-minor-mode
   :line-number-mode
   "Toggle line numbers in current buffer"
   {:lighter " Ln"
    :buffer-local? true
    :init-value false
    :on-enable [:line-number/show]
    :on-disable [:line-number/hide]}))

;; =============================================================================
;; Mode Behavior
;; =============================================================================

(rf/reg-event-db
 :line-number/show
 (fn [db [_ buffer-id]]
   "Show line numbers in buffer (placeholder - would integrate with UI).

   In a full implementation, this would:
   - Update buffer's display settings
   - Trigger re-render with line numbers visible
   - Store :show-line-numbers? true in buffer

   Args:
     buffer-id - Buffer ID"
   (assoc-in db [:buffers buffer-id :show-line-numbers?] true)))

(rf/reg-event-db
 :line-number/hide
 (fn [db [_ buffer-id]]
   "Hide line numbers in buffer.

   Args:
     buffer-id - Buffer ID"
   (assoc-in db [:buffers buffer-id :show-line-numbers?] false)))

;; =============================================================================
;; Subscription
;; =============================================================================

(rf/reg-sub
 :line-number/visible?
 (fn [db [_ buffer-id]]
   "Check if line numbers are visible in buffer.

   Args:
     buffer-id - Buffer ID

   Returns:
     true if line numbers should be shown"
   (get-in db [:buffers buffer-id :show-line-numbers?] false)))
