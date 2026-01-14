(ns lexicon.modes
  "Minor mode infrastructure for Phase 6.5 Week 5-6.

  Implements Emacs-style minor modes with:
  - Auto-generated toggle commands
  - Buffer-local mode variables
  - Mode hooks (on, off, general)
  - Keymap activation

  Minor modes are buffer-local by default, meaning each buffer can have
  modes enabled/disabled independently."
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; =============================================================================
;; Minor Mode Registry
;; =============================================================================

(defonce minor-modes
  ;; Registry of defined minor modes.
  ;; Maps mode keyword to mode definition map.
  (atom {}))

(defn register-minor-mode!
  "Register a minor mode definition.

  Args:
    mode-kw - Mode keyword (e.g., :line-number-mode)
    mode-def - Mode definition map with keys:
               :name - Human-readable name
               :lighter - Mode-line indicator (e.g., \" Ln\")
               :keymap - Mode-specific keymap (optional)
               :init-value - Initial value (default false)
               :buffer-local? - Whether mode is buffer-local (default true)
               :on-enable - Function to call when enabling (optional)
               :on-disable - Function to call when disabling (optional)"
  [mode-kw mode-def]
  (swap! minor-modes assoc mode-kw mode-def))

(defn get-minor-mode
  "Get minor mode definition.

  Args:
    mode-kw - Mode keyword

  Returns:
    Mode definition map, or nil if not found"
  [mode-kw]
  (get @minor-modes mode-kw))

;; =============================================================================
;; Mode State Management
;; =============================================================================

(defn mode-enabled?
  "Check if minor mode is enabled in buffer.

  Args:
    db - Application database
    mode-kw - Mode keyword
    buffer-id - Buffer ID (optional, uses active buffer if not provided)

  Returns:
    true if mode is enabled, false otherwise"
  [db mode-kw & [buffer-id]]
  (let [buffer-id (or buffer-id
                      (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                          :buffer-id))]
    (contains? (get-in db [:buffers buffer-id :minor-modes]) mode-kw)))

;; =============================================================================
;; Re-frame Event Handlers
;; =============================================================================

(rf/reg-event-fx
 :mode/enable
 (fn [{:keys [db]} [_ mode-kw buffer-id]]
   "Enable a minor mode in buffer.

   Args:
     mode-kw - Mode keyword
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))
         mode-def (get-minor-mode mode-kw)
         already-enabled? (mode-enabled? db mode-kw buffer-id)]
     (if (or (nil? mode-def) already-enabled?)
       {}  ; Mode not found or already enabled
       {:db (update-in db [:buffers buffer-id :minor-modes] conj mode-kw)
        :fx (concat
             ;; Call on-enable hook if defined
             (when-let [on-enable (:on-enable mode-def)]
               [[:dispatch (on-enable buffer-id)]])
             ;; Run mode hook
             [[:dispatch [:hooks/run (keyword (str (name mode-kw) "-hook"))]]])}))))

(rf/reg-event-fx
 :mode/disable
 (fn [{:keys [db]} [_ mode-kw buffer-id]]
   "Disable a minor mode in buffer.

   Args:
     mode-kw - Mode keyword
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))
         mode-def (get-minor-mode mode-kw)
         enabled? (mode-enabled? db mode-kw buffer-id)]
     (if (or (nil? mode-def) (not enabled?))
       {}  ; Mode not found or already disabled
       {:db (update-in db [:buffers buffer-id :minor-modes] disj mode-kw)
        :fx (concat
             ;; Call on-disable hook if defined
             (when-let [on-disable (:on-disable mode-def)]
               [[:dispatch (on-disable buffer-id)]])
             ;; Run mode off hook
             [[:dispatch [:hooks/run (keyword (str (name mode-kw) "-off-hook"))]]])}))))

(rf/reg-event-fx
 :mode/toggle
 (fn [{:keys [db]} [_ mode-kw buffer-id]]
   "Toggle a minor mode in buffer.

   Args:
     mode-kw - Mode keyword
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))
         enabled? (mode-enabled? db mode-kw buffer-id)]
     (if enabled?
       {:fx [[:dispatch [:mode/disable mode-kw buffer-id]]]}
       {:fx [[:dispatch [:mode/enable mode-kw buffer-id]]]}))))

;; =============================================================================
;; Re-frame Subscriptions
;; =============================================================================

(rf/reg-sub
 :mode/enabled?
 (fn [db [_ mode-kw buffer-id]]
   "Subscribe to whether mode is enabled.

   Args:
     mode-kw - Mode keyword
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (mode-enabled? db mode-kw buffer-id)))

(rf/reg-sub
 :mode/active-modes
 (fn [db [_ buffer-id]]
   "Get set of active minor modes in buffer.

   Args:
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))]
     (get-in db [:buffers buffer-id :minor-modes]))))

(rf/reg-sub
 :mode/lighter
 (fn [db [_ mode-kw]]
   "Get mode-line lighter for mode.

   Args:
     mode-kw - Mode keyword

   Returns:
     Lighter string (e.g., \" Ln\"), or nil if mode not found"
   (when-let [mode-def (get-minor-mode mode-kw)]
     (:lighter mode-def))))

;; =============================================================================
;; Helper Macro
;; =============================================================================

(defn define-minor-mode
  "Define a minor mode (function, not macro for ClojureScript).

  Args:
    mode-kw - Mode keyword (e.g., :line-number-mode)
    docstring - Documentation string
    options - Map with keys:
              :lighter - Mode-line indicator (optional)
              :keymap - Mode keymap (optional)
              :init-value - Initial value (default false)
              :buffer-local? - Buffer-local mode (default true)
              :on-enable - Event vector to dispatch when enabling
              :on-disable - Event vector to dispatch when disabling

  Returns:
    nil (registers mode and command as side effects)"
  [mode-kw docstring options]
  (let [mode-def (merge {:name (name mode-kw)
                         :lighter ""
                         :keymap nil
                         :init-value false
                         :buffer-local? true}
                        options)
        toggle-command-kw mode-kw]
    ;; Register mode
    (register-minor-mode! mode-kw mode-def)
    ;; Register toggle command
    (rf/dispatch [:register-command
                  toggle-command-kw
                  {:docstring docstring
                   :handler [:mode/toggle mode-kw nil]}])
    nil))
