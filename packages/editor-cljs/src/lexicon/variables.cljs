(ns lexicon.variables
  "Buffer-local and global variable system for Phase 6.5 Week 5-6.

  Implements Emacs-style buffer-local variables where each buffer can have
  its own value for a variable, with global values serving as fallback.

  Architecture:
  - Global variables stored in :global-vars map
  - Buffer-local variables stored in each buffer's :local-vars map
  - Lookup order: buffer-local → global
  - Variables can be made buffer-local with make-local-variable"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]))

;; =============================================================================
;; Variable Lookup
;; =============================================================================

(defn get-variable
  "Get value of variable, checking buffer-local then global.

  Args:
    db - Application database
    var-name - Variable name (keyword)
    buffer-id - Buffer ID (optional, uses active buffer if not provided)

  Returns:
    Variable value, or nil if not found"
  [db var-name & [buffer-id]]
  (let [buffer-id (or buffer-id
                      (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                          :buffer-id))
        local-value (get-in db [:buffers buffer-id :local-vars var-name])]
    (if (contains? (get-in db [:buffers buffer-id :local-vars]) var-name)
      local-value
      (get-in db [:global-vars var-name]))))

(defn buffer-local-value
  "Get buffer-local value from specific buffer, without global fallback.

  Args:
    db - Application database
    var-name - Variable name (keyword)
    buffer-id - Buffer ID

  Returns:
    Buffer-local value, or nil if not set locally"
  [db var-name buffer-id]
  (get-in db [:buffers buffer-id :local-vars var-name]))

(defn variable-is-buffer-local?
  "Check if variable has a buffer-local binding in given buffer.

  Args:
    db - Application database
    var-name - Variable name (keyword)
    buffer-id - Buffer ID (optional, uses active buffer if not provided)

  Returns:
    true if variable has buffer-local value, false otherwise"
  [db var-name & [buffer-id]]
  (let [buffer-id (or buffer-id
                      (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                          :buffer-id))]
    (contains? (get-in db [:buffers buffer-id :local-vars]) var-name)))

;; =============================================================================
;; Re-frame Event Handlers
;; =============================================================================

(rf/reg-event-db
 :variable/set
 (fn [db [_ var-name value buffer-id]]
   "Set variable value (buffer-local if local binding exists, global otherwise).

   Args:
     var-name - Variable name (keyword)
     value - New value
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))]
     (if (variable-is-buffer-local? db var-name buffer-id)
       ;; Has buffer-local binding - update local value
       (assoc-in db [:buffers buffer-id :local-vars var-name] value)
       ;; No buffer-local binding - update global value
       (assoc-in db [:global-vars var-name] value)))))

(rf/reg-event-db
 :variable/set-global
 (fn [db [_ var-name value]]
   "Set global variable value.

   Args:
     var-name - Variable name (keyword)
     value - New value"
   (assoc-in db [:global-vars var-name] value)))

(rf/reg-event-db
 :variable/make-local
 (fn [db [_ var-name buffer-id]]
   "Make variable buffer-local in the specified buffer.

   If variable has no local binding yet, creates one with the current value
   (from global or nil).

   Args:
     var-name - Variable name (keyword)
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))
         current-value (get-variable db var-name buffer-id)]
     (if (variable-is-buffer-local? db var-name buffer-id)
       db  ; Already buffer-local, no-op
       (assoc-in db [:buffers buffer-id :local-vars var-name] current-value)))))

(rf/reg-event-db
 :variable/kill-local
 (fn [db [_ var-name buffer-id]]
   "Remove buffer-local binding for variable.

   After this, variable will fall back to global value.

   Args:
     var-name - Variable name (keyword)
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))]
     (update-in db [:buffers buffer-id :local-vars] dissoc var-name))))

(rf/reg-event-db
 :variable/kill-all-local
 (fn [db [_ buffer-id]]
   "Remove all buffer-local variables from buffer.

   Args:
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))]
     (assoc-in db [:buffers buffer-id :local-vars] {}))))

;; =============================================================================
;; Re-frame Subscriptions
;; =============================================================================

(rf/reg-sub
 :variable/get
 (fn [db [_ var-name buffer-id]]
   "Subscribe to variable value.

   Args:
     var-name - Variable name (keyword)
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (get-variable db var-name buffer-id)))

(rf/reg-sub
 :variable/is-buffer-local?
 (fn [db [_ var-name buffer-id]]
   "Subscribe to whether variable has buffer-local binding.

   Args:
     var-name - Variable name (keyword)
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (variable-is-buffer-local? db var-name buffer-id)))

(rf/reg-sub
 :variable/buffer-local-variables
 (fn [db [_ buffer-id]]
   "Get all buffer-local variables in buffer as a map.

   Args:
     buffer-id - Buffer ID (optional, uses active buffer if not provided)"
   (let [buffer-id (or buffer-id
                       (-> (db/find-window-in-tree (:window-tree db) (:active-window-id db))
                           :buffer-id))]
     (get-in db [:buffers buffer-id :local-vars]))))

(rf/reg-sub
 :variable/global-variables
 (fn [db [_]]
   "Get all global variables as a map."
   (:global-vars db)))
