(ns lexicon.events.dired
  "Dired (Directory Editor) event handlers

  Implements directory browsing and file management commands.
  Related: Issue #93"
  (:require [re-frame.core :as rf]
            [lexicon.db :as db]
            [lexicon.dired :as dired]
            [lexicon.wasm-utils :as wasm]))

;;; =============================================================================
;;; Dired Command - Open Directory
;;; =============================================================================

(rf/reg-event-fx
 :dired
 (fn [{:keys [db]} [_ directory]]
   "Open a directory in Dired mode

   Creates a read-only buffer showing directory contents with Dired interface.

   Usage: (rf/dispatch [:dired \"/path/to/directory\"])
   Interactive: M-x dired RET directory RET"
   (let [;; Get directory listing
         entries (dired/list-directory directory)

         ;; Create Dired state
         dired-state (dired/create-dired-state directory entries)

         ;; Generate buffer content
         content (dired/generate-buffer-content dired-state)

         ;; Create buffer name
         buffer-name (str "*dired " directory "*")

         ;; Find or create buffer
         existing-buffer (db/find-buffer-by-name (:buffers db) buffer-name)
         buffer-id (or (:id existing-buffer)
                       (db/next-buffer-id (:buffers db)))

         ;; Create WASM instance with content
         wasm-instance (wasm/create-wasm-instance content)

         ;; Create/update buffer
         buffer (if existing-buffer
                  (assoc existing-buffer
                         :wasm-instance wasm-instance
                         :is-read-only? true
                         :major-mode :dired-mode)
                  (-> (db/create-buffer buffer-id buffer-name wasm-instance)
                      (assoc :is-read-only? true
                             :major-mode :dired-mode)))

         ;; Set Dired state in buffer local vars
         buffer (dired/set-dired-state buffer dired-state)

         ;; Update db
         db (assoc-in db [:buffers buffer-id] buffer)]

     ;; Switch to buffer in active window
     {:db db
      :fx [[:dispatch [:switch-to-buffer buffer-id]]]})))

;;; =============================================================================
;;; Dired Refresh
;;; =============================================================================

(rf/reg-event-fx
 :dired/refresh
 (fn [{:keys [db]} [_]]
   "Refresh current Dired buffer by re-reading directory

   Preserves marks and attempts to keep point on same entry."
   (let [active-window (db/find-window-in-tree (:window-tree db)
                                                 (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]

     (if-not (dired/dired-buffer? buffer)
       ;; Not a Dired buffer - do nothing
       {}

       ;; Refresh Dired buffer
       (let [dired-state (dired/get-dired-state buffer)
             directory (:directory dired-state)

             ;; Save current entry at point for restoration
             point (get-in db [:ui :cursor-position] 0)
             buffer-text (.getText (:wasm-instance buffer))
             current-entry (dired/find-entry-at-point buffer-text point)

             ;; Re-read directory
             new-entries (dired/list-directory directory)

             ;; Update state while preserving marks
             new-state (assoc dired-state
                              :entries new-entries
                              :point-entry (:name current-entry))

             ;; Regenerate content
             new-content (dired/generate-buffer-content new-state)

             ;; Update WASM buffer
             wasm-instance (:wasm-instance buffer)]

         ;; Clear and insert new content
         (.clear wasm-instance)
         (.insert wasm-instance 0 new-content)

         ;; Update buffer state
         (let [buffer (dired/set-dired-state buffer new-state)]
           {:db (assoc-in db [:buffers buffer-id] buffer)
            ;; TODO: Restore point to same entry if it still exists
            }))))))

;;; =============================================================================
;;; Mark Operations
;;; =============================================================================

(rf/reg-event-fx
 :dired/mark
 (fn [{:keys [db]} [_]]
   "Mark file at point for operation"
   (let [active-window (db/find-window-in-tree (:window-tree db)
                                                 (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]

     (if-not (dired/dired-buffer? buffer)
       {}
       (let [point (get-in db [:ui :cursor-position] 0)
             buffer-text (.getText (:wasm-instance buffer))
             entry-info (dired/find-entry-at-point buffer-text point)]

         (if-not entry-info
           {}
           (let [dired-state (dired/get-dired-state buffer)
                 new-state (dired/toggle-mark dired-state (:index entry-info))
                 new-content (dired/generate-buffer-content new-state)]

             ;; Update buffer
             (.clear (:wasm-instance buffer))
             (.insert (:wasm-instance buffer) 0 new-content)

             {:db (assoc-in db [:buffers buffer-id]
                           (dired/set-dired-state buffer new-state))})))))))

(rf/reg-event-fx
 :dired/unmark
 (fn [{:keys [db]} [_]]
   "Remove mark from file at point"
   (let [active-window (db/find-window-in-tree (:window-tree db)
                                                 (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]

     (if-not (dired/dired-buffer? buffer)
       {}
       (let [point (get-in db [:ui :cursor-position] 0)
             buffer-text (.getText (:wasm-instance buffer))
             entry-info (dired/find-entry-at-point buffer-text point)]

         (if-not entry-info
           {}
           (let [dired-state (dired/get-dired-state buffer)
                 new-state (update dired-state :marks disj (:index entry-info))
                 new-content (dired/generate-buffer-content new-state)]

             ;; Update buffer
             (.clear (:wasm-instance buffer))
             (.insert (:wasm-instance buffer) 0 new-content)

             {:db (assoc-in db [:buffers buffer-id]
                           (dired/set-dired-state buffer new-state))})))))))

(rf/reg-event-fx
 :dired/unmark-all
 (fn [{:keys [db]} [_]]
   "Remove all marks"
   (let [active-window (db/find-window-in-tree (:window-tree db)
                                                 (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]

     (if-not (dired/dired-buffer? buffer)
       {}
       (let [dired-state (dired/get-dired-state buffer)
             new-state (dired/clear-all-marks dired-state)
             new-content (dired/generate-buffer-content new-state)]

         ;; Update buffer
         (.clear (:wasm-instance buffer))
         (.insert (:wasm-instance buffer) 0 new-content)

         {:db (assoc-in db [:buffers buffer-id]
                       (dired/set-dired-state buffer new-state))})))))

;;; =============================================================================
;;; File Operations
;;; =============================================================================

(rf/reg-event-fx
 :dired/delete-marked
 (fn [{:keys [db]} [_]]
   "Delete all marked files (after confirmation)"
   (let [active-window (db/find-window-in-tree (:window-tree db)
                                                 (:active-window-id db))
         buffer-id (:buffer-id active-window)
         buffer (get-in db [:buffers buffer-id])]

     (if-not (dired/dired-buffer? buffer)
       {}
       (let [dired-state (dired/get-dired-state buffer)
             {:keys [directory entries deletions]} dired-state
             files-to-delete (map #(get-in entries [% :name]) deletions)]

         (if (empty? files-to-delete)
           {:fx [[:dispatch [:echo/message "No files marked for deletion"]]]}

           ;; TODO: Add confirmation prompt
           ;; For now, delete directly
           (let [result (dired/delete-files directory files-to-delete)
                 {:keys [success failed]} result]

             ;; Refresh buffer to show changes
             {:fx [[:dispatch [:dired/refresh]]
                   [:dispatch [:echo/message
                              (str "Deleted: " (count success)
                                   " Failed: " (count failed))]]]})))))))

;;; =============================================================================
;;; Dired Mode Keymap
;;; =============================================================================

(def dired-mode-keymap
  "Keymap for Dired mode"
  {:n [:dired/next-line]
   :p [:dired/previous-line]
   :m [:dired/mark]
   :u [:dired/unmark]
   :U [:dired/unmark-all]
   :d [:dired/flag-for-deletion]
   :x [:dired/delete-marked]
   :g [:dired/refresh]})

;; Register Dired mode
(rf/reg-event-db
 :register-dired-mode
 (fn [db [_]]
   (assoc-in db [:modes :dired-mode]
             {:name "Dired"
              :keymap dired-mode-keymap
              :read-only? true})))
