(ns lexicon.core.fs.access
  "File System Access API wrapper for Emacs-style file operations.

  Provides:
  - Directory access granting via picker
  - File reading/writing within granted directories
  - Directory listing for Dired and completion
  - Permission verification on startup

  Browser Support:
  - Chrome/Edge: Full support (minibuffer path input)
  - Firefox/Safari: Falls back to file picker dialogs

  Based on: https://developer.mozilla.org/en-US/docs/Web/API/File_System_Access_API"
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [lexicon.core.storage.indexed-db :as idb]))

;; =============================================================================
;; API Detection
;; =============================================================================

(defn api-supported?
  "Check if File System Access API is available.
   Returns true for Chrome 86+, Edge 86+.
   Returns false for Firefox, Safari, older browsers."
  []
  (and (exists? js/window.showDirectoryPicker)
       (exists? js/window.showOpenFilePicker)
       (exists? js/window.showSaveFilePicker)))

;; =============================================================================
;; Re-frame Events - State Management
;; =============================================================================

(rf/reg-event-db
 :fs-access/initialize
 (fn [db [_]]
   (assoc db :fs-access {:api-supported? (api-supported?)
                         :granted-directories {}
                         :initializing? true})))

(rf/reg-event-db
 :fs-access/db-ready
 (fn [db [_]]
   ;; IndexedDB ready, now load stored handles
   (idb/load-handles)
   db))

(rf/reg-event-db
 :fs-access/db-error
 (fn [db [_ error]]
   (js/console.error "FS Access DB error:" error)
   (-> db
       (assoc-in [:fs-access :initializing?] false)
       (assoc-in [:fs-access :db-error] error))))

(rf/reg-event-fx
 :fs-access/handles-loaded
 (fn [{:keys [db]} [_ handles-map]]
   ;; Handles loaded from IndexedDB, verify permissions for each
   (if (empty? handles-map)
     {:db (assoc-in db [:fs-access :initializing?] false)}
     ;; Start verification chain
     {:db (-> db
              (assoc-in [:fs-access :pending-verification] handles-map)
              (assoc-in [:fs-access :granted-directories] {}))
      :fx [[:dispatch [:fs-access/verify-next-handle]]]})))

(rf/reg-event-fx
 :fs-access/verify-next-handle
 (fn [{:keys [db]} [_]]
   (let [pending (get-in db [:fs-access :pending-verification])]
     (if (empty? pending)
       ;; All handles verified
       {:db (-> db
                (update :fs-access dissoc :pending-verification)
                (assoc-in [:fs-access :initializing?] false))}
       ;; Verify next handle
       (let [[path {:keys [handle name]}] (first pending)]
         {:db (update-in db [:fs-access :pending-verification] dissoc path)
          :fx [[:fs-access/verify-permission {:path path :handle handle :name name}]]})))))

(rf/reg-fx
 :fs-access/verify-permission
 (fn [{:keys [path handle name]}]
   ;; Check if we still have permission
   (-> (.queryPermission handle #js {:mode "readwrite"})
       (.then (fn [state]
                (if (= state "granted")
                  (rf/dispatch [:fs-access/permission-verified path handle name])
                  ;; Permission not granted, try to request
                  (-> (.requestPermission handle #js {:mode "readwrite"})
                      (.then (fn [new-state]
                               (if (= new-state "granted")
                                 (rf/dispatch [:fs-access/permission-verified path handle name])
                                 (rf/dispatch [:fs-access/permission-denied path]))))
                      (.catch (fn [_]
                                (rf/dispatch [:fs-access/permission-denied path])))))))
       (.catch (fn [_]
                 (rf/dispatch [:fs-access/permission-denied path]))))))

(rf/reg-event-fx
 :fs-access/permission-verified
 (fn [{:keys [db]} [_ path handle name]]
   {:db (assoc-in db [:fs-access :granted-directories path]
                  {:handle handle :name name})
    :fx [[:dispatch [:fs-access/verify-next-handle]]
         ;; List directory contents for file completion cache
         [:fs-access/list-directory-for-cache {:path path :handle handle}]]}))

(rf/reg-event-fx
 :fs-access/permission-denied
 (fn [{:keys [db]} [_ path]]
   ;; Remove from IndexedDB since permission was denied
   (idb/remove-handle path)
   {:db db
    :fx [[:dispatch [:fs-access/verify-next-handle]]
         [:dispatch [:message/display (str "Permission denied for: " path)]]]}))

(rf/reg-event-fx
 :fs-access/directory-cached
 (fn [{:keys [db]} [_ path entries]]
   "Cache directory entries for file completion.
    Also triggers :dired/after-cache in case dired is waiting for this directory."
   {:db (assoc-in db [:fs-access :directory-cache path] entries)
    :fx [[:dispatch [:dired/after-cache path]]]}))

(rf/reg-event-db
 :fs-access/handle-stored
 (fn [db [_ _path]]
   ;; Handle stored in IndexedDB
   db))

(rf/reg-event-db
 :fs-access/handle-removed
 (fn [db [_ path]]
   (update-in db [:fs-access :granted-directories] dissoc path)))

(rf/reg-event-db
 :fs-access/handles-cleared
 (fn [db [_]]
   (assoc-in db [:fs-access :granted-directories] {})))

;; =============================================================================
;; Grant Directory Access
;; =============================================================================

(rf/reg-event-fx
 :fs-access/grant-directory
 (fn [{:keys [db]} [_]]
   (if (api-supported?)
     {:fx [[:fs-access/show-directory-picker]]}
     {:fx [[:dispatch [:message/display "File System Access API not supported in this browser"]]]})))

(rf/reg-fx
 :fs-access/show-directory-picker
 (fn [_]
   (-> (js/window.showDirectoryPicker #js {:mode "readwrite"})
       (.then (fn [handle]
                (let [name (.-name handle)
                      ;; Use the directory name as a simple path
                      ;; In real usage, we'd build a proper path
                      path (str "/" name)]
                  (rf/dispatch [:fs-access/directory-granted path handle name]))))
       (.catch (fn [err]
                 (when-not (= (.-name err) "AbortError")
                   (js/console.error "Directory picker error:" err)
                   (rf/dispatch [:message/display (str "Failed to access directory: " (.-message err))])))))))

(rf/reg-event-fx
 :fs-access/directory-granted
 (fn [{:keys [db]} [_ path handle name]]
   ;; Store in state and persist to IndexedDB
   (idb/store-handle path handle name)
   {:db (assoc-in db [:fs-access :granted-directories path]
                  {:handle handle :name name :granted-at (.now js/Date)})
    :fx [[:dispatch [:message/display (str "Granted access to: " path)]]
         ;; Load directory contents for completion
         [:fs-access/list-directory-for-cache {:path path :handle handle}]]}))

(rf/reg-fx
 :fs-access/list-directory-for-cache
 (fn [{:keys [path handle]}]
   "List immediate directory contents and cache for file completion.
    Does NOT recursively list subdirectories - only lists on-demand when
    user navigates to that directory (Emacs behavior)."
   (js/console.log "📂 Listing directory for cache:" path)
   (let [entries (atom [])]
     (letfn [(process-next [iter]
               (-> (.next iter)
                   (.then
                    (fn [result]
                      (if (.-done result)
                        @entries
                        (let [[entry-name entry-handle] (.-value result)
                              kind (.-kind entry-handle)]
                          (swap! entries conj {:name entry-name
                                               :kind kind
                                               :handle entry-handle})
                          (process-next iter)))))))]
       (-> (process-next (.entries handle))
           (.then
            (fn [entries]
              (js/console.log "📂 Cached" (count entries) "entries for" path)
              (rf/dispatch [:fs-access/directory-cached path entries])))
           (.catch
            (fn [err]
              (js/console.error "Failed to list directory:" err))))))))

;; =============================================================================
;; File Operations
;; =============================================================================

(rf/reg-fx
 :fs-access/read-file
 (fn [{:keys [handle path on-success on-error]}]
   (-> (if (instance? js/FileSystemFileHandle handle)
         ;; Direct file handle
         (.getFile handle)
         ;; Need to resolve path to get file handle
         (js/Promise.reject (js/Error. "Path resolution not yet implemented")))
       (.then (fn [file]
                (.text file)))
       (.then (fn [content]
                (rf/dispatch (conj on-success content))))
       (.catch (fn [err]
                 (if on-error
                   (rf/dispatch (conj on-error (.-message err)))
                   (rf/dispatch [:message/display (str "Read error: " (.-message err))])))))))

(rf/reg-fx
 :fs-access/write-file
 (fn [{:keys [handle content on-success on-error]}]
   (-> (.createWritable handle)
       (.then (fn [writable]
                (-> (.write writable content)
                    (.then (fn [] (.close writable)))
                    (.then (fn []
                             (when on-success
                               (rf/dispatch on-success)))))))
       (.catch (fn [err]
                 (if on-error
                   (rf/dispatch (conj on-error (.-message err)))
                   (rf/dispatch [:message/display (str "Write error: " (.-message err))])))))))

(rf/reg-fx
 :fs-access/list-directory
 (fn [{:keys [handle on-success on-error]}]
   (let [entries (atom [])]
     (-> (js/Promise.resolve
          (let [iter (.entries handle)]
            (letfn [(process-next []
                      (-> (.next iter)
                          (.then (fn [result]
                                   (if (.-done result)
                                     @entries
                                     (let [[name entry-handle] (.-value result)
                                           kind (.-kind entry-handle)]
                                       (swap! entries conj {:name name
                                                            :kind kind
                                                            :handle entry-handle})
                                       (process-next)))))))]
              (process-next))))
         (.then (fn [result]
                  (rf/dispatch (conj on-success result))))
         (.catch (fn [err]
                   (if on-error
                     (rf/dispatch (conj on-error (.-message err)))
                     (rf/dispatch [:message/display (str "List error: " (.-message err))]))))))))

(rf/reg-fx
 :fs-access/list-subdirectory-for-cache
 (fn [{:keys [grant-handle grant-path target-path]}]
   "List a subdirectory within a granted directory and cache its contents.
    - grant-handle: The FileSystemDirectoryHandle of the granted root
    - grant-path: The path of the granted root (e.g., '/projects')
    - target-path: The full path to list (e.g., '/projects/lexicon')

    Navigates from grant-handle down to target-path and lists its contents."
   (let [;; Get relative path from grant to target
         relative-path (subs target-path (count grant-path))
         ;; Split into directory parts (filter out empty strings)
         path-parts (->> (str/split relative-path #"/")
                         (filter seq)
                         vec)]
     (if (empty? path-parts)
       ;; Target is the grant root itself - already cached
       nil
       ;; Navigate to subdirectory and list it
       (letfn [(navigate-and-list [handle remaining-parts]
                 (if (empty? remaining-parts)
                   ;; Reached target - list this directory
                   (let [entries (atom [])]
                     (-> (let [iter (.entries handle)]
                           (letfn [(process-next []
                                     (-> (.next iter)
                                         (.then (fn [result]
                                                  (if (.-done result)
                                                    @entries
                                                    (let [[entry-name entry-handle] (.-value result)
                                                          kind (.-kind entry-handle)]
                                                      (swap! entries conj {:name entry-name
                                                                           :kind kind
                                                                           :handle entry-handle})
                                                      (process-next)))))))]
                             (process-next)))
                         (.then (fn [entries]
                                  (js/console.log "📂 Cached subdirectory" target-path "with" (count entries) "entries")
                                  (rf/dispatch [:fs-access/directory-cached target-path entries])))
                         (.catch (fn [err]
                                   (js/console.error "Failed to list subdirectory:" target-path err)))))
                   ;; Navigate to next directory in path
                   (-> (.getDirectoryHandle handle (first remaining-parts))
                       (.then (fn [subdir-handle]
                                (navigate-and-list subdir-handle (rest remaining-parts))))
                       (.catch (fn [err]
                                 (js/console.error "Failed to navigate to:" (first remaining-parts) err))))))]
         (navigate-and-list grant-handle path-parts))))))

;; =============================================================================
;; Path Resolution
;; =============================================================================

(defn find-granted-directory
  "Find the granted directory that contains the given path.
   Returns {:path grant-path :handle H :relative-path remaining-path} or nil."
  [db full-path]
  (let [granted (get-in db [:fs-access :granted-directories])]
    (first
     (keep (fn [[grant-path {:keys [handle]}]]
             (when (str/starts-with? full-path grant-path)
               {:grant-path grant-path
                :handle handle
                :relative-path (subs full-path (count grant-path))}))
           granted))))

(defn path-accessible?
  "Check if a path is within a granted directory."
  [db path]
  (some? (find-granted-directory db path)))

;; =============================================================================
;; Path Navigation Helpers
;; =============================================================================

(defn- split-path
  "Split a path into directory parts, filtering empty strings."
  [path]
  (->> (str/split path #"/")
       (filter seq)
       vec))

(defn- navigate-to-directory
  "Navigate from a handle to a subdirectory.
   Returns a Promise resolving to the directory handle."
  [handle path-parts]
  (if (empty? path-parts)
    (js/Promise.resolve handle)
    (-> (.getDirectoryHandle handle (first path-parts))
        (.then (fn [subdir]
                 (navigate-to-directory subdir (rest path-parts)))))))

(defn- navigate-to-parent
  "Navigate from a handle to the parent of a path.
   Returns a Promise resolving to {:parent-handle H :entry-name name}."
  [handle relative-path]
  (let [parts (split-path relative-path)
        parent-parts (butlast parts)
        entry-name (last parts)]
    (if entry-name
      (-> (navigate-to-directory handle (vec parent-parts))
          (.then (fn [parent-handle]
                   {:parent-handle parent-handle
                    :entry-name entry-name})))
      (js/Promise.reject (js/Error. "Invalid path: no entry name")))))

;; =============================================================================
;; Write Operations
;; =============================================================================

(rf/reg-fx
 :fs-access/create-directory
 (fn [{:keys [grant-handle relative-path on-success on-error]}]
   "Create a new directory at the specified path.
    - grant-handle: FileSystemDirectoryHandle of granted root
    - relative-path: Path relative to grant (e.g., '/subdir/newdir')
    - on-success: Event to dispatch on success
    - on-error: Event to dispatch on error"
   (-> (navigate-to-parent grant-handle relative-path)
       (.then (fn [{:keys [parent-handle entry-name]}]
                (.getDirectoryHandle parent-handle entry-name #js {:create true})))
       (.then (fn [_new-handle]
                (when on-success
                  (rf/dispatch on-success))))
       (.catch (fn [err]
                 (if on-error
                   (rf/dispatch (conj on-error (.-message err)))
                   (rf/dispatch [:message/display (str "Create directory error: " (.-message err))])))))))

(rf/reg-fx
 :fs-access/delete-entry
 (fn [{:keys [grant-handle relative-path recursive? on-success on-error]}]
   "Delete a file or directory at the specified path.
    - grant-handle: FileSystemDirectoryHandle of granted root
    - relative-path: Path relative to grant (e.g., '/subdir/file.txt')
    - recursive?: If true, delete directory contents recursively
    - on-success: Event to dispatch on success
    - on-error: Event to dispatch on error"
   (-> (navigate-to-parent grant-handle relative-path)
       (.then (fn [{:keys [parent-handle entry-name]}]
                (.removeEntry parent-handle entry-name
                              #js {:recursive (boolean recursive?)})))
       (.then (fn []
                (when on-success
                  (rf/dispatch on-success))))
       (.catch (fn [err]
                 (if on-error
                   (rf/dispatch (conj on-error (.-message err)))
                   (rf/dispatch [:message/display (str "Delete error: " (.-message err))])))))))

(rf/reg-fx
 :fs-access/copy-file
 (fn [{:keys [grant-handle source-path dest-path on-success on-error]}]
   "Copy a file from source to destination.
    - grant-handle: FileSystemDirectoryHandle of granted root
    - source-path: Relative path to source file
    - dest-path: Relative path to destination (can be new filename)
    - on-success: Event to dispatch on success
    - on-error: Event to dispatch on error"
   (let [source-parts (split-path source-path)
         source-parent-parts (butlast source-parts)
         source-name (last source-parts)
         dest-parts (split-path dest-path)
         dest-parent-parts (butlast dest-parts)
         dest-name (last dest-parts)]
     (-> (navigate-to-directory grant-handle (vec source-parent-parts))
         (.then (fn [source-parent]
                  (.getFileHandle source-parent source-name)))
         (.then (fn [source-file-handle]
                  (-> (.getFile source-file-handle)
                      (.then (fn [file]
                               (.text file)))
                      (.then (fn [content]
                               ;; Navigate to dest parent and create file
                               (-> (navigate-to-directory grant-handle (vec dest-parent-parts))
                                   (.then (fn [dest-parent]
                                            (.getFileHandle dest-parent dest-name #js {:create true})))
                                   (.then (fn [dest-file-handle]
                                            (.createWritable dest-file-handle)))
                                   (.then (fn [writable]
                                            (-> (.write writable content)
                                                (.then #(.close writable)))))))))))
         (.then (fn []
                  (when on-success
                    (rf/dispatch on-success))))
         (.catch (fn [err]
                   (if on-error
                     (rf/dispatch (conj on-error (.-message err)))
                     (rf/dispatch [:message/display (str "Copy error: " (.-message err))]))))))))

(rf/reg-fx
 :fs-access/move-entry
 (fn [{:keys [grant-handle source-path dest-path on-success on-error]}]
   "Move/rename a file or directory.
    - grant-handle: FileSystemDirectoryHandle of granted root
    - source-path: Relative path to source
    - dest-path: Relative path to destination
    - on-success: Event to dispatch on success
    - on-error: Event to dispatch on error

    Note: The File System Access API doesn't have a native move.
    For files, we copy then delete. For directories, this is more complex."
   (let [source-parts (split-path source-path)
         source-parent-parts (butlast source-parts)
         source-name (last source-parts)]
     ;; First determine if it's a file or directory
     (-> (navigate-to-directory grant-handle (vec source-parent-parts))
         (.then (fn [source-parent]
                  ;; Try to get as file first
                  (-> (.getFileHandle source-parent source-name)
                      (.then (fn [file-handle]
                               ;; It's a file - copy content then delete
                               (-> (.getFile file-handle)
                                   (.then (fn [file] (.text file)))
                                   (.then (fn [content]
                                            (let [dest-parts (split-path dest-path)
                                                  dest-parent-parts (butlast dest-parts)
                                                  dest-name (last dest-parts)]
                                              (-> (navigate-to-directory grant-handle (vec dest-parent-parts))
                                                  (.then (fn [dest-parent]
                                                           (.getFileHandle dest-parent dest-name #js {:create true})))
                                                  (.then (fn [dest-handle]
                                                           (.createWritable dest-handle)))
                                                  (.then (fn [writable]
                                                           (-> (.write writable content)
                                                               (.then #(.close writable)))))
                                                  (.then (fn []
                                                           ;; Delete original
                                                           (.removeEntry source-parent source-name))))))))))
                      (.catch (fn [_]
                                ;; Not a file, try directory
                                ;; For simplicity, just rename in place if same parent
                                (let [dest-parts (split-path dest-path)
                                      dest-parent-parts (butlast dest-parts)
                                      dest-name (last dest-parts)]
                                  (if (= source-parent-parts dest-parent-parts)
                                    ;; Same parent - simple rename not supported by API
                                    ;; Would need to copy entire tree
                                    (js/Promise.reject (js/Error. "Directory rename requires copying entire tree (not yet implemented)"))
                                    (js/Promise.reject (js/Error. "Moving directories between locations not yet implemented")))))))))
         (.then (fn []
                  (when on-success
                    (rf/dispatch on-success))))
         (.catch (fn [err]
                   (if on-error
                     (rf/dispatch (conj on-error (.-message err)))
                     (rf/dispatch [:message/display (str "Move error: " (.-message err))]))))))))


;; =============================================================================
;; Write Operation Events (with path resolution)
;; =============================================================================

(rf/reg-event-fx
 :fs-access/create-directory-at-path
 (fn [{:keys [db]} [_ full-path on-success on-error]]
   "Create a directory at the given full path.
    Resolves the path to a granted directory first."
   (if-let [{:keys [handle relative-path]} (find-granted-directory db full-path)]
     {:fx [[:fs-access/create-directory {:grant-handle handle
                                          :relative-path relative-path
                                          :on-success on-success
                                          :on-error on-error}]]}
     {:fx [[:dispatch (or on-error [:message/display (str "No access to: " full-path)])]]})))

(rf/reg-event-fx
 :fs-access/delete-at-path
 (fn [{:keys [db]} [_ full-path recursive? on-success on-error]]
   "Delete a file or directory at the given full path.
    Resolves the path to a granted directory first."
   (if-let [{:keys [handle relative-path]} (find-granted-directory db full-path)]
     {:fx [[:fs-access/delete-entry {:grant-handle handle
                                      :relative-path relative-path
                                      :recursive? recursive?
                                      :on-success on-success
                                      :on-error on-error}]]}
     {:fx [[:dispatch (or on-error [:message/display (str "No access to: " full-path)])]]})))

(rf/reg-event-fx
 :fs-access/copy-file-path
 (fn [{:keys [db]} [_ source-full-path dest-full-path on-success on-error]]
   "Copy a file from source to destination full paths.
    Both paths must be in the same granted directory."
   (if-let [{:keys [handle relative-path]} (find-granted-directory db source-full-path)]
     (let [dest-info (find-granted-directory db dest-full-path)]
       (if (and dest-info (= (:grant-path dest-info) (:grant-path (find-granted-directory db source-full-path))))
         {:fx [[:fs-access/copy-file {:grant-handle handle
                                       :source-path relative-path
                                       :dest-path (:relative-path dest-info)
                                       :on-success on-success
                                       :on-error on-error}]]}
         {:fx [[:dispatch (or on-error [:message/display "Source and destination must be in same granted directory"])]]}))
     {:fx [[:dispatch (or on-error [:message/display (str "No access to: " source-full-path)])]]})))

(rf/reg-event-fx
 :fs-access/move-path
 (fn [{:keys [db]} [_ source-full-path dest-full-path on-success on-error]]
   "Move/rename a file or directory.
    Both paths must be in the same granted directory."
   (if-let [{:keys [handle relative-path]} (find-granted-directory db source-full-path)]
     (let [source-info (find-granted-directory db source-full-path)
           dest-info (find-granted-directory db dest-full-path)]
       (if (and dest-info (= (:grant-path dest-info) (:grant-path source-info)))
         {:fx [[:fs-access/move-entry {:grant-handle handle
                                        :source-path relative-path
                                        :dest-path (:relative-path dest-info)
                                        :on-success on-success
                                        :on-error on-error}]]}
         {:fx [[:dispatch (or on-error [:message/display "Source and destination must be in same granted directory"])]]}))
     {:fx [[:dispatch (or on-error [:message/display (str "No access to: " source-full-path)])]]})))

;; =============================================================================
;; Subscriptions
;; =============================================================================

(rf/reg-sub
 :fs-access/supported?
 (fn [db _]
   (get-in db [:fs-access :api-supported?] false)))

(rf/reg-sub
 :fs-access/granted-directories
 (fn [db _]
   (get-in db [:fs-access :granted-directories] {})))

(rf/reg-sub
 :fs-access/initializing?
 (fn [db _]
   (get-in db [:fs-access :initializing?] false)))

;; =============================================================================
;; Commands
;; =============================================================================

(defn init-fs-access!
  "Initialize File System Access module.
   Call this from main.cljs after db init."
  []
  ;; Register grant-directory-access command
  (rf/dispatch [:register-command :grant-directory-access
                {:docstring "Grant access to a directory for file operations"
                 :interactive nil
                 :handler [:fs-access/grant-directory]}])

  ;; Initialize state
  (rf/dispatch [:fs-access/initialize])

  ;; Initialize IndexedDB
  (idb/init-db!))
