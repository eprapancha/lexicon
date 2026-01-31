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
    :fx [[:dispatch [:fs-access/verify-next-handle]]]}))

(rf/reg-event-fx
 :fs-access/permission-denied
 (fn [{:keys [db]} [_ path]]
   ;; Remove from IndexedDB since permission was denied
   (idb/remove-handle path)
   {:db db
    :fx [[:dispatch [:fs-access/verify-next-handle]]
         [:dispatch [:message/display (str "Permission denied for: " path)]]]}))

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
    :fx [[:dispatch [:message/display (str "Granted access to: " path)]]]}))

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
