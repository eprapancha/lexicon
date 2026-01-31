(ns lexicon.core.storage.indexed-db
  "IndexedDB wrapper for persisting FileSystemDirectoryHandle objects.

  FileSystemDirectoryHandle cannot be serialized to JSON/localStorage,
  so we use IndexedDB which can store structured cloneable objects.

  Database: 'lexicon-fs-handles'
  Object Store: 'directory-handles'
  Key: path string
  Value: {:handle FileSystemDirectoryHandle :name string :granted-at timestamp}"
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private DB-NAME "lexicon-fs-handles")
(def ^:private DB-VERSION 1)
(def ^:private STORE-NAME "directory-handles")

;; =============================================================================
;; State
;; =============================================================================

(defonce ^:private db-instance (atom nil))

;; =============================================================================
;; Database Initialization
;; =============================================================================

(defn init-db!
  "Initialize IndexedDB. Returns a promise that resolves when ready.
   Dispatches :fs-access/db-ready on success, :fs-access/db-error on failure."
  []
  (if-let [indexed-db (.-indexedDB js/window)]
    (let [request (.open indexed-db DB-NAME DB-VERSION)]
      ;; Handle database upgrade (create object store)
      (set! (.-onupgradeneeded request)
            (fn [event]
              (let [db (.-result (.-target event))]
                (when-not (.contains (.-objectStoreNames db) STORE-NAME)
                  (.createObjectStore db STORE-NAME #js {:keyPath "path"})))))

      ;; Handle success
      (set! (.-onsuccess request)
            (fn [event]
              (let [db (.-result (.-target event))]
                (reset! db-instance db)
                (rf/dispatch [:fs-access/db-ready]))))

      ;; Handle error
      (set! (.-onerror request)
            (fn [event]
              (let [error (.-error (.-target event))]
                (js/console.error "IndexedDB error:" error)
                (rf/dispatch [:fs-access/db-error (str error)])))))

    ;; IndexedDB not supported
    (rf/dispatch [:fs-access/db-error "IndexedDB not supported"])))

;; =============================================================================
;; CRUD Operations
;; =============================================================================

(defn store-handle
  "Store a directory handle in IndexedDB.
   path: string path (e.g., '/home/user/projects')
   handle: FileSystemDirectoryHandle
   name: directory name
   Dispatches :fs-access/handle-stored on success."
  [path handle name]
  (when-let [db @db-instance]
    (let [tx (.transaction db #js [STORE-NAME] "readwrite")
          store (.objectStore tx STORE-NAME)
          record #js {:path path
                      :handle handle
                      :name name
                      :granted-at (.now js/Date)}
          request (.put store record)]

      (set! (.-onsuccess request)
            (fn [_]
              (rf/dispatch [:fs-access/handle-stored path])))

      (set! (.-onerror request)
            (fn [event]
              (js/console.error "Failed to store handle:" (.-error (.-target event))))))))

(defn load-handles
  "Load all stored directory handles from IndexedDB.
   Dispatches :fs-access/handles-loaded with map of {path {:handle H :name N :granted-at T}}."
  []
  (when-let [db @db-instance]
    (let [tx (.transaction db #js [STORE-NAME] "readonly")
          store (.objectStore tx STORE-NAME)
          request (.getAll store)]

      (set! (.-onsuccess request)
            (fn [event]
              (let [records (js->clj (.-result (.-target event)) :keywordize-keys true)
                    handles-map (reduce (fn [m record]
                                          (assoc m (:path record)
                                                 {:handle (:handle record)
                                                  :name (:name record)
                                                  :granted-at (:granted-at record)}))
                                        {}
                                        records)]
                (rf/dispatch [:fs-access/handles-loaded handles-map]))))

      (set! (.-onerror request)
            (fn [event]
              (js/console.error "Failed to load handles:" (.-error (.-target event)))
              (rf/dispatch [:fs-access/handles-loaded {}]))))))

(defn remove-handle
  "Remove a directory handle from IndexedDB by path.
   Dispatches :fs-access/handle-removed on success."
  [path]
  (when-let [db @db-instance]
    (let [tx (.transaction db #js [STORE-NAME] "readwrite")
          store (.objectStore tx STORE-NAME)
          request (.delete store path)]

      (set! (.-onsuccess request)
            (fn [_]
              (rf/dispatch [:fs-access/handle-removed path])))

      (set! (.-onerror request)
            (fn [event]
              (js/console.error "Failed to remove handle:" (.-error (.-target event))))))))

(defn clear-all-handles
  "Remove all stored handles. Useful for testing or resetting state.
   Dispatches :fs-access/handles-cleared on success."
  []
  (when-let [db @db-instance]
    (let [tx (.transaction db #js [STORE-NAME] "readwrite")
          store (.objectStore tx STORE-NAME)
          request (.clear store)]

      (set! (.-onsuccess request)
            (fn [_]
              (rf/dispatch [:fs-access/handles-cleared])))

      (set! (.-onerror request)
            (fn [event]
              (js/console.error "Failed to clear handles:" (.-error (.-target event))))))))

;; =============================================================================
;; Utility
;; =============================================================================

(defn db-ready?
  "Check if IndexedDB is initialized and ready."
  []
  (some? @db-instance))
