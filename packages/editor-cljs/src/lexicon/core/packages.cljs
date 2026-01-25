(ns lexicon.core.packages
  "Package system for Lexicon - manages optional feature packages"
  (:require [re-frame.core :as rf]))

;; -- Package Registry --
;; Stores information about loaded packages

(defonce package-registry (atom {}))

;; -- Package API --

(defn register-package!
  "Register a package in the system. Called by packages on load.

  Package definition should include:
  - :name - Package name (keyword)
  - :version - Package version string
  - :initialize - Function to call when package is loaded
  - :cleanup - Function to call when package is unloaded
  - :description - Human-readable description"
  [package-def]
  (let [package-name (:name package-def)]
    (swap! package-registry assoc package-name
           (assoc package-def :loaded? false))))

(defn load-package!
  "Load and initialize a package by name"
  [package-name]
  (if-let [package (get @package-registry package-name)]
    (if (:loaded? package)
      (println "Package already loaded:" package-name)
      (do
        (println "Loading package:" package-name)
        (when-let [init-fn (:initialize package)]
          (init-fn))
        (swap! package-registry assoc-in [package-name :loaded?] true)
        (rf/dispatch [:package/loaded package-name])
        true))
    (do
      (println "Package not found:" package-name)
      false)))

(defn unload-package!
  "Unload and cleanup a package by name"
  [package-name]
  (if-let [package (get @package-registry package-name)]
    (if-not (:loaded? package)
      (println "Package not loaded:" package-name)
      (do
        (println "Unloading package:" package-name)
        (when-let [cleanup-fn (:cleanup package)]
          (cleanup-fn))
        (swap! package-registry assoc-in [package-name :loaded?] false)
        (rf/dispatch [:package/unloaded package-name])
        true))
    (do
      (println "Package not found:" package-name)
      false)))

(defn package-loaded?
  "Check if a package is currently loaded"
  [package-name]
  (get-in @package-registry [package-name :loaded?] false))

(defn package-list
  "Get list of all registered packages"
  []
  (keys @package-registry))

(defn package-info
  "Get information about a package"
  [package-name]
  (get @package-registry package-name))

;; -- Re-frame Events for Package Management --

(rf/reg-event-db
 :package/loaded
 (fn [db [_ package-name]]
   "Record that a package has been loaded"
   (assoc-in db [:packages package-name :loaded?] true)))

(rf/reg-event-db
 :package/unloaded
 (fn [db [_ package-name]]
   "Record that a package has been unloaded"
   (assoc-in db [:packages package-name :loaded?] false)))

;; -- Subscriptions --

(rf/reg-sub
 :package/loaded?
 (fn [db [_ package-name]]
   "Check if a package is loaded"
   (get-in db [:packages package-name :loaded?] false)))

(rf/reg-sub
 :packages/all
 (fn [db _]
   "Get all package info from db"
   (:packages db {})))
