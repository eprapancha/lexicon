(ns lexicon.core.package-loader
  "Loads all available packages so they are compiled into the bundle.
   Packages self-register but do not auto-activate.

   ARCHITECTURE: This is where packages are loaded. Packages:
   - Live at lexicon/<package>.cljs (top-level, not in core)
   - Use ONLY lexicon.lisp as their API
   - Never import from lexicon.events, lexicon.db, or other internals
   - Self-register commands via lisp/define-command

   This maintains correct dependency direction:
   core.cljs -> package_loader.cljs -> packages
   (not: events.cljs -> packages)"
  (:require [re-frame.core :as rf]
            [lexicon.dired :as dired]))      ; Dired directory editor

(defn register-all-packages!
  "Register all package commands. Call this AFTER db is initialized."
  []
  (println "📦 Package loader: Registering packages...")
  (dired/register-dired-package!)
  (println "📦 Package loader: All packages registered"))

;; Register event that can be dispatched after db init
(rf/reg-event-fx
 :packages/register-all
 (fn [_ _]
   (println "📦 :packages/register-all event received")
   (register-all-packages!)
   {}))
