(ns lexicon.package-loader
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
  (:require ;; [lexicon.evil.core]  ; DISABLED - Phase 7
            [lexicon.dired]))      ; Dired directory editor

(println "📦 Package loader: Packages loaded (dired)")
