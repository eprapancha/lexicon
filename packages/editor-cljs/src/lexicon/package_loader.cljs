(ns lexicon.package-loader
  "Loads all available packages so they are compiled into the bundle.
   Packages self-register but do not auto-activate."
  ;; Phase 6B: Evil-mode loading disabled - deferred to Phase 7
  ;; Evil-mode will be loaded when display infrastructure is complete
  ;; (overlays, faces, buffer-local vars, hooks, advice system)
  (:require ;; [lexicon.evil.core]  ; DISABLED - Phase 7
            ))

(println "📦 Package loader: No packages auto-loaded (Phase 6B: Display & Theming)")
