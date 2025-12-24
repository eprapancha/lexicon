(ns lexicon.package-loader
  "Loads all available packages so they are compiled into the bundle.
   Packages self-register but do not auto-activate."
  (:require [lexicon.evil.core]))  ; Load evil-mode package (registers but doesn't activate)

(println "📦 Package loader: All packages registered")
