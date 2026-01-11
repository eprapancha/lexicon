(ns test-faces
  (:require [lexicon.faces :as faces]))

(println "Testing face resolution...")
(println "Theme registry:" (keys faces/themes))
(println "Modus Vivendi palette:" (select-keys faces/modus-vivendi-palette [:bg-mode-line-active :fg-mode-line-active]))
(println "Mode-line style:" (faces/face-to-style :modus-vivendi :mode-line))
