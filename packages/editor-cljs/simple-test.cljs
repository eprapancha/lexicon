(ns simple-test
  (:require [lexicon.db :as db]
            [lexicon.events]
            [lexicon.subs]))

;; Simple test to verify namespaces load correctly
(println "✅ lexicon.db namespace loaded")
(println "✅ lexicon.events namespace loaded") 
(println "✅ lexicon.subs namespace loaded")
(println "✅ Default DB structure:" (pr-str db/default-db))