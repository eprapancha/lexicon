(ns lexicon.filesystem.critical.file-completion-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical file-system-exposes-completion-table
  "Emacs invariant:
   File completion must be exposed as a function-backed
   completion table, not a static list.

   Required for:
   - Vertico
   - Orderless
   - project-file completion

   Status: PENDING"
  (let [table (h/file-completion-table \"/tmp\")]
    (is (fn? table))
    (is (seq (table \"\")))))
