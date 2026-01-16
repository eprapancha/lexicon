(ns lexicon.history.critical.registers-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical registers-can-store-text
  "Emacs invariant:
   Registers can store text independent of buffers.

   Required for:
   - workflows
   - macros
   - navigation

   Status: PENDING"
  (is false))

(deftest ^:critical registers-can-store-positions
  "Emacs invariant:
   Registers can store point + buffer identity.

   Required for:
   - jumps
   - Evil marks

   Status: PENDING"
  (is false))
