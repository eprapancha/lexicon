(ns lexicon.interaction.high.read-only-buffers-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:high read-only-buffer-prevents-modification
  "Emacs invariant:
   Read-only buffers reject edits but allow navigation.

   Required for:
   - help buffers
   - Magit
   - Dired

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer \"help\")]
      (h/set-read-only buf true)
      (h/insert-text buf \"x\")
      (is (= \"\" (h/buffer-text buf))))))
