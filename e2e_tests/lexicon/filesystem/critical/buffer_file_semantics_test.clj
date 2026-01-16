(ns lexicon.filesystem.critical.buffer-file-semantics-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical buffer-can-exist-without-file
  "Emacs invariant:
   Buffers are primary entities. Files are optional backing stores.

   Required for:
   - *scratch*
   - *Messages*
   - minibuffer
   - Dired
   - Magit

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer \"scratch\")]
      (is (nil? (h/buffer-file buf)))
      (h/insert-text buf \"hello\")
      (is (= \"hello\" (h/buffer-text buf))))))

(deftest ^:critical buffer-can-change-visited-file
  "Emacs invariant:
   File association is mutable metadata, not buffer identity.

   Required for:
   - rename-file-and-buffer
   - revert-buffer
   - project refactors

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer \"test\")]
      (h/visit-file buf \"/tmp/a.txt\")
      (h/visit-file buf \"/tmp/b.txt\")
      (is (= \"/tmp/b.txt\" (h/buffer-file buf)))
      (is (= buf (h/current-buffer))))))
