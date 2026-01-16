(ns lexicon.core.critical.buffer-identity-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical buffer-identity-is-stable-across-views
  "Emacs invariant:
   A buffer has stable identity independent of how many windows/views
   display it. Mutations through one view must be observable through all.

   This invariant is REQUIRED for:
   - multiple windows
   - indirect buffers
   - Magit, Dired, Embark

   Status: ACTIVE"
  (h/with-editor
    (let [buf (h/create-buffer "test")]
      (h/show-buffer-in-two-windows buf)
      (h/insert-text buf "hello")
      (is (= "hello" (h/window-text 1)))
      (is (= "hello" (h/window-text 2))))))
