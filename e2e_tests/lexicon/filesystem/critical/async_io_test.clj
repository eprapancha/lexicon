(ns lexicon.filesystem.critical.async-io-test
  (:require
   [cljs.test :refer [deftest is async]]
   [lexicon.test.helpers :as h]))

(deftest ^:critical async-file-io-does-not-block-editor
  "Emacs/WASM invariant:
   File I/O must not block editor state mutation.

   Required for:
   - browser viability
   - responsiveness
   - correctness under latency

   Status: PENDING"
  (async done
    (h/with-editor
      (h/read-file-async \"big.txt\")
      (h/insert-text \"x\")
      (is (= \"x\" (h/current-buffer-text)))
      (done))))
