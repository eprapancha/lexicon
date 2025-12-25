(ns lexicon.test-runner
  "Test runner for all Lexicon tests."
  (:require [cljs.test :refer [run-tests]]
            [lexicon.core-test]
            [lexicon.window-test]
            [lexicon.regression-test]))

(defn ^:export run-all-tests []
  (run-tests 'lexicon.core-test
             'lexicon.window-test
             'lexicon.regression-test))

;; Run tests on load (for node-test target)
(defn ^:dev/after-load after-load []
  (run-all-tests))
