(ns lexicon.test-runner
  "Test runner for all Lexicon tests.

  This namespace simply requires all test namespaces so they are
  discovered by shadow-cljs browser-test runner."
  (:require [lexicon.core-test]
            [lexicon.window-test]
            [lexicon.regression-test]))
