(ns lexicon.test-runner
  "Test runner for all Lexicon tests.

  This namespace requires test namespaces and ensures event handlers
  are registered without initializing the full app.

  WASM module is loaded by index.html before shadow-cljs starts tests."
  (:require [lexicon.test-setup]
            [lexicon.core-test]
            [lexicon.window-test]
            [lexicon.regression-test]))
