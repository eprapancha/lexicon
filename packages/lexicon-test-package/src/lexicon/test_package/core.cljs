(ns lexicon.test-package.core
  "Minimal test package for validating SCI package loading.

  This package tests:
  - SCI evaluation of external package code
  - lexicon.lisp function calls from sandboxed environment
  - Package lifecycle (initialize!/cleanup!)

  All lexicon.lisp functions are available in the user namespace
  via SCI -- call (message ...), (insert ...) etc. directly.")

(defn initialize! []
  (message "Test package initialized successfully"))

(defn cleanup! []
  (message "Test package cleanup complete"))
