(ns lexicon.test-package.core
  "Minimal test package for validating SCI package loading.

  This package tests:
  - SCI evaluation of external package code
  - Core API function calls from sandboxed environment
  - Package lifecycle (initialize!/cleanup!)"
  (:require [lexicon.core.api :as api]))

(defn initialize! []
  "Called when package is loaded.
  Registers a test command that displays a message."
  (api/define-command
    :test-package/hello
    {:interactive true
     :doc "Display greeting from test package"}
    (fn []
      (api/message "Hello from test package! SCI integration works.")))

  (api/message "Test package initialized successfully"))

(defn cleanup! []
  "Called when package is unloaded.
  Unregisters the test command."
  ;; Note: In future, Core API will have unregister-command
  ;; For now, just log the cleanup
  (api/message "Test package cleanup complete"))
