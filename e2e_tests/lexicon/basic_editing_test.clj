(ns lexicon.basic-editing-test
  "Phase 0 Basic Editing Tests - E2E with Etaoin

  Tests from ManualTestingPlan.md - P0-01 through P0-06
  Migrated from Playwright (JavaScript) to Etaoin (ClojureScript)"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [etaoin.api :as e]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000) ;; 10 seconds

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown
(defn start-driver []
  (e/firefox {:headless false}))

(defn stop-driver [driver]
  (when driver
    (e/quit driver)))

(defn with-driver [f]
  (let [driver (start-driver)]
    (try
      (binding [*driver* driver]
        (f))
      (finally
        (stop-driver driver)))))

(use-fixtures :each with-driver)

;; Helper functions
(defn wait-for-editor-ready []
  "Wait for editor to be ready by checking for .editor-wrapper"
  (e/wait *driver* {:timeout test-timeout}
          [{:css ".editor-wrapper"}]))

(defn click-editor []
  "Click the editor to focus it"
  (e/click *driver* {:css ".editor-wrapper"}))

(defn get-editor-text []
  "Get text content from the editable area"
  (e/get-element-text *driver* {:css ".editable-area"}))

(defn type-text
  "Type text with delay between characters"
  [text]
  (doseq [ch text]
    (e/fill *driver* {:css ".hidden-input"} (str ch))
    (Thread/sleep 10)))

(defn press-key
  "Press a special key (Enter, Backspace, ArrowLeft, etc.)"
  [key-name]
  (case key-name
    "Enter" (e/fill *driver* {:css ".hidden-input"} e/k-enter)
    "Backspace" (e/fill *driver* {:css ".hidden-input"} e/k-backspace)
    "Delete" (e/fill *driver* {:css ".hidden-input"} e/k-delete)
    "ArrowLeft" (e/fill *driver* {:css ".hidden-input"} e/k-left)
    "ArrowRight" (e/fill *driver* {:css ".hidden-input"} e/k-right)
    "ArrowUp" (e/fill *driver* {:css ".hidden-input"} e/k-up)
    "ArrowDown" (e/fill *driver* {:css ".hidden-input"} e/k-down))
  (Thread/sleep 50))

;; Tests
(deftest test-p0-01-basic-text-input
  (testing "P0-01: Basic text input"
    ;; Go to app
    (e/go *driver* app-url)

    ;; Wait for editor to be ready
    (wait-for-editor-ready)

    ;; Focus the editor
    (click-editor)

    ;; Type a sentence
    (let [sentence "The quick brown fox jumps over the lazy dog."]
      (type-text sentence)

      ;; Wait for updates to propagate
      (Thread/sleep 100)

      ;; Verify text appears in the buffer
      (let [editor-text (get-editor-text)]
        (is (.contains editor-text sentence)
            (str "Editor should contain: " sentence ", but got: " editor-text))))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.basic-editing-test))
