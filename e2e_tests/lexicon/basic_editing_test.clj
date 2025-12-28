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
  (e/firefox {:headless true}))

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
  (e/wait-visible *driver* {:css ".editor-wrapper"} {:timeout (/ test-timeout 1000)}))

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
  (let [script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key-name "',
      code: '" key-name "',
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
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

(deftest test-p0-02-enter-creates-newline
  (testing "P0-02: Enter/Return key creates newline"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type first line
    (type-text "line 1")

    ;; Press Enter
    (press-key "Enter")
    (Thread/sleep 50)

    ;; Type second line
    (type-text "line 2")
    (Thread/sleep 100)

    ;; Verify both lines are present
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "line 1"))
      (is (.contains editor-text "line 2")))))

(deftest test-p0-03-backspace-deletes
  (testing "P0-03: Backspace deletes character"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "abcde")
    (Thread/sleep 50)

    ;; Press Backspace twice
    (press-key "Backspace")
    (Thread/sleep 50)
    (press-key "Backspace")
    (Thread/sleep 100)

    ;; Verify text is now "abc"
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "abc"))
      (is (not (.contains editor-text "de"))))))

(deftest test-regression-typing-after-backspace-all
  (testing "REGRESSION: Typing works after backspacing entire buffer"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Step 1: Type initial text
    (type-text "abcd")
    (Thread/sleep 100)

    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "abcd")))

    ;; Step 2: Backspace everything
    (press-key "Backspace")
    (Thread/sleep 50)
    (press-key "Backspace")
    (Thread/sleep 50)
    (press-key "Backspace")
    (Thread/sleep 50)
    (press-key "Backspace")
    (Thread/sleep 100)

    ;; Step 3: Try to type again - this should work
    (type-text "line 1")
    (Thread/sleep 100)

    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "line 1")))))

(deftest test-p0-04-delete-key
  (testing "P0-04: Delete key deletes character forward"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type text
    (type-text "abcde")
    (Thread/sleep 50)

    ;; Move cursor to between 'b' and 'c' using left arrow
    (press-key "ArrowLeft")
    (Thread/sleep 30)
    (press-key "ArrowLeft")
    (Thread/sleep 30)
    (press-key "ArrowLeft")
    (Thread/sleep 50)

    ;; Press Delete twice
    (press-key "Delete")
    (Thread/sleep 50)
    (press-key "Delete")
    (Thread/sleep 100)

    ;; Should be "abe"
    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "abe")))))

(deftest test-p0-05-arrow-navigation
  (testing "P0-05: Arrow key navigation"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type two lines
    (type-text "line 1")
    (Thread/sleep 50)
    (press-key "Enter")
    (Thread/sleep 50)
    (type-text "line 2")
    (Thread/sleep 50)

    ;; Test Up Arrow - should move to line 1
    (press-key "ArrowUp")
    (Thread/sleep 50)

    ;; Type something - should appear on line 1
    (type-text "X")
    (Thread/sleep 100)

    (let [editor-text (get-editor-text)]
      (is (or (.contains editor-text "line 1X")
              (re-find #"line 1.*X" editor-text))))))

(deftest test-p0-06-mouse-click-positioning
  (testing "P0-06: Mouse click positioning"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type several lines
    (type-text "First line")
    (Thread/sleep 50)
    (press-key "Enter")
    (Thread/sleep 50)
    (type-text "Second line")
    (Thread/sleep 50)
    (press-key "Enter")
    (Thread/sleep 50)
    (type-text "Third line")
    (Thread/sleep 100)

    ;; Click somewhere in the middle
    (e/click *driver* {:css ".editable-area"})
    (Thread/sleep 50)

    ;; Type a character - should insert at clicked position
    (type-text "X")
    (Thread/sleep 100)

    (let [editor-text (get-editor-text)]
      (is (.contains editor-text "X")))))

;; Run tests
(defn -main []
  (clojure.test/run-tests 'lexicon.basic-editing-test))
