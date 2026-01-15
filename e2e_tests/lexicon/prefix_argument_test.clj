(ns lexicon.prefix-argument-test
  "Phase 6.5 Week 1-2: Prefix Argument (C-u) Tests - E2E with Etaoin

  Tests prefix argument accumulation and command modification:
  - C-u alone → (4)
  - C-u C-u → (16)
  - C-u 5 → 5
  - C-u - → '-
  - C-u - 5 → -5
  - Interactive spec 'p' conversion
  - Interactive spec 'P' raw form

  Based on Issue #76 acceptance criteria."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as test-helpers]))

;; Test configuration
(def app-url "http://localhost:8080")
(def test-timeout 10000) ;; 10 seconds

;; Browser driver (will be set by fixture)
(def ^:dynamic *driver* nil)

;; Setup/teardown - use common fixture with automatic *Messages* printing
(use-fixtures :once (partial test-helpers/with-driver-and-messages #'*driver*))

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

(defn press-ctrl-key
  "Press Ctrl+key combination (e.g., 'u' for C-u)"
  [key]
  (let [key-code (str "Key" (str/upper-case key))
        script (str "
    const input = document.querySelector('.hidden-input');
    input.focus();
    const event = new KeyboardEvent('keydown', {
      key: '" key "',
      code: '" key-code "',
      ctrlKey: true,
      bubbles: true
    });
    input.dispatchEvent(event);
  ")]
    (e/js-execute *driver* script))
  (Thread/sleep 50))

(defn press-key
  "Press a non-modifier key"
  [key]
  (e/fill *driver* {:css ".hidden-input"} key)
  (Thread/sleep 50))

(defn press-minus
  "Press the minus key"
  []
  (e/fill *driver* {:css ".hidden-input"} "-")
  (Thread/sleep 50))

(defn get-prefix-arg
  "Get current prefix-arg from window.editorState"
  []
  (e/js-execute *driver* "
    const state = window.editorState;
    if (!state) return null;
    return state.prefixArg || null;
  "))

;; =============================================================================
;; Test Suite: Prefix Argument Accumulation
;; =============================================================================

(deftest test-cu-alone
  (testing "C-u alone sets prefix-arg to (4)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u
    (press-ctrl-key "u")

    ;; Check prefix-arg is (4) - represented as a list/array in JS
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 4 (first prefix-arg)) "prefix-arg should be (4)"))))

(deftest test-cu-cu
  (testing "C-u C-u sets prefix-arg to (16)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u C-u
    (press-ctrl-key "u")
    (press-ctrl-key "u")

    ;; Check prefix-arg is (16)
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 16 (first prefix-arg)) "prefix-arg should be (16)"))))

(deftest test-cu-digit
  (testing "C-u 5 sets prefix-arg to 5"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u 5
    (press-ctrl-key "u")
    (press-key "5")

    ;; Check prefix-arg is 5 (number)
    (let [prefix-arg (get-prefix-arg)]
      (is (number? prefix-arg) "prefix-arg should be a number")
      (is (= 5 prefix-arg) "prefix-arg should be 5"))))

(deftest test-cu-multi-digit
  (testing "C-u 5 2 sets prefix-arg to 52"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u 5 2
    (press-ctrl-key "u")
    (press-key "5")
    (press-key "2")

    ;; Check prefix-arg is 52
    (let [prefix-arg (get-prefix-arg)]
      (is (= 52 prefix-arg) "prefix-arg should be 52"))))

(deftest test-cu-minus
  (testing "C-u - sets prefix-arg to '- (symbol)"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u -
    (press-ctrl-key "u")
    (press-minus)

    ;; Check prefix-arg is '- (symbol, represented as string in ClojureScript)
    (let [prefix-arg (get-prefix-arg)]
      (is (or (= prefix-arg "-") (= prefix-arg '-))
          "prefix-arg should be '- symbol"))))

(deftest test-cu-minus-digit
  (testing "C-u - 5 sets prefix-arg to -5"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Press C-u - 5
    (press-ctrl-key "u")
    (press-minus)
    (press-key "5")

    ;; Check prefix-arg is -5
    (let [prefix-arg (get-prefix-arg)]
      (is (= -5 prefix-arg) "prefix-arg should be -5"))))

;; =============================================================================
;; Test Suite: Prefix Argument with Commands
;; =============================================================================

(deftest test-cu-forward-char
  (testing "C-u 4 C-f moves forward 4 characters"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Type some text
    (type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to beginning
    (press-ctrl-key "a")
    (Thread/sleep 50)

    ;; C-u 4 C-f should move forward 4 chars
    (press-ctrl-key "u")
    (press-key "4")
    (press-ctrl-key "f")
    (Thread/sleep 100)

    ;; Cursor should be after "Hell" (position 4)
    ;; Insert a marker to verify position
    (type-text "X")
    (Thread/sleep 100)

    (let [text (get-editor-text)]
      (is (str/includes? text "HellXo World")
          "Cursor should be at position 4 after C-u 4 C-f"))))

(deftest test-prefix-arg-cleared-after-command
  (testing "Prefix arg is cleared after command execution"
    (e/go *driver* app-url)
    (wait-for-editor-ready)
    (click-editor)

    ;; Set prefix arg
    (press-ctrl-key "u")
    (Thread/sleep 50)
    (let [prefix-arg-before (get-prefix-arg)]
      (is prefix-arg-before "prefix-arg should be set"))

    ;; Execute command
    (press-ctrl-key "f")
    (Thread/sleep 100)

    ;; Check prefix-arg is cleared
    (let [prefix-arg-after (get-prefix-arg)]
      (is (nil? prefix-arg-after) "prefix-arg should be cleared after command"))))

;; =============================================================================
;; Test Suite: Mode Line Display (Future - requires mode-line implementation)
;; =============================================================================

(comment
  (deftest test-mode-line-display
    (testing "C-u shows 'C-u' in mode line"
      ;; Future test - requires mode-line display of prefix-arg-description
      ))

  (deftest test-mode-line-cu-cu
    (testing "C-u C-u shows 'C-u C-u' in mode line"
      ;; Future test
      ))

  (deftest test-mode-line-cu-5
    (testing "C-u 5 shows 'C-u 5' in mode line"
      ;; Future test
      )))
