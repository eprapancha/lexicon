(ns lexicon.ui.editing.prefix-argument-test
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
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; Helper functions (test-specific)
(defn press-minus
  "Press the minus key"
  []
  (e/fill h/*driver* {:css ".hidden-input"} "-")
  (Thread/sleep 50))

(defn get-prefix-arg
  "Get current prefix-arg from window.editorState"
  []
  (e/js-execute h/*driver* "
    const state = window.editorState;
    if (!state) return null;
    return state.prefixArg || null;
  "))

;; =============================================================================
;; Test Suite: Prefix Argument Accumulation
;; =============================================================================

(deftest test-cu-alone
  (testing "C-u alone sets prefix-arg to (4)"
    (h/setup-test*)

    ;; Press C-u
    (h/press-ctrl "u")
    (Thread/sleep 50)

    ;; Check prefix-arg is (4) - represented as a list/array in JS
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 4 (first prefix-arg)) "prefix-arg should be (4)"))))

(deftest test-cu-cu
  (testing "C-u C-u sets prefix-arg to (16)"
    (h/setup-test*)

    ;; Press C-u C-u
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-ctrl "u")
    (Thread/sleep 50)

    ;; Check prefix-arg is (16)
    (let [prefix-arg (get-prefix-arg)]
      (is (sequential? prefix-arg) "prefix-arg should be a sequence")
      (is (= 16 (first prefix-arg)) "prefix-arg should be (16)"))))

(deftest test-cu-digit
  (testing "C-u 5 sets prefix-arg to 5"
    (h/setup-test*)

    ;; Press C-u 5
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)

    ;; Check prefix-arg is 5 (number)
    (let [prefix-arg (get-prefix-arg)]
      (is (number? prefix-arg) "prefix-arg should be a number")
      (is (= 5 prefix-arg) "prefix-arg should be 5"))))

(deftest test-cu-multi-digit
  (testing "C-u 5 2 sets prefix-arg to 52"
    (h/setup-test*)

    ;; Press C-u 5 2
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)
    (h/press-key "2")
    (Thread/sleep 50)

    ;; Check prefix-arg is 52
    (let [prefix-arg (get-prefix-arg)]
      (is (= 52 prefix-arg) "prefix-arg should be 52"))))

(deftest test-cu-minus
  (testing "C-u - sets prefix-arg to '- (symbol)"
    (h/setup-test*)

    ;; Press C-u -
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (press-minus)

    ;; Check prefix-arg is '- (symbol, represented as string in ClojureScript)
    (let [prefix-arg (get-prefix-arg)]
      (is (or (= prefix-arg "-") (= prefix-arg '-))
          "prefix-arg should be '- symbol"))))

(deftest test-cu-minus-digit
  (testing "C-u - 5 sets prefix-arg to -5"
    (h/setup-test*)

    ;; Press C-u - 5
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (press-minus)
    (h/press-key "5")
    (Thread/sleep 50)

    ;; Check prefix-arg is -5
    (let [prefix-arg (get-prefix-arg)]
      (is (= -5 prefix-arg) "prefix-arg should be -5"))))

;; =============================================================================
;; Test Suite: Prefix Argument with Commands
;; =============================================================================

(deftest test-cu-forward-char
  (testing "C-u 4 C-f moves forward 4 characters"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Move to beginning
    (h/press-ctrl "a")
    (Thread/sleep 50)

    ;; C-u 4 C-f should move forward 4 chars
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "4")
    (Thread/sleep 50)
    (h/press-ctrl "f")
    (Thread/sleep 100)

    ;; Cursor should be after "Hell" (position 4)
    ;; Insert a marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "HellXo World")
          "Cursor should be at position 4 after C-u 4 C-f"))))

(deftest test-cu-backward-char
  (testing "C-u 5 C-b moves backward 5 characters"
    (h/setup-test*)
    (h/clear-buffer)

    ;; Type some text
    (h/type-text "Hello World")
    (Thread/sleep 100)

    ;; Cursor is at end (position 11)
    ;; C-u 5 C-b should move backward 5 chars to position 6
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (h/press-key "5")
    (Thread/sleep 50)
    (h/press-ctrl "b")
    (Thread/sleep 100)

    ;; Cursor should be at position 6 (before "World")
    ;; Insert a marker to verify position
    (h/type-text "X")
    (Thread/sleep 100)

    (let [text (h/get-buffer-text*)]
      (is (str/includes? text "Hello XWorld")
          "Cursor should be at position 6 after C-u 5 C-b"))))

(deftest test-prefix-arg-cleared-after-command
  (testing "Prefix arg is cleared after command execution"
    (h/setup-test*)

    ;; Set prefix arg
    (h/press-ctrl "u")
    (Thread/sleep 50)
    (let [prefix-arg-before (get-prefix-arg)]
      (is prefix-arg-before "prefix-arg should be set"))

    ;; Execute command
    (h/press-ctrl "f")
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
