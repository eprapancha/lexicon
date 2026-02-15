(ns lexicon.ui.minibuffer.icomplete-test
  "E2E tests for icomplete-mode and fido-mode features.

  Emacs source: lisp/icomplete.el

  icomplete-mode shows completion candidates inline in the minibuffer
  as you type. fido-mode is an enhanced icomplete with flex matching.

  Display format:
  - {candidate1 | candidate2 | ...} - multiple candidates
  - (single) - single match, required
  - [single] - single match, optional"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; icomplete-mode Activation
;; =============================================================================

(deftest test-icomplete-mode-activation
  (testing "icomplete-mode can be toggled on and off"
    (h/setup-test*)

    ;; Check initial icomplete state
    (let [initial-state (h/get-icomplete-state)]
      (println "DEBUG: Initial icomplete state:" initial-state)
      (is (or (nil? initial-state)
              (false? (:enabled initial-state)))
          (str "Initial icomplete state should be disabled, got: " initial-state)))

    ;; Enable icomplete-mode via M-x
    (println "DEBUG: About to run icomplete-mode")
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)
    (println "DEBUG: After running icomplete-mode")

    ;; Check icomplete state after first toggle
    (let [state-after-enable (h/get-icomplete-state)]
      (is (true? (:enabled state-after-enable))
          (str "After first toggle, icomplete should be enabled, got: " state-after-enable)))

    ;; Verify the message
    (let [echo-text (h/get-echo-area-text)]
      (is (str/includes? echo-text "Icomplete mode enabled")
          (str "Should show 'Icomplete mode enabled' message, got: " echo-text)))

    ;; Disable icomplete-mode
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)

    ;; Check icomplete state after second toggle
    (let [state-after-disable (h/get-icomplete-state)]
      (is (false? (:enabled state-after-disable))
          (str "After second toggle, icomplete should be disabled, got: " state-after-disable)))

    (let [echo-text (h/get-echo-area-text)]
      (is (str/includes? echo-text "Icomplete mode disabled")
          (str "Should show 'Icomplete mode disabled' message, got: " echo-text)))))

;; =============================================================================
;; icomplete Candidate Display
;; =============================================================================

(deftest test-icomplete-candidate-display
  (testing "icomplete shows completions inline in minibuffer as you type"
    (h/setup-test*)

    ;; Enable icomplete-mode
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)

    ;; Open M-x and type a partial command
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Type partial command that has multiple matches
    (h/type-in-minibuffer "goto")
    (Thread/sleep 300)

    ;; Check for icomplete display (inline candidates)
    (let [icomplete-display (h/get-icomplete-display)]
      (is (not (nil? icomplete-display))
          "icomplete should show inline candidates")
      (when icomplete-display
        (is (or (str/includes? icomplete-display "{")
                (str/includes? icomplete-display "[")
                (str/includes? icomplete-display "("))
            (str "icomplete display should have brackets, got: " icomplete-display))))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable icomplete-mode for cleanup
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 100)))

;; =============================================================================
;; icomplete Cycling (C-. and C-,)
;; =============================================================================

(deftest test-icomplete-cycling
  (testing "C-. and C-, cycle through icomplete candidates"
    (h/setup-test*)

    ;; Enable icomplete-mode
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)

    ;; Open M-x with partial input
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Type partial command that has multiple completions
    (h/type-in-minibuffer "goto")
    (Thread/sleep 200)

    ;; Get initial input value
    (let [initial-input (h/get-minibuffer-input-value)]
      (is (= "goto" initial-input) "Initial input should be 'goto'")

      ;; Press C-. to cycle forward
      (h/press-ctrl ".")
      (Thread/sleep 150)

      (let [after-forward (h/get-minibuffer-input-value)]
        ;; After cycling, input should be a completion (like "goto-char" or "goto-line")
        (is (not= "goto" after-forward)
            (str "C-. should change input to a completion, got: " after-forward))
        ;; The completion should at least contain "goto" (prefix or substring match)
        (is (str/includes? after-forward "goto")
            (str "Cycled completion should contain 'goto', got: " after-forward))

        ;; Press C-, to cycle backward
        (h/press-ctrl ",")
        (Thread/sleep 150)

        (let [after-backward (h/get-minibuffer-input-value)]
          ;; Should have cycled (may be different or same completion)
          ;; The important thing is it's a valid completion containing "goto"
          (is (str/includes? after-backward "goto")
              (str "After C-, should still be a goto completion, got: " after-backward)))))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable icomplete-mode for cleanup
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 100)))

;; =============================================================================
;; fido-mode (Flex Matching)
;; =============================================================================

(deftest test-icomplete-fido-mode
  (testing "fido-mode enables flex matching"
    (h/setup-test*)

    ;; Enable fido-mode via M-x
    (h/run-mx-command "fido-mode")
    (Thread/sleep 150)

    ;; Verify the message
    (let [echo-text (h/get-echo-area-text)]
      (is (str/includes? echo-text "Fido mode enabled")
          (str "Should show 'Fido mode enabled' message, got: " echo-text)))

    ;; Open M-x and type a flex pattern
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible")

    ;; Type flex pattern "gol" which should match "goto-line" via flex matching
    ;; (characters g, o, l appear in order in "goto-line")
    (h/type-in-minibuffer "gol")
    (Thread/sleep 300)

    ;; With fido-mode (flex matching), we should see candidates
    (let [icomplete-display (h/get-icomplete-display)
          minibuffer-content (h/get-minibuffer-full-content)]
      ;; Either icomplete display shows candidates, or full minibuffer content has them
      (is (or (and icomplete-display
                   (or (str/includes? icomplete-display "goto")
                       (str/includes? icomplete-display "{")))
              (str/includes? minibuffer-content "goto"))
          (str "Flex matching should find goto commands. Display: " icomplete-display
               " Content: " minibuffer-content)))

    ;; Cancel
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable fido-mode for cleanup
    (h/run-mx-command "fido-mode")
    (Thread/sleep 100)))
