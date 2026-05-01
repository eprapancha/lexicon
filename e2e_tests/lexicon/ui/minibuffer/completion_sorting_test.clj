(ns lexicon.ui.minibuffer.completion-sorting-test
  "E2E tests for completion candidate sorting in the minibuffer.

  Verifies that display-sort-function sorts completion candidates
  alphabetically when shown via icomplete-mode.

  Display format:
  - {candidate1 | candidate2 | ...} - multiple candidates

  Emacs source: lisp/minibuffer.el, lisp/icomplete.el"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [etaoin.api :as e]
            [lexicon.test-helpers :as h]))

(use-fixtures :once h/with-driver)

;; =============================================================================
;; Helper: Parse icomplete candidates from display string
;; =============================================================================

(defn- parse-icomplete-candidates
  "Parse candidates from icomplete display format like '{cmd1 | cmd2 | cmd3}'.
  Returns a vector of candidate strings, or nil if parsing fails."
  [display-text]
  (when (and display-text
             (str/includes? display-text "{")
             (str/includes? display-text "}"))
    (let [;; Extract content between { and }
          inner (-> display-text
                    (str/replace #".*\{" "")
                    (str/replace #"\}.*" "")
                    str/trim)]
      (when (not (str/blank? inner))
        (mapv str/trim (str/split inner #"\s*\|\s*"))))))

;; =============================================================================
;; Completion Sorting
;; =============================================================================

(deftest test-completion-candidates-sorted-alphabetically
  (testing "M-x completion candidates appear in alphabetical order"
    (h/setup-test*)

    ;; Enable icomplete-mode so candidates display inline
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)

    ;; Verify icomplete-mode is enabled
    (let [state (h/get-icomplete-state)]
      (is (true? (:enabled state))
          (str "icomplete-mode should be enabled, got: " state)))

    ;; Open M-x and type a prefix that matches multiple commands
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible after M-x")

    ;; Type "goto" which should match goto-char and goto-line (at minimum)
    (h/type-in-minibuffer "goto")
    (Thread/sleep 300)

    ;; Get the icomplete display and parse candidates
    (let [icomplete-display (h/get-icomplete-display)]
      (is (not (nil? icomplete-display))
          "icomplete should show inline candidates for 'goto' prefix")

      (when icomplete-display
        (let [candidates (parse-icomplete-candidates icomplete-display)]
          (is (and (some? candidates) (>= (count candidates) 2))
              (str "Should have at least 2 candidates for 'goto', got: " candidates))

          (when (and candidates (>= (count candidates) 2))
            ;; Verify candidates are in case-insensitive alphabetical order
            ;; (matching Emacs display-sort-function :alphabetical behavior)
            (let [sorted? (every? (fn [[a b]]
                                    (<= (compare (str/lower-case a) (str/lower-case b)) 0))
                                  (partition 2 1 candidates))]
              (is sorted?
                  (str "Candidates should be in case-insensitive alphabetical order.\n"
                       "  Actual: " (pr-str candidates))))))))

    ;; Cancel minibuffer
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable icomplete-mode for cleanup
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 100)))

(deftest test-completion-sorting-with-different-prefix
  (testing "Sorting holds for different command prefixes"
    (h/setup-test*)

    ;; Enable icomplete-mode
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 150)

    ;; Open M-x and type a different prefix
    (h/press-meta "x")
    (Thread/sleep 200)

    (is (h/minibuffer-visible?) "Minibuffer should be visible after M-x")

    ;; Type "buffer" which should match multiple buffer-related commands
    (h/type-in-minibuffer "buffer")
    (Thread/sleep 300)

    ;; Get the icomplete display and parse candidates
    (let [icomplete-display (h/get-icomplete-display)]
      (is (not (nil? icomplete-display))
          "icomplete should show inline candidates for 'buffer' prefix")

      (when icomplete-display
        (let [candidates (parse-icomplete-candidates icomplete-display)]
          (is (and (some? candidates) (>= (count candidates) 2))
              (str "Should have at least 2 candidates for 'buffer', got: " candidates))

          (when (and candidates (>= (count candidates) 2))
            ;; Verify candidates are in case-insensitive alphabetical order
            (let [sorted? (every? (fn [[a b]]
                                    (<= (compare (str/lower-case a) (str/lower-case b)) 0))
                                  (partition 2 1 candidates))]
              (is sorted?
                  (str "Candidates should be in case-insensitive alphabetical order.\n"
                       "  Actual: " (pr-str candidates))))))))

    ;; Cancel minibuffer
    (h/press-ctrl "g")
    (Thread/sleep 100)

    ;; Disable icomplete-mode for cleanup
    (h/run-mx-command "icomplete-mode")
    (Thread/sleep 100)))
